#' Download LFS PUMF data from StatCan website
#' @param years A vector of years to download
#' @param cache_dir The directory to cache the downloaded files
#' @param refresh_cache
#'  Whether to force download even if the files are already cached
#' @return Invisible
#' @export
fetch_lfs_pumf <- function(
    years, cache_dir = here::here(),
    refresh_cache = FALSE) {
    years |>
        purrr::walk(
            .fetch_single_year,
            cache_dir = cache_dir,
            refresh_cache = refresh_cache
        )
}

#' @rdname fetch_lfs_pumf
.fetch_single_year <- function(
    year,
    cache_dir,
    refresh_cache = FALSE) {
    if (!refresh_cache && fs::dir_exists(glue::glue("{cache_dir}/{year}"))) {
        message(
            glue::glue(
                "Skipping {year} as it is already cached,",
                " set refresh_cache = TRUE to force download."
            )
        )
        invisible()
    }

    u <- glue::glue(
        "https://www150.statcan.gc.ca/",
        "n1/pub/71m0001x/2021001/hist/{year}-CSV.zip"
    )
    zip_path <- glue::glue("{cache_dir}/{year}-CSV.zip")
    csv_dir <- glue::glue("{cache_dir}/{year}")

    if (!fs::dir_exists(csv_dir)) fs::dir_create(csv_dir)

    curl::curl_fetch_disk(u, zip_path)
    unzip(zip_path, exdir = csv_dir)
    fs::file_delete(zip_path)
    invisible()
}

#' Read LFS PUMF
#' @param dir The directory containing the LFS PUMF (e.g. data/raw/2020)
#' @return A list with two elements: records and codebook
read_lfs_pumf <- function(dir) {
    records <- .read_lfs_records(dir)
    codebook <- suppressWarnings(.read_lfs_codebook(dir))
    list(records = records, codebook = codebook)
}

#' @rdname read_lfs_pumf
.read_lfs_records <- function(dir) {
    csv_paths <- fs::dir_ls(dir, regexp = "pub.*csv")
    cat(csv_paths, "\n")

    csv_paths |>
        purrr::map(data.table::fread) |>
        data.table::rbindlist() |>
        as.data.frame() |>
        tibble::as_tibble()
}

#' @rdname read_lfs_pumf
.read_lfs_codebook <- function(dir) {
    path <- glue::glue("{dir}/LFS_PUMF_EPA_FGMD_codebook.csv")

    d <- readr::read_csv(path, show_col_types = FALSE) |>
        janitor::clean_names() |>
        dplyr::transmute(
            var = variable_variable,
            field_id = field_champ,
            label = english_label_etiquette_anglais
        ) |>
        dplyr::filter(
            !is.na(label),
            label != "Not applicable",
        ) |>
        dplyr::mutate(
            var_name = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                TRUE ~ toupper(var)
            ),
            var_label = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                TRUE ~ label
            ),
            factor_level = dplyr::case_when(
                !is.na(field_id) ~ NA_character_,
                TRUE ~ var
            ),
            factor_label = dplyr::case_when(
                !is.na(field_id) ~ NA_character_,
                TRUE ~ label
            )
        ) |>
        tidyr::fill(field_id, var_name, var_label) |>
        dplyr::group_by(var_name) |>
        dplyr::mutate(
            is_factor = sum(!is.na(factor_level)) > 1,
            factor_level = dplyr::case_when(
                is_factor ~ as.integer(factor_level),
                TRUE ~ NA_integer_
            ),
            factor_label = dplyr::case_when(
                is_factor ~ factor_label,
                TRUE ~ NA_character_
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(
            !(is_factor & is.na(factor_level))
        ) |>
        dplyr::select(
            field_id,
            var_name,
            var_label,
            is_factor,
            factor_level,
            factor_label
        )


    var_labels <- d |>
        dplyr::group_nest(field_id, var_name, var_label, is_factor) |>
        dplyr::select(-data)

    factor_labels <- d |>
        dplyr::filter(is_factor) |>
        dplyr::select(-var_label, -is_factor) |>
        dplyr::group_nest(field_id, var_name, .key = "factor_labels") |>
        dplyr::mutate(factor_labels = purrr::map2(var_name, factor_labels, ~ {
            .y |>
                dplyr::mutate(
                    factor_label = forcats::fct_inorder(factor_label)
                ) |>
                setNames(c(glue::glue("{.x}"), glue::glue("{.x}_label")))
        }))

    suppressMessages(dplyr::left_join(var_labels, factor_labels))
}

#' Produce a data frame (tibble) with labelled vectors encoded as factors
#'
#' @param records A data frame (tibble) of records
#' @param codebook A data frame (tibble) of codebook
#' @return A data frame (tibble) with labelled vectors encoded as factors
#' @export
encode_lfs_factors <- function(records, codebook) {
    d <- records |>
        dplyr::mutate(
            NAICS_21_code = NAICS_21,
            NOC_10_code = NOC_10,
            NOC_43_code = NOC_43
        )
    factors <- codebook[codebook$is_factor, ]
    factor_labels <- factors$factor_labels
    d <- suppressMessages(
        purrr::reduce(factor_labels, .init = d, .f = dplyr::left_join)
    )

    d |>
        dplyr::select(-c(factors[["var_name"]])) |>
        dplyr::rename_all(~ stringr::str_remove(.x, "_label$")) |>
        dplyr::select(c(colnames(records), dplyr::matches("_code$")))
}
