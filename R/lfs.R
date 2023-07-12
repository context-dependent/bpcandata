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
        return(invisible())
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
#' @export
read_lfs_pumf <- function(dir) {
    records <- .read_lfs_records(dir)
    codebook <- suppressWarnings(.read_lfs_codebook(dir))
    list(records = records, codebook = codebook)
}

#' @rdname read_lfs_pumf
.read_lfs_records <- function(dir) {
    csv_paths <- fs::dir_ls(dir, regexp = "pub.*csv")

    csv_paths |>
        purrr::map(data.table::fread) |>
        data.table::rbindlist() |>
        as.data.frame() |>
        tibble::as_tibble() |>
        .correct_decimal_placement()
}

#' @rdname read_lfs_pumf
#' @importFrom dplyr mutate across case_when filter group_by ungroup select
.correct_decimal_placement <- function(x) {
    x |>
        mutate(
            across(matches("HRS"), ~ .x / 10),
            across(matches("EARN"), ~ .x / 100)
        )
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

#' Produce data documentation in standard Roxygen markdown
#' @rdname roxygenize_lfs_codebook
#' @param records A data frame (tibble) of records
#' @param codebook A data frame (tibble) of codebook
#' @param year The year of the data
#' @param description A description of the data
#' @return A character vector of Roxygen markdown
#' @export
roxygenize_lfs_codebook <- function(records, codebook, year) {
    stringr::str_c(
        .cb_top(year),
        .cb_format(records, codebook),
        sep = "\n",
        collapse = "\n"
    )
}

#' Produce the top of data documentation
#' @rdname roxygenize_lfs_codebook
.cb_top <- function(year) {
    .cb_gen(
        title = glue::glue("Labour Force Survey Public Use Microdata File ({year})"),
        rdname = glue::glue("lfs_pumf_{year}"),
        description = .cb_describe(year),
        source = .cb_source(year)
    )
}

#' Generate roxygen documentation from key-value pairs
#' @rdname roxygenize_lfs_codebook
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> key-value pairs
.cb_gen <- function(...) {
    items <- rlang::list2(...)
    items |>
        purrr::imap_chr(~ {
            glue::glue("#' @{.y} {.x}")
        }) |>
        stringr::str_c(collapse = "\n")
}

#' Generate roxygen documentation for the format of a data set
#' @rdname roxygenize_lfs_codebook
.cb_format <- function(records, codebook) {
    head <- glue::glue(
        r"(The data set has <<nrow(records)>> rows and <<ncol(records)>> columns. \describe{)",
        .open = "<<", .close = ">>"
    )

    data_dictionary <- codebook |>
        as.list() |>
        purrr::transpose() |>
        purrr::map(.cb_column, records) |>
        stringr::str_c(collapse = "\n")

    .cb_gen(
        format = glue::glue(
            "<<head>>",
            "<<data_dictionary>>",
            r"(#' })",
            .open = "<<", .close = ">>",
            .sep = "\n"
        )
    )
}


#' Generate roxygen documentation for a column of the data set
#' @rdname roxygenize_lfs_codebook
#' @param col a column from the codebook, as a named list with:
#' - `field_id`: the field ID
#' - `var_name`: the variable name
#' - `var_label`: the variable label
#' - `is_factor`: whether the variable is a factor
#' - `factor_labels`: if `is_factor`, a data frame (tibble) of factor labels
.cb_column <- function(col, records) {
    if (col$is_factor) {
        .cb_column__factor(col)
    } else {
        .cb_column__number(col, records)
    }
}

#' Generate roxygen documentation for a numeric column of the data set
#' @rdname roxygenize_lfs_codebook
.cb_column__number <- function(col, records) {
    col_range <- records[[col$var_name]] |> range(na.rm = TRUE)
    glue::glue(
        "#'   ",
        r"(\item{\code{<<col$var_name>>} \code{<num>} <<col$var_label>> [<<col_range[1]>>, <<col_range[2]>>]})",
        .open = "<<", .close = ">>"
    )
}

#' Generate roxygen documentation for a factor column of the data set
#' @rdname roxygenize_lfs_codebook
.cb_column__factor <- function(col) {
    head <- glue::glue(
        "#'   ",
        r"(\item{\code{<<col$var_name>>} \code{<fct>} <<col$var_label>>)",
        .open = "<<", .close = ">>"
    )

    levels <- col$factor_labels

    colnames(levels) <- c("factor_level", "factor_label")

    body <- levels |>
        dplyr::filter(!is.na(factor_level), !is.na(factor_label)) |>
        dplyr::mutate(factor_level = as.character(factor_level)) |>
        glue::glue_data(
            "#'       ",
            r"(\item{\code{<<factor_level>>} <<factor_label>>})",
            .open = "<<", .close = ">>"
        ) |>
        stringr::str_c(collapse = "\n")

    glue::glue(
        "<<head>>",
        r"(#'     \describe{)",
        "<<body>>",
        r"(#'     }})",
        .open = "<<", .close = ">>",
        .sep = "\n"
    )
}

#' Truncate n-level factors to 7 levels
#' (levels[1, 2, 3], ..., levels[n-2, n-1, n])
#' @rdname roxygenize_lfs_codebook
.cb_truncate_factor <- function(factor_labels) {
    if (nrow(factor_labels) <= 7) {
        factor_labels
    } else {
        dplyr::bind_rows(
            dplyr::slice(factor_labels, 1:3),
            tibble::tibble(
                factor_level = "...",
                factor_label = "..."
            ),
            dplyr::slice(factor_labels, -c(3, 2, 1))
        )
    }
}


#' Generate roxygen documentation for the source of a data set
#' @rdname roxygenize_lfs_codebook
.cb_source <- function(year) {
    glue::glue(
        "Statistics Canada. ({{year}})",
        "Labour Force Survey Public Use Microdata File.",
        r"(\url{https://doi.org/10.25318/71m0001x-eng})",
        .open = "{{", .close = "}}"
    )
}

#' Generate roxygen documentation for the description of a data set
#' @rdname roxygenize_lfs_codebook
.cb_describe <- function(year) {
    glue::glue(
        "This public use microdata file (PUMF) contains",
        "non-aggregated data for a wide variety of variables",
        "collected from the Labour Force Survey (LFS) in {{year}}.",
        "The LFS collects monthly information on",
        "the labour market activities of Canada's working age population.",
        "This product is for users who prefer to do their own analysis",
        "by focusing on specific subgroups in the population",
        "or by cross-classifying variables that are not in",
        "our catalogued products.",
        "For more information about this survey",
        "(questionnaires, definitions, data sources and methods used):",
        r"(\href{https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3701}{Labour Force Survey})",
        .open = "{{", .close = "}}", .sep = "\n#'   "
    )
}
