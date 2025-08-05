#' Download LFS PUMF data from StatCan website
#' @param years A vector of years to download
#' @param cache_dir The directory to cache the downloaded files
#' @param refresh_cache
#'  Whether to force download even if the files are already cached
#' @return Invisible
#' @export
fetch_lfs_pumf <- function(
    years, cache_dir = here::here(),
    refresh_cache = FALSE,
    keep_zip = FALSE,
    extract = TRUE) {
    years |>
        purrr::walk(
            .fetch_single_year,
            cache_dir = cache_dir,
            refresh_cache = refresh_cache,
            extract = extract,
            keep_zip = keep_zip
        )
}

#' @rdname fetch_lfs_pumf
.fetch_single_year <- function(
    year,
    cache_dir,
    refresh_cache = FALSE,
    extract = TRUE,
    keep_zip = FALSE) {
    zip_path <- glue::glue("{cache_dir}/{year}-CSV.zip")
    csv_dir <- glue::glue("{cache_dir}/{year}")

    if (
        !refresh_cache &&
            (
                fs::dir_exists(glue::glue("{cache_dir}/{year}")) ||
                    fs::file_exists(zip_path)
            )
    ) {
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

    curl::curl_fetch_disk(u, zip_path)

    if (!fs::dir_exists(csv_dir) && extract) {
        fs::dir_create(csv_dir)
        unzip(zip_path, exdir = csv_dir)
    }

    if (!keep_zip) {
        fs::file_delete(zip_path)
    }
    invisible()
}

#' Read LFS PUMF
#' @param path The directory or zip archive containing the LFS PUMF (e.g. data/raw/2020)
#' @return A tbl_df of LFS PUMF records in `dir` labelled with the codebook in `dir`
#' @export
read_lfs_pumf <- function(path) {
    with(
        list(
            records = read_lfs_records(path),
            codebook = suppressWarnings(read_lfs_codebook(path))
        ),
        encode_lfs_factors(records, codebook)
    )
}

#' @rdname read_lfs_pumf
#' @export
read_lfs_records <- function(path) {
    .find <- zip_dir_ls(path)

    .find(path, glob = "*pub*.csv", recurse = FALSE) |>
        vroom::vroom(
            delim = ",",
            progress = FALSE,
            show_col_types = FALSE,
            col_types = vroom::cols(
                .default = vroom::col_character(),
                REC_NUM = vroom::col_integer(),
                SURVYEAR = vroom::col_integer(),
                UHRSMAIN = vroom::col_double(),
                AHRSMAIN = vroom::col_double(),
                UTOTHRS = vroom::col_double(),
                ATOTHRS = vroom::col_double(),
                HRSAWAY = vroom::col_double(),
                XTRAHRS = vroom::col_double(),
                HRLYEARN = vroom::col_double(),
                DURJLESS = vroom::col_integer(),
                TENURE = vroom::col_integer(),
                PAIDOT = vroom::col_double(),
                UNPAIDOT = vroom::col_double()
            )
        ) |>
        .correct_decimal_placement()
}

#' @rdname read_lfs_pumf
#' @importFrom dplyr mutate across case_when filter group_by ungroup select
.correct_decimal_placement <- function(x) {
    x |>
        dplyr::mutate(
            dplyr::across(matches("HRS|PAIDOT"), ~ .x / 10),
            dplyr::across(matches("EARN"), ~ .x / 100)
        )
}

#' @rdname read_lfs_pumf
#' @export
read_lfs_codebook <- function(path) {
    .find <- zip_dir_ls(path)

    .find(
        path,
        glob = "**/LFS_PUMF_EPA_FGMD_codebook.csv",
        recurse = TRUE
    ) |>
        vroom::vroom(
            delim = ",",
            show_col_types = FALSE,
            col_select = c(
                var = Variable_Variable,
                field_id = Field_Champ,
                label = EnglishLabel_EtiquetteAnglais,
                universe = EnglishUniverse_UniversAnglais
            )
        ) |>
        dplyr::filter(
            !is.na(label),
            label != "Not applicable",
        ) |>
        dplyr::mutate(
            var_name = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                .default = toupper(var)
            ),
            var_label = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                .default = stringi::stri_enc_toutf8(label)
            ),
            var_universe = dplyr::case_when(
                is.na(field_id) ~ NA_character_,
                .default = stringi::stri_enc_toutf8(universe)
            ),
            factor_level = dplyr::case_when(
                !is.na(field_id) ~ NA_character_,
                var |> stringr::str_detect("^\\d+$") ~ var,
                .default = NA_character_
            ),
            factor_label = dplyr::case_when(
                !is.na(field_id) ~ NA_character_,
                .default = label
            )
        ) |>
        tidyr::fill(field_id, var_name, var_label, var_universe) |>
        dplyr::summarize(
            factor_ncat = dplyr::n_distinct(na.omit(factor_level)),
            factor_levels = list(c(na.omit(factor_level))),
            factor_labels = list(c(na.omit(factor_label))),
            value_labels = list(
                c(na.omit(factor_level)) |>
                    purrr::set_names(c(na.omit(factor_label)))
            ),
            is_factor = factor_ncat > 0,
            .by = c(var_name, var_label, var_universe)
        ) |>
        dplyr::mutate(
            value_labels = value_labels |>
                purrr::set_names(var_name)
        )
}

#' Produce a data frame (tibble) with labelled vectors encoded as factors
#' @rdname read_lfs_pumf
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

    codebook |>
        purrr::pwalk(\(
            is_factor,
            factor_levels,
            factor_labels,
            var_name,
            var_label, ...
        ) {
            if (is_factor) {
                d[[var_name]] <<- factor(
                    d[[var_name]],
                    levels = factor_levels,
                    labels = factor_labels
                )
            }

            labelled::var_label(d[[var_name]]) <<- var_label
        })

    d
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
        r"(The data set has <<nrow(records)>> rows and <<ncol(records)>> columns. \itemize{)",
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
        r"(\item{\code{<<col$var_name>>} \code{<num>} <<col$var_label>> [<<col_range[1]>>, <<col_range[2]>>] \code{for} <<col$var_universe>>})",
        .open = "<<", .close = ">>"
    )
}

#' Generate roxygen documentation for a factor column of the data set
#' @rdname roxygenize_lfs_codebook
.cb_column__factor <- function(col) {
    head <- glue::glue(
        "#'   ",
        r"(\item{\code{<<col$var_name>>} \code{<fct>} <<col$var_label>> \code{for} <<col$var_universe>>}{)",
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
        r"(#'     \itemize{)",
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

#' @title Generate bootstrap weights for LFS PUMF data
#' @rdname generate_bootstrap_weights
#' @param d :data.frame (labelled) LFS PUMF data.
#' @param n_reps :integer number of bootstrap replicates to generate.
#' @return a (n_reps by nrow(d)) array of bootstrap weights.
#' @export
generate_lfs_bootstrap_weights <- function(d, n_reps) {
    uncalibrated_weights <- .generate_replicates(d$FINALWT, n_reps)
    age_tabs <- .calculate_age_tabs(d$AGE_6, d$AGE_12)
    domains <- interaction(d$SURVYEAR, d$SURVMNTH, d$PROV, d$SEX, age_tabs)

    .calibrate_weights(uncalibrated_weights, d$FINALWT, domains)
}


#' @title Sample poisson factors for simulated bootstrap weights
#' @rdname generate_bootstrap_weights
.sample_poisson_factors <- function(k) {
    sample(c(-1, 1), k, replace = TRUE)
}

#' @title initialize uncalibrated bootstrap replicate weights
#' @rdname generate_bootstrap_weights
.generate_replicates <- function(final_weight, n_reps) {
    k <- length(final_weight)
    adjustment_factors <- final_weight * sqrt((final_weight - 1) / final_weight)

    replicate(
        n_reps,
        final_weight + .sample_poisson_factors(k) * adjustment_factors,
        simplify = "array"
    )
}


#' @title Calibrate uncalibrated bootstrap replicate weights
#' @rdname generate_bootstrap_weights
.calibrate_weights <- function(uncalibrated_weights, final_weight, domains) {
    domain_indices <- split(seq_len(nrow(uncalibrated_weights)), domains)
    domain_fw_totals <- domain_indices |>
        sapply(function(x) sum(final_weight[x]))
    domain_bs_totals <- domain_indices |>
        sapply(function(x) colSums(uncalibrated_weights[x, ]))
    domain_scaling_factors <- domain_fw_totals / t(domain_bs_totals)

    unname(uncalibrated_weights * domain_scaling_factors[domains, ])
}

#' @title Calculate AGE_TABS for LFS PUMF data
#' @rdname generate_bootstrap_weights
#' @param age_6 :factor AGE_6 variable.
#' @param age_12 :factor AGE_12 variable.
#' @return a factor AGE_TABS variable.
#' @export
.calculate_age_tabs <- function(age_6, age_12) {
    age_tabs_base <- dplyr::case_when(
        age_6 %in% c("15 to 16 years", "17 to 19 years") ~ age_6,
        TRUE ~ age_12
    )
    forcats::fct_collapse(
        age_tabs_base,
        "35 to 44 years" = c("35 to 39 years", "40 to 44 years"),
        "45 to 54 years" = c("45 to 49 years", "50 to 54 years")
    )
}

#' @title quality measures for bootstrap estimates
#' @rdname bs_stat
.bs_out <- function(est_fw, est_bs) {
    bs_var <- mean((est_bs - est_fw)^2)
    bs_sd <- sqrt(bs_var)

    data.frame(
        est = est_fw,
        var = bs_var,
        se = bs_sd
    )
}

#' @title survey mean with bootstrap sampling variance and standard error
#' @description
#'     designed for use in a grouped dplyr::summarize() context
#'     where bootstrap weights are stored as an array column
#' @rdname bs_stat
#' @param x :number vector of survey data.
#' @param bootstrap_weights :array of bootstrap replicate weights.
#' @param final_weights :vector of final weights.
#' @return a data.frame with columns est, var, and se.
#' @export
lfs_bs_mean <- function(x, bootstrap_weights, final_weights) {
    est_fw <- sum(final_weights * x) / sum(final_weights)
    est_bs <- colSums(bootstrap_weights * x) / colSums(bootstrap_weights)
    .bs_out(est_fw, est_bs)
}

#' @title survey total with bootstrap sampling variance and standard error
#' @description
#'     designed for use in a grouped dplyr::summarize() context
#'     where bootstrap weights are stored as an array column
#' @rdname bs_stat
#' @param bootstrap_weights :array of bootstrap replicate weights.
#' @param final_weights :vector of final weights.
#' @return a data.frame with columns est, var, and se.
#' @export
lfs_bs_total <- function(bootstrap_weights, final_weights) {
    est_fw <- sum(final_weights)
    est_bs <- colSums(bootstrap_weights)
    .bs_out(est_fw, est_bs)
}
