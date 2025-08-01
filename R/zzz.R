zip_ls <- function(archive, ..., recurse = FALSE) {
    unzip(archive, list = TRUE)$Name |>
        fs::path_filter(...) |>
        purrr::map(\(.x) {
            unz(archive, .x)
        })
}

zip_dir_ls <- function(path) {
    if (fs::is_dir(path)) {
        fs::dir_ls
    } else if (fs::is_file(path) && stringr::str_ends(path, ".zip")) {
        zip_ls
    } else {
        stop("path must be a directory or a zip file")
    }
}
