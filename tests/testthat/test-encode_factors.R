test_that("multiplication works", {
  dir <- fs::file_temp()
  withr::defer({
    fs::dir_delete(dir)
  })
  fetch_lfs_pumf(2020, dir)

  d <- read_lfs_pumf(glue::glue("{dir}/2020"))
  d2 <- encode_lfs_factors(d$records, d$codebook)

  factors <- d$codebook |>
    dplyr::filter(is_factor) |>
    dplyr::pull(var_name)

  expect_true(all(purrr::map_lgl(d2[, factors], is.factor)))
})
