test_that("lfs tools behave correctly", {
  dir <- fs::file_temp()
  withr::defer({
    fs::dir_delete(dir)
  })
  fetch_lfs_pumf(2020, dir)

  expect_true(fs::dir_exists(glue::glue("{dir}/2020")))
  expect_true(fs::file_exists(glue::glue("{dir}/2020/pub0120.csv")))
  expect_true(
    fs::file_exists(
      glue::glue("{dir}/2020/LFS_PUMF_EPA_FGMD_recordlayout.csv")
    )
  )

  d <- read_lfs_pumf(glue::glue("{dir}/2020"))

  expect_type(d, "list")
  expect_s3_class(d$records, "data.frame")
  expect_s3_class(d$codebook, "data.frame")
  expect_equal(
    colnames(d$codebook),
    c(
      "field_id",
      "var_name",
      "var_label",
      "is_factor",
      "factor_labels"
    )
  )

  d2 <- encode_lfs_factors(d$records, d$codebook)

  factors <- d$codebook |>
    dplyr::filter(is_factor) |>
    dplyr::pull(var_name)

  expect_true(all(purrr::map_lgl(d2[, factors], is.factor)))

  cb_roxy <- roxygenize_lfs_codebook(d$records, d$codebook, 2020)

  message(cb_roxy)
})
