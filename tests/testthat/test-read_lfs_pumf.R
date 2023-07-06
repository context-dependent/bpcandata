test_that("read_lfs_pumf returns records and a codebook", {
  dir <- fs::file_temp()
  withr::defer({
    fs::dir_delete(dir)
  })
  fetch_lfs_pumf(2020, dir)

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
})
