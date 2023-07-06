test_that("fetch_lfs_pumf downloads the right files", {
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
})
