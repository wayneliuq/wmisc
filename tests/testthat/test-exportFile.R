library(wmisc)
library(readr)
library(writexl)

test_that("1. matrix exports successfully", {
  ## export a matrix
  set.seed(20210903); x <- matrix(rnorm(500), nrow = 50)
  x[c(9, 48, 87)] <- NA

  expected_1_matrix <- fs::path_wd(c(
    csv = "export/matrix.csv",
    tsv = "export/matrix.txt",
    xlsx = "export/matrix.xlsx"
  ))

  exportFile(x, filename = "matrix", dir = "export", prefix = "none")

  ## export a data.frame
  set.seed(20210903); y <- data.frame(
    rowID = paste0("ID", sprintf("%03d", c(1:100))),
    matrix(rnorm(300), nrow = 100),
    char = paste0(sample(LETTERS, 100, T), sample(LETTERS, 100, T), sample(letters, 100, T), sample(letters, 100, T)),
    logi = sample(c(T, F, NA), 100, T)
  )

  expected_1_df <- fs::path_wd(c(
    csv = "export/df.csv",
    tsv = "export/df.txt",
    xlsx = "export/df.xlsx"
  ))

  exportFile(y, filename = "df", dir = "export", prefix = "none")

  ## exported as different formats
  expect_equal(as.logical(fs::file_exists(expected_1_matrix)), c(T, T, T))
  expect_equal(as.logical(fs::file_exists(expected_1_df)), c(T, T, T))

  fs::dir_delete(fs::path_wd("/export"))
})
