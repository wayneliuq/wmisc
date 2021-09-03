library(wmisc)

test_that("figure exports", {
  ## create and export a test figure
  x <- hist(rnorm(100))

  exportFig(x,
            filename = "histogram",
            prefix = NULL,
            dir = "export",
            heightpx = 800,
            widthpx = 1200)

  expected_1_figure <- c(fs::path_wd("export/histogram.png"),
                         fs::path_wd("export/histogram.pdf"))

  expect_equal(as.logical(fs::file_exists(expected_1_figure)), c(T,T))

  fs::dir_delete(fs::path_wd("export"))
})
