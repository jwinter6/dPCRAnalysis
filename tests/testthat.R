library(testthat)

source_files <- sort(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
invisible(lapply(source_files, source))

test_dir("tests/testthat", reporter = "summary")
