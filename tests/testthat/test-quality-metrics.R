test_that("summarize_quality_qc computes accepted partitions, rain and lambda", {
  df <- tibble::tibble(
    plate_name = "P1",
    plate_id = "id-1",
    plate_type = "Nano",
    well = "A1",
    sample = "S1",
    channel = "C",
    color_channel = "green",
    volume = 24,
    threshold = 100,
    partition = 1:4,
    rfu = c(50, 105, 130, 95),
    invalid_partition = c(FALSE, FALSE, TRUE, FALSE),
    positive_control = c(FALSE, TRUE, TRUE, FALSE),
    reference = "Std-Ref",
    device_type = factor("qiaquity", levels = DEVICE_LEVELS),
    source_file = "x.csv"
  )

  res <- summarize_quality_qc(
    df,
    conf_level = 0.95,
    rain_band_fraction = 0.10,
    min_accepted = 3,
    rain_limit_fraction = 0.025
  )

  expect_equal(nrow(res), 1)
  expect_equal(res$n_total[[1]], 4)
  expect_equal(res$n_accepted[[1]], 3)
  expect_equal(res$n_positive[[1]], 1)
  expect_equal(round(res$positive_percent[[1]], 2), round(100 / 3, 2))
  expect_true(is.finite(res$lambda[[1]]))
  expect_true(res$rain_percent[[1]] >= 0)
})
