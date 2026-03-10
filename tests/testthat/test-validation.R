test_that("validate_dpcr_data reports missing required columns", {
  df_missing_rfu <- tibble::tibble(
    plate_name = "Plate_1",
    plate_id = "id-1",
    plate_type = "Nanoplate",
    well = "A1",
    sample = "Sample_X",
    channel = "C",
    color_channel = "green",
    volume = 24,
    threshold = 20,
    partition = 1,
    invalid_partition = FALSE,
    positive_control = FALSE,
    reference = "Std-Ref",
    device_type = "qiaquity",
    source_file = "x.csv"
  )

  res <- validate_dpcr_data(df_missing_rfu)

  expect_false(res$ok)
  expect_true(any(res$issues$field == "rfu" & res$issues$severity == "error"))
})

test_that("validate_dpcr_data marks negative RFU as warning", {
  df <- tibble::add_row(
    new_empty_dpcr_data(),
    plate_name = "Plate_1",
    plate_id = "id-1",
    plate_type = "Nanoplate",
    well = "A1",
    sample = "Sample_X",
    channel = "C",
    color_channel = "green",
    volume = 24,
    threshold = 20,
    partition = 1,
    rfu = -1,
    invalid_partition = FALSE,
    positive_control = TRUE,
    reference = "Std-Ref",
    device_type = factor("qiaquity", levels = DEVICE_LEVELS),
    source_file = "x.csv"
  )

  res <- validate_dpcr_data(df)

  expect_true(any(res$issues$field == "rfu" & res$issues$severity == "warning"))
})
