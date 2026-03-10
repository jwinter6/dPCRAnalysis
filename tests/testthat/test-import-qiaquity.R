test_that("import_qiaquity_csv maps columns and schema correctly", {
  tmp <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "sep=,",
      "Plate name,Plate ID,Plate type,Well,Sample,Channel,Cycled volume,Threshold,Partition,Is invalid,Is positive,RFU,REF",
      "Plate_A,plate-001,Nanoplate 26K 8-well,A1,Sample_1,C,24.066,19.38,1,0,1,105.4,Std-Ref",
      ",,,A1,Sample_1,C,24.066,19.38,2,1,0,88.1,Std-Ref"
    ),
    con = tmp,
    useBytes = TRUE
  )

  out <- import_qiaquity_csv(tmp)

  expect_s3_class(out, "tbl_df")
  expect_true(all(DPCR_STANDARD_COLUMNS %in% names(out)))
  expect_equal(nrow(out), 2)
  expect_equal(out$plate_name[[2]], "Plate_A")
  expect_equal(as.character(out$device_type[[1]]), "qiaquity")
  expect_equal(out$source_file[[1]], basename(tmp))
})
