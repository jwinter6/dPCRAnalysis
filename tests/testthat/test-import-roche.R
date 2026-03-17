roche_fixture_paths <- function() {
  candidate_dirs <- c(
    "Example_Data",
    "exampledata",
    file.path("..", "Example_Data"),
    file.path("..", "exampledata"),
    file.path("..", "..", "Example_Data"),
    file.path("..", "..", "exampledata")
  )
  existing_dirs <- candidate_dirs[dir.exists(candidate_dirs)]

  if (length(existing_dirs) == 0) {
    return(character())
  }

  fixture_dir <- existing_dirs[[1]]

  file.path(
    fixture_dir,
    c(
      "plate1-2502180201270663301615_lane5_NTC.csv",
      "plate1-2502180201270663301615_lane6_E13.csv",
      "plate1-2502180201270663301615_lane7_E14.csv",
      "plate1-2502180201270663301615_lane8_NEG.csv"
    )
  )
}

test_that("detect_device_type recognises Roche Digital LightCycler fixtures", {
  fixtures <- roche_fixture_paths()

  expect_true(all(file.exists(fixtures)))
  detected <- vapply(fixtures, detect_device_type, character(1))

  expect_true(all(detected == "roche"))
})

test_that("parse_roche_filename_metadata extracts plate and sample from filename", {
  meta <- parse_roche_filename_metadata("PLATE123_runinfo_ABC123.csv")

  expect_equal(meta$plate_name, "PLATE123")
  expect_equal(meta$plate_id, "PLATE123")
  expect_equal(meta$sample, "ABC123")
})

test_that("parse_roche_filename_metadata handles case-insensitive extensions and multiple underscores", {
  meta <- parse_roche_filename_metadata("PLATE_2024_07_batch_999_SAMPLE-01.CSV")

  expect_equal(meta$plate_name, "PLATE")
  expect_equal(meta$sample, "SAMPLE-01")
})

test_that("parse_roche_filename_metadata rejects invalid Roche filenames", {
  expect_error(
    parse_roche_filename_metadata("PLATE123.csv"),
    "mindestens ein Unterstrich"
  )

  expect_error(
    parse_roche_filename_metadata("PLATE123_runinfo_.csv"),
    "Sample-Name .* leer"
  )
})

test_that("import_roche_csv pivots active channel columns into the internal schema", {
  path <- roche_fixture_paths()[[2]]
  raw <- read_roche_csv_raw(path)
  active_channels <- find_active_roche_channel_columns(raw)

  out <- import_roche_csv(path, file_name = "plate1-2502180201270663301615_lane6_E13.csv")

  expect_s3_class(out, "tbl_df")
  expect_true(all(DPCR_STANDARD_COLUMNS %in% names(out)))
  expect_equal(active_channels, c("Channel2", "Channel3", "Channel7"))
  expect_equal(nrow(out), nrow(raw) * length(active_channels))
  expect_equal(sort(unique(out$channel)), active_channels)
  expect_equal(unique(out$plate_name), "plate1-2502180201270663301615")
  expect_equal(unique(out$plate_id), "plate1-2502180201270663301615")
  expect_equal(unique(out$sample), "E13")
  expect_equal(unique(out$well), "lane6")
  expect_equal(unique(as.character(out$device_type)), "roche")
  expect_equal(unique(out$source_file), "plate1-2502180201270663301615_lane6_E13.csv")

  p1 <- dplyr::filter(out, partition == 1)
  expect_equal(p1$rfu[match("Channel2", p1$channel)], 693)
  expect_equal(p1$rfu[match("Channel3", p1$channel)], 1818)
  expect_equal(p1$rfu[match("Channel7", p1$channel)], 2196)
  expect_false(any(p1$invalid_partition))
})

test_that("import_roche_csv maps Roche flags to invalid_partition and reference", {
  path <- roche_fixture_paths()[[1]]
  out <- import_roche_csv(path, file_name = "plate1-2502180201270663301615_lane5_NTC.csv")

  flagged <- dplyr::filter(out, partition == 3)

  expect_true(all(flagged$invalid_partition))
  expect_equal(unique(flagged$reference), "InvalidL3")
  expect_equal(unique(flagged$sample), "NTC")
})

test_that("validate_dpcr_data tolerates Roche-specific missing optional fields", {
  path <- roche_fixture_paths()[[4]]
  out <- import_roche_csv(path, file_name = "plate1-2502180201270663301615_lane8_NEG.csv")
  res <- validate_dpcr_data(out)

  expect_true(res$ok)
  expect_false(any(res$issues$field %in% c("volume", "threshold", "positive_control")))
})

test_that("import_roche_csv derives metadata from original uploaded filename instead of temp path", {
  tmp <- tempfile(fileext = ".csv")
  file.copy(roche_fixture_paths()[[2]], tmp, overwrite = TRUE)

  out <- import_roche_csv(tmp, file_name = "PLATE123_runinfo_ABC123.csv")

  expect_equal(unique(out$plate_name), "PLATE123")
  expect_equal(unique(out$sample), "ABC123")
  expect_equal(unique(out$source_file), "PLATE123_runinfo_ABC123.csv")
})

test_that("detect_device_type still recognises QIAcuity layouts", {
  tmp <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "sep=,",
      "Plate name,Plate ID,Plate type,Well,Sample,Channel,Cycled volume,Threshold,Partition,Is invalid,Is positive,RFU,REF",
      "Plate_A,plate-001,Nanoplate 26K 8-well,A1,Sample_1,C,24.066,19.38,1,0,1,105.4,Std-Ref"
    ),
    con = tmp,
    useBytes = TRUE
  )

  expect_equal(detect_device_type(tmp), "qiaquity")
})
