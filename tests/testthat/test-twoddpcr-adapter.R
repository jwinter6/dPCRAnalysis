test_that("adapt_dpcr_to_twoddpcr maps app data into twoddpcr wells", {
  skip_if_not_installed("twoddpcr")

  df <- make_twoddpcr_test_data()
  adapter <- adapt_dpcr_to_twoddpcr(
    df,
    samples = "Sample_1",
    channel_x = "C",
    channel_y = "G"
  )

  expect_true(adapter$ok)
  expect_equal(adapter$channel_x, "C")
  expect_equal(adapter$channel_y, "G")
  expect_equal(length(names(adapter$plate)), 1)
  expect_true(all(adapter$pair_data$sample == "Sample_1"))
  expect_equal(adapter$pair_data$Ch1.Amplitude, adapter$pair_data$y_value)
  expect_equal(adapter$pair_data$Ch2.Amplitude, adapter$pair_data$x_value)
  expect_equal(adapter$well_map$sample, "Sample_1")
  expect_equal(adapter$well_map$well, "A1")
})

test_that("twoddpcr classification wrappers and summary table run on app-like data", {
  skip_if_not_installed("twoddpcr")

  df <- make_twoddpcr_test_data()
  adapter <- adapt_dpcr_to_twoddpcr(
    df,
    channel_x = "C",
    channel_y = "G"
  )

  threshold_classification <- classify_twoddpcr_plate(
    adapter$plate,
    method = "thresholds",
    ch1_threshold = 700,
    ch2_threshold = 600
  )
  expect_true("thresholds" %in% twoddpcr::commonClassificationMethod(threshold_classification$plate))

  kmeans_classification <- classify_twoddpcr_plate(
    adapter$plate,
    method = "kmeans",
    kmeans_centers = 4
  )
  expect_true("kmeans" %in% twoddpcr::commonClassificationMethod(kmeans_classification$plate))

  rain_classification <- apply_twoddpcr_rain_to_plate(
    kmeans_classification$plate,
    base_method = "kmeans",
    rain_type = "mahalanobis",
    mahalanobis_max = list(NN = 15, NP = 20, PN = 25, PP = 30)
  )
  expect_true("kmeansMahRain" %in% twoddpcr::commonClassificationMethod(rain_classification$plate))

  summary_df <- build_twoddpcr_summary_table(
    rain_classification$plate,
    c_method = rain_classification$method,
    well_map = adapter$well_map,
    ch1_abbrev = "G",
    ch2_abbrev = "C",
    type = "full"
  )

  expect_equal(nrow(summary_df), 2)
  expect_setequal(summary_df$sample, c("Sample_1", "Sample_2"))
  expect_true(all(summary_df$AcceptedDroplets == 80))
})

test_that("twoddpcr grid and knn classification wrappers work on app-like data", {
  skip_if_not_installed("twoddpcr")

  df <- make_twoddpcr_test_data()
  adapter <- adapt_dpcr_to_twoddpcr(
    df,
    channel_x = "C",
    channel_y = "G"
  )

  grid_thresholds <- list(
    ch1NNThreshold = 700,
    ch2NNThreshold = 600,
    ch1NPThreshold = 700,
    ch2NPThreshold = 600,
    ch1PNThreshold = 700,
    ch2PNThreshold = 600,
    ch1PPThreshold = 700,
    ch2PPThreshold = 600
  )

  grid_classification <- classify_twoddpcr_plate(
    adapter$plate,
    method = "grid",
    grid_thresholds = grid_thresholds
  )
  expect_true("grid" %in% twoddpcr::commonClassificationMethod(grid_classification$plate))

  training_df <- build_twoddpcr_training_data(
    adapter$plate,
    source = "grid",
    grid_thresholds = grid_thresholds
  )
  expect_true(all(c("Ch1.Amplitude", "Ch2.Amplitude", "class") %in% names(training_df)))
  expect_gt(nrow(training_df), 0)

  knn_classification <- classify_twoddpcr_plate(
    adapter$plate,
    method = "knn",
    knn_training_data = training_df,
    knn_k = 3,
    knn_prob = 0.6
  )
  expect_true("knn" %in% twoddpcr::commonClassificationMethod(knn_classification$plate))
})
