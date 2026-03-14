test_that("sample detail module filters samples globally and produces twoddpcr outputs", {
  skip_if_not_installed("twoddpcr")

  state <- shiny::reactiveValues(
    dpcr_data = make_twoddpcr_test_data()
  )

  shiny::testServer(
    mod_sample_detail_server,
    args = list(id = "sample_detail", state = state),
    {
      api <- session$getReturned()

      session$setInputs(
        detail_plate_select = "__all__",
        detail_samples = "Sample_1",
        detail_channel_x = "C",
        detail_channel_y = "G",
        detail_exclude_invalid = TRUE
      )
      session$flushReact()

      detail_adapter <- api$build_detail_adapter()
      expect_true(detail_adapter$ok)
      expect_true(all(detail_adapter$pair_data$sample == "Sample_1"))

      session$setInputs(
        classify_mode = "thresholds",
        detail_ch1_threshold = 700,
        detail_ch2_threshold = 600
      )
      session$flushReact()
      api$run_classification_internal()
      session$setInputs(results_method = "thresholds")
      session$flushReact()

      classified_plate <- shiny::isolate(api$current_analysis_plate())
      expect_false(is.null(classified_plate))
      expect_true("thresholds" %in% twoddpcr::commonClassificationMethod(classified_plate))

      summary_df <- build_twoddpcr_summary_table(
        classified_plate,
        c_method = "thresholds",
        well_map = detail_adapter$well_map,
        ch1_abbrev = "G",
        ch2_abbrev = "C",
        type = "full"
      )
      expect_equal(nrow(summary_df), 1)
      expect_equal(summary_df$sample, "Sample_1")
      expect_equal(summary_df$well, "A1")
    }
  )
})

test_that("sample detail module supports grid, knn and per-cluster rain parameters", {
  skip_if_not_installed("twoddpcr")

  state <- shiny::reactiveValues(
    dpcr_data = make_twoddpcr_test_data()
  )

  shiny::testServer(
    mod_sample_detail_server,
    args = list(id = "sample_detail", state = state),
    {
      api <- session$getReturned()

      session$setInputs(
        detail_plate_select = "__all__",
        detail_samples = c("Sample_1", "Sample_2"),
        detail_channel_x = "C",
        detail_channel_y = "G",
        detail_exclude_invalid = TRUE,
        classify_mode = "grid",
        grid_ch1_nn = 700,
        grid_ch2_nn = 600,
        grid_ch1_np = 700,
        grid_ch2_np = 600,
        grid_ch1_pn = 700,
        grid_ch2_pn = 600,
        grid_ch1_pp = 700,
        grid_ch2_pp = 600
      )
      session$flushReact()

      grid_result <- api$run_classification_internal()
      expect_true("grid" %in% twoddpcr::commonClassificationMethod(grid_result$plate))

      session$setInputs(
        classify_mode = "knn",
        knn_training_source = "grid",
        knn_k = 3,
        knn_prob = 0.6
      )
      session$flushReact()

      training_df <- api$build_training_data_internal(api$build_detail_adapter())
      expect_gt(nrow(training_df), 0)

      knn_result <- api$run_classification_internal()
      expect_true("knn" %in% twoddpcr::commonClassificationMethod(knn_result$plate))

      session$setInputs(
        rain_type = "mahalanobis",
        mvn_rain_nn = 10,
        mvn_rain_np = 20,
        mvn_rain_pn = 30,
        mvn_rain_pp = 40
      )
      session$flushReact()

      rain_result <- api$apply_rain_internal()
      expect_equal(rain_result$method, "knnMahRain")
      expect_true("knnMahRain" %in% twoddpcr::commonClassificationMethod(rain_result$plate))
    }
  )
})
