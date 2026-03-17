test_that("sample analysis module initializes with empty data", {
  state <- shiny::reactiveValues(
    dpcr_data = new_empty_dpcr_data(),
    app_settings = get_default_app_settings(),
    custom_palettes = list()
  )

  expect_no_error(
    shiny::testServer(
      mod_sample_analysis_server,
      args = list(id = "sample_analysis", state = state),
      {
        session$flushReact()
      }
    )
  )
})

test_that("sample analysis module toggles filename color choice based on source files", {
  df <- tibble::tibble(
    plate_name = rep("Plate_1", 4),
    plate_id = rep("id-1", 4),
    plate_type = rep("Nanoplate", 4),
    well = c("A1", "A1", "A2", "A2"),
    sample = c("Sample_1", "Sample_1", "Sample_2", "Sample_2"),
    channel = c("C", "G", "C", "G"),
    color_channel = c("green", "yellow", "green", "yellow"),
    volume = rep(24, 4),
    threshold = rep(100, 4),
    partition = c(1, 1, 2, 2),
    rfu = c(120, 130, 80, 90),
    invalid_partition = rep(FALSE, 4),
    positive_control = c(TRUE, TRUE, FALSE, FALSE),
    reference = rep("Std-Ref", 4),
    device_type = factor(rep("qiaquity", 4), levels = DEVICE_LEVELS),
    source_file = c("one.csv", "one.csv", "two.csv", "two.csv")
  )

  state <- shiny::reactiveValues(
    dpcr_data = df,
    app_settings = get_default_app_settings(),
    custom_palettes = list()
  )

  shiny::testServer(
    mod_sample_analysis_server,
    args = list(id = "sample_analysis", state = state),
    {
      api <- session$getReturned()
      session$flushReact()

      expect_true("source_file" %in% unname(api$plot_a_color_choices()))
      expect_true("source_file" %in% unname(api$plot_b_color_choices()))

      state$dpcr_data <- dplyr::mutate(df, source_file = "one.csv")
      session$flushReact()

      expect_false("source_file" %in% unname(api$plot_a_color_choices()))
      expect_false("source_file" %in% unname(api$plot_b_color_choices()))
    }
  )
})
