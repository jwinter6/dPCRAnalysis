test_that("compute_auto_threshold returns a finite threshold for bimodal values", {
  values <- c(rep(10, 50), rep(100, 50))

  threshold <- compute_auto_threshold(values)

  expect_true(is.finite(threshold))
  expect_true(threshold > 10)
  expect_true(threshold < 100)
})

test_that("default_channel_axis_label uses selected channel dynamically", {
  expect_equal(default_channel_axis_label("FAM", "X"), "Fluoreszenzkanal FAM")
  expect_equal(default_channel_axis_label(character(), "Y"), "Fluoreszenzkanal Y")
})

test_that("plot A channel filter selects only the requested channel", {
  df <- tibble::tibble(
    plate_name = rep("Plate_1", 3),
    plate_id = rep("id-1", 3),
    plate_type = rep("Nanoplate", 3),
    well = c("A1", "A1", "A1"),
    sample = c("Sample_1", "Sample_1", "Sample_1"),
    channel = c("C", "G", "REF"),
    color_channel = c("green", "yellow", NA_character_),
    volume = rep(24, 3),
    threshold = rep(100, 3),
    partition = c(1, 1, 1),
    rfu = c(120, 90, 15),
    invalid_partition = c(FALSE, FALSE, FALSE),
    positive_control = c(TRUE, FALSE, FALSE),
    reference = rep("Std-Ref", 3),
    device_type = factor(rep("qiaquity", 3), levels = DEVICE_LEVELS),
    source_file = rep("x.csv", 3)
  )

  filtered_df <- build_plot_a_data(df, channel = "G")

  expect_equal(nrow(filtered_df), 1)
  expect_equal(filtered_df$channel[[1]], "G")
})

test_that("plot B sample filter behaves like plot A multi-select", {
  df <- tibble::tibble(
    plate_name = rep("Plate_1", 8),
    plate_id = rep("id-1", 8),
    plate_type = rep("Nanoplate", 8),
    well = c("A1", "A1", "A2", "A2", "A1", "A1", "A2", "A2"),
    sample = c("Sample_1", "Sample_1", "Sample_1", "Sample_1", "Sample_2", "Sample_2", "Sample_2", "Sample_2"),
    channel = c("C", "G", "C", "G", "C", "G", "C", "G"),
    color_channel = c("green", "yellow", "green", "yellow", "green", "yellow", "green", "yellow"),
    volume = rep(24, 8),
    threshold = rep(100, 8),
    partition = c(1, 1, 2, 2, 1, 1, 2, 2),
    rfu = c(120, 130, 80, 90, 140, 150, 60, 70),
    invalid_partition = rep(FALSE, 8),
    positive_control = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    reference = rep("Std-Ref", 8),
    device_type = factor(rep("qiaquity", 8), levels = DEVICE_LEVELS),
    source_file = rep("x.csv", 8)
  )

  pair_df <- build_plot_b_data(df, channel_x = "C", channel_y = "G", samples = "Sample_2")

  expect_equal(nrow(pair_df), 2)
  expect_true(all(pair_df$sample == "Sample_2"))
})

test_that("plot B utilities join channels and compute quadrant counts", {
  df <- tibble::tibble(
    plate_name = rep("Plate_1", 4),
    plate_id = rep("id-1", 4),
    plate_type = rep("Nanoplate", 4),
    well = rep("A1", 4),
    sample = rep("Sample_1", 4),
    channel = c("C", "G", "C", "G"),
    color_channel = c("green", "yellow", "green", "yellow"),
    volume = rep(24, 4),
    threshold = c(100, 100, 100, 100),
    partition = c(1, 1, 2, 2),
    rfu = c(120, 130, 80, 90),
    invalid_partition = c(FALSE, FALSE, FALSE, FALSE),
    positive_control = c(TRUE, TRUE, FALSE, FALSE),
    reference = rep("Std-Ref", 4),
    device_type = factor(rep("qiaquity", 4), levels = DEVICE_LEVELS),
    source_file = rep("x.csv", 4)
  )

  pair_df <- build_plot_b_data(df, channel_x = "C", channel_y = "G")
  table_df <- build_plot_b_table(pair_df, threshold_x = 100, threshold_y = 100)

  expect_equal(nrow(pair_df), 2)
  expect_equal(pair_df$x_value, c(120, 80))
  expect_equal(pair_df$y_value, c(130, 90))
  expect_equal(table_df$gesamt[[1]], 2)
  expect_equal(table_df$quadrant_x_pos_y_pos[[1]], 1)
  expect_equal(table_df$quadrant_x_neg_y_neg[[1]], 1)
})
