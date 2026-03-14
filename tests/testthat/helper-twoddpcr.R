make_twoddpcr_test_data <- function() {
  build_well <- function(sample_name, well_name, x_shift = 0, y_shift = 0) {
    partitions <- 1:80
    quadrants <- rep(c("NN", "PN", "NP", "PP"), each = 20)
    x_values <- ifelse(quadrants %in% c("NP", "PP"), 1000, 200) + x_shift + partitions / 100
    y_values <- ifelse(quadrants %in% c("PN", "PP"), 1200, 300) + y_shift + partitions / 100

    tibble::tibble(
      plate_name = "Plate_1",
      plate_id = "plate-1",
      plate_type = "Nanoplate",
      well = well_name,
      sample = sample_name,
      channel = rep(c("C", "G"), each = length(partitions)),
      color_channel = rep(c("green", "yellow"), each = length(partitions)),
      volume = 24,
      threshold = 500,
      partition = rep(partitions, times = 2),
      rfu = c(x_values, y_values),
      invalid_partition = FALSE,
      positive_control = FALSE,
      reference = "Ref",
      device_type = factor("qiaquity", levels = DEVICE_LEVELS),
      source_file = "synthetic.csv"
    )
  }

  dplyr::bind_rows(
    build_well("Sample_1", "A1", x_shift = 0, y_shift = 0),
    build_well("Sample_2", "B1", x_shift = 25, y_shift = 30)
  )
}
