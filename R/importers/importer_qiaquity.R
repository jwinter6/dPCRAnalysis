import_qiaquity_csv <- function(path) {
  raw <- readr::read_csv(
    file = path,
    skip = 1,
    col_types = readr::cols(.default = readr::col_guess()),
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )

  if (ncol(raw) < 13) {
    stop(sprintf("QIAcuity-Datei '%s' enthält weniger als 13 Spalten.", basename(path)))
  }

  mapped <- raw[, 1:13]
  names(mapped) <- c(
    "plate_name",
    "plate_id",
    "plate_type",
    "well",
    "sample",
    "channel",
    "volume",
    "threshold",
    "partition",
    "invalid_partition",
    "positive_control",
    "rfu",
    "reference"
  )

  mapped <- tibble::as_tibble(mapped)
  mapped <- trim_character_columns(mapped)

  mapped <- tidyr::fill(mapped, plate_name, plate_id, plate_type, .direction = "down")

  mapped <- dplyr::mutate(
    mapped,
    color_channel = derive_color_channel(channel),
    volume = parse_numeric_safe(volume),
    threshold = parse_numeric_safe(threshold),
    partition = parse_numeric_safe(partition),
    rfu = parse_numeric_safe(rfu),
    invalid_partition = parse_logical_like(invalid_partition),
    positive_control = parse_logical_like(positive_control),
    device_type = "qiaquity",
    source_file = basename(path)
  )

  mapped <- dplyr::filter(
    mapped,
    !(is.na(partition) & is.na(rfu) & (is.na(sample) | sample == ""))
  )

  coerce_dpcr_schema(mapped)
}
