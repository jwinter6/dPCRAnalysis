guess_decimal_mark_for_delimiter <- function(delimiter = ",") {
  if (identical(delimiter, ";")) {
    return(",")
  }

  "."
}

read_roche_csv_raw <- function(path) {
  preview <- safe_read_header_preview(path, n_lines = 5)
  delimiter <- guess_delimiter_from_lines(preview, default = ",")
  decimal_mark <- guess_decimal_mark_for_delimiter(delimiter)

  raw <- readr::read_delim(
    file = path,
    delim = delimiter,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(decimal_mark = decimal_mark),
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )

  raw <- tibble::as_tibble(raw)
  names(raw) <- trimws(names(raw))
  trim_character_columns(raw)
}

parse_roche_filename_metadata <- function(path) {
  filename <- basename(path)
  stem <- sub("\\.[Cc][Ss][Vv]$", "", filename)
  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]

  if (length(parts) < 2) {
    stop(sprintf(
      "Ungültiger Roche-Dateiname '%s': mindestens ein Unterstrich ('_') für Plate- und Sample-Extraktion ist erforderlich.",
      filename
    ))
  }

  if (grepl("_$", stem)) {
    stop(sprintf(
      "Ungültiger Roche-Dateiname '%s': Sample-Name nach dem letzten Unterstrich ist leer.",
      filename
    ))
  }

  plate_name <- trimws(parts[[1]])
  sample_name <- trimws(parts[[length(parts)]])

  if (!nzchar(plate_name)) {
    stop(sprintf(
      "Ungültiger Roche-Dateiname '%s': Plattenname vor dem ersten Unterstrich ist leer.",
      filename
    ))
  }

  if (!nzchar(sample_name)) {
    stop(sprintf(
      "Ungültiger Roche-Dateiname '%s': Sample-Name nach dem letzten Unterstrich ist leer.",
      filename
    ))
  }

  lane_parts <- parts[grepl("^lane[0-9]+$", parts, ignore.case = TRUE)]
  if (length(lane_parts) > 0) {
    well <- lane_parts[[1]]
  } else {
    well <- sample_name
  }

  list(
    plate_name = plate_name,
    plate_id = plate_name,
    plate_type = "Digital LightCycler",
    well = well,
    sample = sample_name
  )
}

guess_roche_positive_control <- function(sample_name) {
  sample_name <- trimws(toupper(as.character(sample_name)))

  dplyr::case_when(
    grepl("(^|[_-])(NTC|NEG|NEGATIVE)([_-]|$)", sample_name) ~ FALSE,
    grepl("(^|[_-])(POS|POSITIVE|PC)([_-]|$)", sample_name) ~ TRUE,
    TRUE ~ NA
  )
}

is_roche_channel_column <- function(name) {
  name <- trimws(tolower(as.character(name)))
  grepl("^channel\\s*[0-9]+$", name) ||
    name %in% c("fam", "hex", "rox", "cy5", "cy5.5", "red", "green", "yellow", "orange", "blue")
}

parse_roche_numeric <- function(x, decimal_mark = ".") {
  readr::parse_number(
    as.character(x),
    locale = readr::locale(
      decimal_mark = decimal_mark,
      grouping_mark = if (identical(decimal_mark, ",")) "." else ","
    )
  )
}

find_active_roche_channel_columns <- function(raw, decimal_mark = ".") {
  channel_cols <- names(raw)[vapply(names(raw), is_roche_channel_column, logical(1))]

  if (length(channel_cols) == 0) {
    return(character())
  }

  keep <- vapply(channel_cols, function(col) {
    values <- parse_roche_numeric(raw[[col]], decimal_mark = decimal_mark)
    any(!is.na(values) & values != 0)
  }, logical(1))

  active <- channel_cols[keep]
  if (length(active) == 0) {
    return(channel_cols)
  }

  active
}

import_roche_csv <- function(path, file_name = basename(path)) {
  preview <- safe_read_header_preview(path, n_lines = 5)
  delimiter <- guess_delimiter_from_lines(preview, default = ",")
  decimal_mark <- guess_decimal_mark_for_delimiter(delimiter)
  raw <- read_roche_csv_raw(path)

  if (!is_roche_header_parts(names(raw))) {
    stop(sprintf("Datei '%s' entspricht keinem unterstützten Roche-Digital-LightCycler-CSV-Layout.", file_name))
  }

  metadata <- parse_roche_filename_metadata(file_name)
  channel_cols <- find_active_roche_channel_columns(raw, decimal_mark = decimal_mark)

  if (length(channel_cols) == 0) {
    stop(sprintf("Datei '%s' enthält keine auswertbaren Roche-Kanalspalten.", file_name))
  }

  flag_col <- names(raw)[tolower(names(raw)) %in% c("flag", "flags", "qualityflag", "quality_flag")]
  flag_col <- if (length(flag_col) == 0) NULL else flag_col[[1]]

  mapped <- raw |>
    dplyr::mutate(partition = seq_len(dplyr::n())) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(channel_cols),
      names_to = "channel",
      values_to = "rfu_raw"
    ) |>
    dplyr::mutate(
      plate_name = metadata$plate_name,
      plate_id = metadata$plate_id,
      plate_type = metadata$plate_type,
      well = metadata$well,
      sample = metadata$sample,
      color_channel = derive_color_channel(channel),
      volume = NA_real_,
      threshold = NA_real_,
      partition = parse_numeric_safe(partition),
      rfu = parse_roche_numeric(rfu_raw, decimal_mark = decimal_mark),
      invalid_partition = if (!is.null(flag_col)) {
        trimws(tolower(.data[[flag_col]])) != "valid"
      } else {
        FALSE
      },
      positive_control = guess_roche_positive_control(sample),
      reference = if (!is.null(flag_col)) .data[[flag_col]] else "Roche Digital LightCycler",
      device_type = "roche",
      source_file = file_name
    ) |>
    dplyr::select(dplyr::all_of(DPCR_STANDARD_COLUMNS))

  mapped <- dplyr::filter(
    mapped,
    !(is.na(rfu) & is.na(partition))
  )

  coerce_dpcr_schema(mapped)
}
