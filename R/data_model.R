parse_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

parse_logical_like <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  result <- rep(NA, length(x_chr))

  result[x_chr %in% c("true", "t", "yes", "y", "1")] <- TRUE
  result[x_chr %in% c("false", "f", "no", "n", "0")] <- FALSE

  numeric_candidate <- suppressWarnings(as.numeric(x_chr))
  numeric_idx <- is.na(result) & !is.na(numeric_candidate)
  result[numeric_idx] <- numeric_candidate[numeric_idx] > 0

  na_like <- x_chr %in% c("", "na", "n/a", "null")
  result[na_like] <- NA

  as.logical(result)
}

derive_color_channel <- function(channel_value) {
  channel_chr <- trimws(toupper(as.character(channel_value)))

  dplyr::case_when(
    channel_chr %in% c("C", "FAM", "GREEN", "G") ~ "green",
    channel_chr %in% c("R", "ROX", "HEX", "RED") ~ "red",
    channel_chr %in% c("B", "BLUE") ~ "blue",
    channel_chr %in% c("Y", "YELLOW") ~ "yellow",
    TRUE ~ NA_character_
  )
}

new_empty_dpcr_data <- function() {
  tibble::tibble(
    plate_name = character(),
    plate_id = character(),
    plate_type = character(),
    well = character(),
    sample = character(),
    channel = character(),
    color_channel = character(),
    volume = numeric(),
    threshold = numeric(),
    partition = numeric(),
    rfu = numeric(),
    invalid_partition = logical(),
    positive_control = logical(),
    reference = character(),
    device_type = factor(character(), levels = DEVICE_LEVELS),
    source_file = character()
  )
}

trim_character_columns <- function(df) {
  char_cols <- names(df)[vapply(df, is.character, logical(1))]

  if (length(char_cols) == 0) {
    return(df)
  }

  dplyr::mutate(df, dplyr::across(dplyr::all_of(char_cols), ~ trimws(.x)))
}

coerce_dpcr_schema <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(new_empty_dpcr_data())
  }

  out <- tibble::as_tibble(df)

  missing_cols <- setdiff(DPCR_STANDARD_COLUMNS, names(out))
  for (col in missing_cols) {
    out[[col]] <- NA
  }

  out <- trim_character_columns(out)

  for (col in DPCR_CHARACTER_COLUMNS) {
    out[[col]] <- as.character(out[[col]])
  }

  for (col in DPCR_NUMERIC_COLUMNS) {
    out[[col]] <- parse_numeric_safe(out[[col]])
  }

  for (col in DPCR_LOGICAL_COLUMNS) {
    out[[col]] <- parse_logical_like(out[[col]])
  }

  device_chr <- trimws(tolower(as.character(out$device_type)))
  device_chr[device_chr == "" | is.na(device_chr)] <- "unknown"
  device_chr[!device_chr %in% DEVICE_LEVELS] <- "unknown"
  out$device_type <- factor(device_chr, levels = DEVICE_LEVELS)

  out <- dplyr::select(out, dplyr::all_of(DPCR_STANDARD_COLUMNS))

  tibble::as_tibble(out)
}
