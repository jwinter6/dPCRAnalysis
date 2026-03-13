safe_read_header_preview <- function(file_path, n_lines = 3) {
  tryCatch(
    readLines(file_path, n = n_lines, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
}

detect_device_type <- function(file_path, header_preview = NULL) {
  if (is.null(header_preview)) {
    header_preview <- safe_read_header_preview(file_path, n_lines = 3)
  }

  if (length(header_preview) == 0) {
    return("unknown")
  }

  header_preview <- gsub("^\\ufeff", "", header_preview)
  header_text <- tolower(paste(header_preview, collapse = ","))

  header_line <- if (length(header_preview) >= 2) {
    header_preview[[2]]
  } else {
    header_preview[[1]]
  }

  header_parts <- trimws(strsplit(tolower(header_line), ",", fixed = TRUE)[[1]])

  qia_base_signature <- c("plate name", "plate id", "plate type", "well", "sample")
  has_qia_base_signature <- all(qia_base_signature %in% header_parts)
  has_qia_channel_layout <- all(c("channel", "partition", "rfu") %in% header_parts)
  has_qia_ref_layout <- all(c("ref", "partition", "rfu") %in% header_parts)
  has_sep_marker <- any(grepl("^sep=,", tolower(header_preview)))

  if (
    (has_qia_base_signature && (has_qia_channel_layout || has_qia_ref_layout)) ||
      (has_sep_marker && has_qia_base_signature && length(header_parts) >= 12)
  ) {
    return("qiaquity")
  }

  if (grepl("roche|digital lightcycler|lightcycler", header_text)) {
    return("roche")
  }

  if (grepl("bio-rad|biorad|qx100|qx200|qx400|qx600|qx700|qx800|droplet", header_text)) {
    return("biorad")
  }

  "unknown"
}
