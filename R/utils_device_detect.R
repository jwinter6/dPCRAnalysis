safe_read_header_preview <- function(file_path, n_lines = 3) {
  tryCatch(
    readLines(file_path, n = n_lines, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
}

guess_delimiter_from_lines <- function(lines, default = ",") {
  lines <- as.character(lines)
  lines <- lines[!is.na(lines) & nzchar(lines)]

  if (length(lines) == 0) {
    return(default)
  }

  candidates <- c(",", ";", "\t")
  counts <- vapply(
    candidates,
    function(delim) {
      sum(vapply(
        lines,
        function(line) {
          matches <- gregexpr(delim, line, fixed = TRUE)[[1]]
          if (length(matches) == 1 && matches[[1]] < 0) {
            return(0L)
          }
          length(matches)
        },
        integer(1)
      ))
    },
    numeric(1)
  )

  if (all(counts <= 0)) {
    return(default)
  }

  candidates[[which.max(counts)]]
}

split_preview_line <- function(line, delimiter = ",") {
  line <- gsub("^\\ufeff", "", as.character(line))
  trimws(strsplit(line, delimiter, fixed = TRUE)[[1]])
}

is_roche_header_parts <- function(header_parts) {
  header_parts <- trimws(tolower(header_parts))
  header_parts <- header_parts[nzchar(header_parts)]

  channel_like <- grepl("^channel\\s*[0-9]+$", header_parts)
  named_channel_like <- header_parts %in% c(
    "fam", "hex", "rox", "cy5", "cy5.5", "red", "green", "yellow",
    "orange", "blue", "channel 1", "channel 2", "channel 3", "channel 4"
  )

  (sum(channel_like | named_channel_like) >= 3) &&
    any(header_parts %in% c("flag", "flags", "qualityflag", "quality_flag"))
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
  delimiter <- guess_delimiter_from_lines(header_preview)

  header_line <- if (length(header_preview) >= 2) {
    header_preview[[2]]
  } else {
    header_preview[[1]]
  }

  header_parts <- split_preview_line(tolower(header_line), delimiter = delimiter)

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

  roche_header_line <- header_preview[[1]]
  roche_header_parts <- split_preview_line(tolower(roche_header_line), delimiter = delimiter)

  if (is_roche_header_parts(roche_header_parts)) {
    return("roche")
  }

  if (grepl("roche|digital lightcycler|lightcycler", header_text)) {
    return("roche")
  }

  if (grepl("bio-rad|biorad|qx100|qx200|qx400|qx600|qx700|qx800|droplet", header_text)) {
    return("biorad")
  }

  "unknown"
}
