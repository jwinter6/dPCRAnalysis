count_missing_values <- function(x) {
  if (is.character(x)) {
    return(sum(is.na(x) | trimws(x) == ""))
  }

  sum(is.na(x))
}

validate_dpcr_data <- function(df) {
  issues <- empty_issues_table()

  add_issue <- function(severity, field, message, n_rows = NA_integer_) {
    issues <<- dplyr::bind_rows(
      issues,
      tibble::tibble(
        severity = as.character(severity),
        field = as.character(field),
        message = as.character(message),
        n_rows = as.integer(n_rows)
      )
    )
  }

  if (is.null(df) || !is.data.frame(df)) {
    add_issue("error", "data", "Kein gültiger Datensatz zur Validierung übergeben.", 0)
    return(list(ok = FALSE, issues = issues, clean_data = new_empty_dpcr_data()))
  }

  raw_df <- tibble::as_tibble(df)
  raw_names <- names(raw_df)

  missing_required <- setdiff(DPCR_REQUIRED_COLUMNS, raw_names)
  if (length(missing_required) > 0) {
    for (col in missing_required) {
      add_issue("error", col, sprintf("Pflichtfeld fehlt: %s", col), NA_integer_)
    }
  }

  for (col in intersect(DPCR_NUMERIC_COLUMNS, raw_names)) {
    raw_col <- raw_df[[col]]
    parsed_col <- parse_numeric_safe(raw_col)

    non_numeric_count <- sum(
      !is.na(raw_col) &
        trimws(as.character(raw_col)) != "" &
        is.na(parsed_col)
    )

    if (non_numeric_count > 0) {
      add_issue(
        "error",
        col,
        sprintf("Spalte '%s' enthält nicht-numerische Werte.", col),
        non_numeric_count
      )
    }
  }

  for (col in intersect(DPCR_LOGICAL_COLUMNS, raw_names)) {
    raw_col <- raw_df[[col]]
    parsed_col <- parse_logical_like(raw_col)

    invalid_logical_count <- sum(
      !is.na(raw_col) &
        trimws(as.character(raw_col)) != "" &
        is.na(parsed_col)
    )

    if (invalid_logical_count > 0) {
      add_issue(
        "warning",
        col,
        sprintf("Spalte '%s' enthält uneindeutige Wahr/Falsch-Werte.", col),
        invalid_logical_count
      )
    }
  }

  clean <- coerce_dpcr_schema(raw_df)

  for (col in DPCR_REQUIRED_COLUMNS) {
    missing_count <- count_missing_values(clean[[col]])
    if (missing_count > 0) {
      add_issue(
        "warning",
        col,
        sprintf("Spalte '%s' enthält fehlende Werte.", col),
        missing_count
      )
    }
  }

  negative_rfu <- sum(!is.na(clean$rfu) & clean$rfu < 0)
  if (negative_rfu > 0) {
    add_issue("warning", "rfu", "Negative RFU-Werte gefunden.", negative_rfu)
  }

  non_positive_partition <- sum(!is.na(clean$partition) & clean$partition <= 0)
  if (non_positive_partition > 0) {
    add_issue("warning", "partition", "Partitionen <= 0 gefunden.", non_positive_partition)
  }

  unknown_device <- sum(as.character(clean$device_type) == "unknown")
  if (unknown_device > 0) {
    add_issue("warning", "device_type", "Gerätetyp konnte nicht eindeutig erkannt werden.", unknown_device)
  }

  has_errors <- any(issues$severity == "error")

  list(
    ok = !has_errors,
    issues = issues,
    clean_data = clean
  )
}
