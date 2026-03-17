save_analysis_rdata <- function(path,
                                dpcr_data,
                                validation_report,
                                metadata = list(),
                                app_settings = get_default_app_settings(),
                                custom_palettes = list()) {
  dpcr_data <- coerce_dpcr_schema(dpcr_data)
  validation_report <- tibble::as_tibble(validation_report)
  saved_at <- Sys.time()
  app_version <- APP_VERSION
  app_settings <- sanitize_app_settings(app_settings, custom_palettes)
  custom_palettes <- sanitize_custom_palettes(custom_palettes)

  save(dpcr_data, validation_report, metadata, app_settings, custom_palettes, saved_at, app_version, file = path)
  invisible(path)
}

load_analysis_rdata <- function(path) {
  env <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = env)

  if (!("dpcr_data" %in% loaded_names)) {
    stop("Die geladene RData-Datei enthält kein Objekt 'dpcr_data'.")
  }

  result <- list(
    dpcr_data = coerce_dpcr_schema(env$dpcr_data),
    validation_report = if ("validation_report" %in% loaded_names) {
      tibble::as_tibble(env$validation_report)
    } else {
      empty_issues_table()
    },
    metadata = if ("metadata" %in% loaded_names) env$metadata else list(),
    app_settings = if ("app_settings" %in% loaded_names) {
      sanitize_app_settings(env$app_settings, if ("custom_palettes" %in% loaded_names) env$custom_palettes else list())
    } else {
      get_default_app_settings()
    },
    custom_palettes = if ("custom_palettes" %in% loaded_names) {
      sanitize_custom_palettes(env$custom_palettes)
    } else {
      list()
    },
    saved_at = if ("saved_at" %in% loaded_names) env$saved_at else NULL,
    app_version = if ("app_version" %in% loaded_names) env$app_version else NA_character_
  )

  result
}

build_analysis_summary <- function(dpcr_data) {
  df <- coerce_dpcr_schema(dpcr_data)

  tibble::tibble(
    Kennzahl = c(
      "Zeilen gesamt",
      "Platten",
      "Samples",
      "Kanäle",
      "Dateien"
    ),
    Wert = c(
      nrow(df),
      dplyr::n_distinct(df$plate_name, na.rm = TRUE),
      dplyr::n_distinct(df$sample, na.rm = TRUE),
      dplyr::n_distinct(df$channel, na.rm = TRUE),
      dplyr::n_distinct(df$source_file, na.rm = TRUE)
    )
  )
}
