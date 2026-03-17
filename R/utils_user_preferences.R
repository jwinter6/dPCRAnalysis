is_scalar_nonempty_string <- function(value) {
  value <- trimws(as.character(value))
  value <- value[!is.na(value) & nzchar(value)]
  length(value) >= 1
}

sanitize_single_hex_color <- function(value) {
  value <- toupper(trimws(as.character(value)))
  value <- value[!is.na(value) & nzchar(value)]

  if (length(value) == 0) {
    return(NULL)
  }

  value <- value[[1]]
  if (!grepl("^#[0-9A-F]{6}$", value)) {
    return(NULL)
  }

  value
}

get_default_user_preferences <- function() {
  browser_preferences_from_state(get_default_app_settings(), list())
}

browser_preferences_from_state <- function(app_settings = get_default_app_settings(), custom_palettes = list()) {
  custom_palettes <- sanitize_custom_palettes(custom_palettes)
  app_settings <- sanitize_app_settings(app_settings, custom_palettes)
  preview_colors <- get_palette_preview_colors(app_settings, custom_palettes, n = 2)

  list(
    theme = list(
      paletteName = app_settings$palette_id,
      primaryColor = if (length(preview_colors) >= 1) preview_colors[[1]] else NULL,
      accentColor = if (length(preview_colors) >= 2) preview_colors[[2]] else if (length(preview_colors) == 1) preview_colors[[1]] else NULL
    ),
    appSettings = app_settings,
    customPalettes = custom_palettes
  )
}

sanitize_user_preferences <- function(preferences = list()) {
  preferences <- if (is.null(preferences) || !is.list(preferences)) list() else preferences
  custom_palettes <- sanitize_custom_palettes(preferences$customPalettes)

  app_settings_input <- if (is.list(preferences$appSettings)) {
    preferences$appSettings
  } else {
    list(palette_id = preferences$theme$paletteName)
  }

  app_settings <- sanitize_app_settings(app_settings_input, custom_palettes)
  preview_colors <- get_palette_preview_colors(app_settings, custom_palettes, n = 2)

  theme <- if (is.list(preferences$theme)) preferences$theme else list()

  list(
    theme = list(
      paletteName = app_settings$palette_id,
      primaryColor = sanitize_single_hex_color(theme$primaryColor) %||% if (length(preview_colors) >= 1) preview_colors[[1]] else NULL,
      accentColor = sanitize_single_hex_color(theme$accentColor) %||% if (length(preview_colors) >= 2) preview_colors[[2]] else if (length(preview_colors) == 1) preview_colors[[1]] else NULL
    ),
    appSettings = app_settings,
    customPalettes = custom_palettes
  )
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || (length(lhs) == 1 && is.na(lhs))) {
    return(rhs)
  }

  lhs
}

extract_query_parameter <- function(url_search = NULL, name = "userId") {
  url_search <- as.character(url_search)
  url_search <- url_search[!is.na(url_search) & nzchar(url_search)]

  if (length(url_search) == 0) {
    return(NULL)
  }

  query <- sub("^\\?", "", url_search[[1]])
  if (!nzchar(query)) {
    return(NULL)
  }

  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  if (length(parts) == 0) {
    return(NULL)
  }

  for (part in parts) {
    key_value <- strsplit(part, "=", fixed = TRUE)[[1]]
    key <- utils::URLdecode(key_value[[1]])

    if (!identical(key, name)) {
      next
    }

    value <- if (length(key_value) >= 2) utils::URLdecode(paste(key_value[-1], collapse = "=")) else ""
    value <- trimws(value)

    if (nzchar(value)) {
      return(value)
    }
  }

  NULL
}

resolve_browser_user_id <- function(server_user_id = NULL, url_search = NULL, fallback = "anonymous") {
  candidates <- c(
    trimws(as.character(server_user_id)),
    extract_query_parameter(url_search, "userId"),
    extract_query_parameter(url_search, "user_id"),
    fallback
  )

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  if (length(candidates) == 0) {
    return("anonymous")
  }

  candidates[[1]]
}

apply_user_preferences_to_state <- function(state, preferences = list()) {
  sanitized <- sanitize_user_preferences(preferences)
  state$custom_palettes <- sanitized$customPalettes
  state$app_settings <- sanitized$appSettings
  invisible(sanitized)
}

register_user_preferences_sync <- function(state, input, session) {
  preferences_ready <- shiny::reactiveVal(FALSE)

  session$onFlushed(function() {
    url_search <- shiny::isolate(session$clientData$url_search)
    app_settings <- shiny::isolate(state$app_settings)
    custom_palettes <- shiny::isolate(state$custom_palettes)

    session$sendCustomMessage(
      "userPreferences:init",
      list(
        userId = resolve_browser_user_id(session$user, url_search),
        defaults = browser_preferences_from_state(app_settings, custom_palettes)
      )
    )
  }, once = TRUE)

  shiny::observeEvent(input$client_user_preferences_loaded, {
    payload <- input$client_user_preferences_loaded

    if (is.null(payload) || !is.list(payload)) {
      preferences_ready(TRUE)
      return()
    }

    state$current_user_id <- resolve_browser_user_id(payload$userId, NULL)
    apply_user_preferences_to_state(state, payload$preferences)
    preferences_ready(TRUE)
  }, ignoreInit = FALSE)

  shiny::observeEvent(list(state$app_settings, state$custom_palettes, state$current_user_id), {
    if (!isTRUE(preferences_ready())) {
      return()
    }

    if (isTRUE(state$skip_next_user_preferences_save)) {
      state$skip_next_user_preferences_save <- FALSE
      return()
    }

    session$sendCustomMessage(
      "userPreferences:save",
      list(
        userId = resolve_browser_user_id(state$current_user_id, NULL, fallback = resolve_browser_user_id(session$user, NULL)),
        preferences = browser_preferences_from_state(state$app_settings, state$custom_palettes),
        defaults = get_default_user_preferences()
      )
    )
  }, ignoreInit = TRUE)

  invisible(list(preferences_ready = preferences_ready))
}
