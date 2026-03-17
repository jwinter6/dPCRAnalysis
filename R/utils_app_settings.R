get_builtin_palette_definitions <- function() {
  list(
    ggplot_default = list(
      id = "ggplot_default",
      label = "ggplot2 Standard",
      type = "ggplot_default",
      colors = NULL
    ),
    okabe_ito = list(
      id = "okabe_ito",
      label = "Okabe-Ito",
      type = "manual",
      colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
    ),
    dark2 = list(
      id = "dark2",
      label = "Dark2",
      type = "manual",
      colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
    ),
    set2 = list(
      id = "set2",
      label = "Set2",
      type = "manual",
      colors = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
    )
  )
}

sanitize_palette_name <- function(name) {
  name <- trimws(as.character(name))
  name <- name[!is.na(name) & nzchar(name)]

  if (length(name) == 0) {
    return("")
  }

  name[[1]]
}

parse_palette_color_input <- function(text) {
  text <- paste(text, collapse = "\n")
  parts <- unlist(strsplit(text, "[,;\\s]+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- toupper(parts[nzchar(parts)])
  unique(parts)
}

validate_hex_colors <- function(colors) {
  colors <- toupper(trimws(as.character(colors)))
  colors <- colors[!is.na(colors) & nzchar(colors)]

  if (length(colors) == 0) {
    return(list(valid = FALSE, invalid = character(), message = "Mindestens eine Farbe ist erforderlich."))
  }

  invalid <- colors[!grepl("^#[0-9A-F]{6}$", colors)]

  if (length(invalid) > 0) {
    return(list(
      valid = FALSE,
      invalid = unique(invalid),
      message = sprintf("Ungültige Hex-Farben: %s", paste(unique(invalid), collapse = ", "))
    ))
  }

  list(valid = TRUE, invalid = character(), message = NULL)
}

sanitize_custom_palettes <- function(custom_palettes = list()) {
  if (is.null(custom_palettes) || length(custom_palettes) == 0) {
    return(list())
  }

  palette_names <- trimws(as.character(names(custom_palettes)))
  palette_names[is.na(palette_names)] <- ""
  keep_idx <- nzchar(palette_names)

  if (!any(keep_idx)) {
    return(list())
  }

  palette_names <- palette_names[keep_idx]
  palette_values <- unname(custom_palettes[keep_idx])

  out <- vector("list", length(palette_values))
  names(out) <- palette_names

  for (i in seq_along(palette_values)) {
    colors <- parse_palette_color_input(palette_values[[i]])
    validation <- validate_hex_colors(colors)

    if (!isTRUE(validation$valid)) {
      next
    }

    out[[palette_names[[i]]]] <- colors
  }

  out <- out[!vapply(out, is.null, logical(1))]

  if (length(out) == 0) {
    return(list())
  }

  out
}

custom_palette_id <- function(name) {
  paste0("custom::", sanitize_palette_name(name))
}

custom_palette_name_from_id <- function(palette_id) {
  palette_id <- as.character(palette_id)
  if (!length(palette_id) || is.na(palette_id[[1]])) {
    return(NULL)
  }

  palette_id <- palette_id[[1]]
  prefix <- "custom::"

  if (!startsWith(palette_id, prefix)) {
    return(NULL)
  }

  substring(palette_id, nchar(prefix) + 1L)
}

palette_choice_labels <- function(custom_palettes = list()) {
  builtin <- get_builtin_palette_definitions()
  builtin_labels <- vapply(builtin, function(def) def$label, character(1))
  choices <- stats::setNames(names(builtin), builtin_labels)

  custom_palettes <- sanitize_custom_palettes(custom_palettes)
  if (length(custom_palettes) == 0) {
    return(choices)
  }

  custom_choices <- stats::setNames(
    vapply(names(custom_palettes), custom_palette_id, character(1)),
    vapply(names(custom_palettes), function(name) sprintf("Eigene Palette: %s", name), character(1))
  )

  c(choices, custom_choices)
}

get_palette_choice_label <- function(palette_id, custom_palettes = list()) {
  choices <- palette_choice_labels(custom_palettes)
  palette_id <- as.character(palette_id)
  palette_id <- palette_id[!is.na(palette_id) & nzchar(palette_id)]

  if (length(palette_id) == 0) {
    return(NULL)
  }

  match_idx <- match(palette_id[[1]], unname(choices))

  if (is.na(match_idx)) {
    return(NULL)
  }

  names(choices)[[match_idx]]
}

get_default_export_settings <- function() {
  list(
    width = PLOT_EXPORT_WIDTH_DEFAULT,
    height = PLOT_EXPORT_HEIGHT_DEFAULT,
    dpi = PLOT_EXPORT_DPI_DEFAULT,
    format = PLOT_EXPORT_FORMAT_DEFAULT
  )
}

get_default_app_settings <- function() {
  list(
    palette_id = APP_DEFAULT_PALETTE_ID,
    export = get_default_export_settings()
  )
}

sanitize_export_settings <- function(export_settings = list()) {
  defaults <- get_default_export_settings()
  export_settings <- utils::modifyList(defaults, export_settings)

  width <- suppressWarnings(as.numeric(export_settings$width))
  height <- suppressWarnings(as.numeric(export_settings$height))
  dpi <- suppressWarnings(as.numeric(export_settings$dpi))
  format <- tolower(trimws(as.character(export_settings$format)))

  if (!is.finite(width) || width <= 0) {
    width <- defaults$width
  }

  if (!is.finite(height) || height <= 0) {
    height <- defaults$height
  }

  if (!is.finite(dpi) || dpi <= 0) {
    dpi <- defaults$dpi
  }

  format <- format[!is.na(format) & nzchar(format)]
  format <- if (length(format) == 0) defaults$format else format[[1]]
  if (!format %in% PLOT_EXPORT_FORMATS) {
    format <- defaults$format
  }

  list(
    width = width,
    height = height,
    dpi = dpi,
    format = format
  )
}

sanitize_app_settings <- function(app_settings = NULL, custom_palettes = list()) {
  defaults <- get_default_app_settings()

  if (is.null(app_settings) || !is.list(app_settings)) {
    app_settings <- defaults
  } else {
    app_settings <- utils::modifyList(defaults, app_settings)
  }

  palette_choices <- unname(palette_choice_labels(custom_palettes))
  palette_id <- as.character(app_settings$palette_id)
  palette_id <- palette_id[!is.na(palette_id) & nzchar(palette_id)]
  palette_id <- if (length(palette_id) == 0) APP_DEFAULT_PALETTE_ID else palette_id[[1]]

  if (!palette_id %in% palette_choices) {
    palette_id <- APP_DEFAULT_PALETTE_ID
  }

  list(
    palette_id = palette_id,
    export = sanitize_export_settings(app_settings$export)
  )
}

get_palette_definition <- function(palette_id, custom_palettes = list()) {
  custom_palettes <- sanitize_custom_palettes(custom_palettes)
  builtin <- get_builtin_palette_definitions()
  palette_id <- sanitize_app_settings(list(palette_id = palette_id), custom_palettes)$palette_id

  if (palette_id %in% names(builtin)) {
    return(builtin[[palette_id]])
  }

  custom_name <- custom_palette_name_from_id(palette_id)
  if (!is.null(custom_name) && custom_name %in% names(custom_palettes)) {
    return(list(
      id = palette_id,
      label = custom_name,
      type = "manual",
      colors = custom_palettes[[custom_name]]
    ))
  }

  builtin[[APP_DEFAULT_PALETTE_ID]]
}

expand_palette_colors <- function(colors, n) {
  colors <- toupper(as.character(colors))
  colors <- colors[!is.na(colors) & nzchar(colors)]

  if (length(colors) == 0 || n <= 0) {
    return(character())
  }

  if (n <= length(colors)) {
    return(colors[seq_len(n)])
  }

  grDevices::colorRampPalette(colors)(n)
}

get_palette_preview_colors <- function(app_settings = get_default_app_settings(), custom_palettes = list(), n = 8L) {
  palette_info <- get_palette_definition(app_settings$palette_id, custom_palettes)
  n <- max(1L, as.integer(n))

  if (identical(palette_info$type, "ggplot_default")) {
    return(scales::hue_pal()(n))
  }

  expand_palette_colors(palette_info$colors, n)
}

build_manual_scale_values <- function(levels, app_settings = get_default_app_settings(), custom_palettes = list()) {
  levels <- as.character(levels)
  levels <- levels[!is.na(levels) & nzchar(levels)]

  if (length(levels) == 0) {
    return(character())
  }

  colors <- get_palette_preview_colors(app_settings, custom_palettes, n = length(levels))
  stats::setNames(colors, levels)
}

extract_mapped_column_name <- function(mapping) {
  if (is.null(mapping)) {
    return(NULL)
  }

  label <- rlang::as_label(mapping)
  label <- sub("^~", "", label)
  label <- trimws(label)

  if (grepl("^\\.data\\[\\[", label)) {
    label <- sub("^\\.data\\[\\[\"?([^\"\"]+)\"?\\]\\]$", "\\1", label)
  }

  label <- gsub("^`|`$", "", label)

  if (!nzchar(label)) {
    return(NULL)
  }

  label
}

extract_discrete_levels_from_mapping <- function(mapping, data) {
  column_name <- extract_mapped_column_name(mapping)

  if (is.null(column_name) || is.null(data) || !is.data.frame(data) || !column_name %in% names(data)) {
    return(character())
  }

  values <- data[[column_name]]
  if (!(is.character(values) || is.factor(values) || is.logical(values))) {
    return(character())
  }

  if (is.factor(values)) {
    present <- as.character(stats::na.omit(unique(values)))
    levels <- levels(values)
    return(levels[levels %in% present])
  }

  values <- trimws(as.character(values))
  unique(values[!is.na(values) & nzchar(values)])
}

collect_plot_discrete_levels <- function(plot, aesthetic = c("colour", "fill")) {
  aesthetic <- match.arg(aesthetic)
  keys <- if (identical(aesthetic, "colour")) c("colour", "color") else "fill"
  collected <- character()

  global_mapping <- plot$mapping
  global_data <- plot$data

  for (key in keys) {
    collected <- c(collected, extract_discrete_levels_from_mapping(global_mapping[[key]], global_data))
  }

  for (layer in plot$layers) {
    layer_data <- if (is.null(layer$data) || inherits(layer$data, "waiver")) global_data else layer$data

    for (key in keys) {
      layer_mapping <- layer$mapping[[key]]
      if (is.null(layer_mapping)) {
        layer_mapping <- global_mapping[[key]]
      }
      collected <- c(collected, extract_discrete_levels_from_mapping(layer_mapping, layer_data))
    }
  }

  unique(collected[!is.na(collected) & nzchar(collected)])
}

apply_app_discrete_scales <- function(plot, app_settings = get_default_app_settings(), custom_palettes = list()) {
  app_settings <- sanitize_app_settings(app_settings, custom_palettes)
  color_levels <- collect_plot_discrete_levels(plot, "colour")
  fill_levels <- collect_plot_discrete_levels(plot, "fill")
  palette_info <- get_palette_definition(app_settings$palette_id, custom_palettes)

  if (length(color_levels) > 0) {
    if (identical(palette_info$type, "ggplot_default")) {
      plot <- plot + ggplot2::scale_color_hue(drop = FALSE, na.value = "#ADB5BD")
    } else {
      plot <- plot + ggplot2::scale_color_manual(
        values = build_manual_scale_values(color_levels, app_settings, custom_palettes),
        drop = FALSE,
        na.value = "#ADB5BD"
      )
    }
  }

  if (length(fill_levels) > 0) {
    if (identical(palette_info$type, "ggplot_default")) {
      plot <- plot + ggplot2::scale_fill_hue(drop = FALSE, na.value = "#DEE2E6")
    } else {
      plot <- plot + ggplot2::scale_fill_manual(
        values = build_manual_scale_values(fill_levels, app_settings, custom_palettes),
        drop = FALSE,
        na.value = "#DEE2E6"
      )
    }
  }

  plot
}

get_palette_accent_colors <- function(app_settings = get_default_app_settings(), custom_palettes = list(), n = 4L) {
  colors <- get_palette_preview_colors(app_settings, custom_palettes, n = n)

  if (length(colors) < n) {
    colors <- rep(colors, length.out = n)
  }

  colors
}

plot_output_with_download <- function(ns, plot_id, height = "420px") {
  shiny::tagList(
    shiny::plotOutput(ns(plot_id), height = height),
    shiny::tags$div(
      class = "plot-download-container",
      shiny::downloadButton(ns(paste0(plot_id, "_download")), "Download Plot")
    )
  )
}

sanitize_filename_token <- function(value) {
  value <- trimws(as.character(value))
  value <- value[!is.na(value) & nzchar(value)]
  value <- if (length(value) == 0) "plot" else value[[1]]
  value <- tolower(value)
  value <- gsub("[^a-z0-9_-]+", "_", value)
  value <- gsub("_+", "_", value)
  value <- gsub("^_|_$", "", value)

  if (!nzchar(value)) {
    return("plot")
  }

  value
}

build_plot_download_spec <- function(plot_fn, export_settings_fn, filename_prefix = "plot") {
  list(
    filename = function() {
      export_settings <- sanitize_export_settings(export_settings_fn())
      paste0("plot_", sanitize_filename_token(filename_prefix), ".", export_settings$format)
    },
    content = function(file) {
      plot_obj <- plot_fn()

      if (is.null(plot_obj) || !inherits(plot_obj, "ggplot")) {
        stop("Kein ggplot2-Plot zum Export verfügbar.")
      }

      export_settings <- sanitize_export_settings(export_settings_fn())
      ggsave_args <- list(
        filename = file,
        plot = plot_obj,
        width = export_settings$width,
        height = export_settings$height,
        units = "in",
        device = export_settings$format
      )

      if (export_settings$format %in% c("png", "jpeg")) {
        ggsave_args$dpi <- export_settings$dpi
      }

      do.call(ggplot2::ggsave, ggsave_args)
      invisible(file)
    }
  )
}

register_plot_download <- function(output, output_id, plot_fn, export_settings_fn, filename_prefix = "plot") {
  spec <- build_plot_download_spec(
    plot_fn = plot_fn,
    export_settings_fn = export_settings_fn,
    filename_prefix = filename_prefix
  )

  output[[output_id]] <- shiny::downloadHandler(
    filename = spec$filename,
    content = spec$content
  )

  invisible(spec)
}
