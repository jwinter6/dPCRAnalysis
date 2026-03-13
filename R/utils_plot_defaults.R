get_default_scatter_settings <- function() {
  list(
    title = "Scatterplot: Partition vs RFU",
    subtitle = "Interaktive Übersicht pro Partition",
    x_col = "partition",
    y_col = "rfu",
    x_label = "Partition",
    y_label = "RFU",
    x_text_size = 12,
    y_text_size = 12,
    color_by = "channel",
    alpha = 0.6
  )
}

available_numeric_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(DPCR_PLOT_NUMERIC_CANDIDATES)
  }

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  intersect(c(DPCR_PLOT_NUMERIC_CANDIDATES, numeric_cols), names(df))
}

build_empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = message,
      size = 5,
      color = "#495057"
    )
}

build_scatter_plot <- function(df, settings) {
  if (nrow(df) == 0) {
    return(build_empty_plot("Keine Daten verfügbar"))
  }

  x_col <- settings$x_col
  y_col <- settings$y_col
  color_by <- settings$color_by

  if (!x_col %in% names(df) || !y_col %in% names(df)) {
    return(build_empty_plot("Ausgewählte Plot-Spalten nicht vorhanden"))
  }

  base_aes <- ggplot2::aes(
    x = .data[[x_col]],
    y = .data[[y_col]],
    text = paste0(
      "Sample: ", sample,
      "<br>Well: ", well,
      "<br>", x_col, ": ", .data[[x_col]],
      "<br>", y_col, ": ", .data[[y_col]]
    )
  )

  if (!is.null(color_by) && color_by != "none" && color_by %in% names(df)) {
    base_aes <- ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      color = .data[[color_by]],
      text = paste0(
        "Sample: ", sample,
        "<br>Well: ", well,
        "<br>", x_col, ": ", .data[[x_col]],
        "<br>", y_col, ": ", .data[[y_col]],
        "<br>", color_by, ": ", .data[[color_by]]
      )
    )
  }

  ggplot2::ggplot(df, base_aes) +
    ggplot2::geom_point(alpha = settings$alpha, size = 1.6) +
    ggplot2::labs(
      title = settings$title,
      subtitle = settings$subtitle,
      x = settings$x_label,
      y = settings$y_label,
      color = if (!is.null(color_by) && color_by != "none") color_by else NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(size = settings$x_text_size),
      axis.title.y = ggplot2::element_text(size = settings$y_text_size),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2")
}

build_histogram_plot <- function(df, settings) {
  if (nrow(df) == 0) {
    return(build_empty_plot("Keine Daten verfügbar"))
  }

  x_col <- settings$x_col
  if (!x_col %in% names(df)) {
    return(build_empty_plot("Ausgewählte Plot-Spalte nicht vorhanden"))
  }

  fill_col <- if (!is.null(settings$fill_by) && settings$fill_by %in% names(df)) settings$fill_by else NULL

  base_aes <- ggplot2::aes(x = .data[[x_col]])
  if (!is.null(fill_col) && fill_col != "none") {
    base_aes <- ggplot2::aes(x = .data[[x_col]], fill = .data[[fill_col]])
  }

  ggplot2::ggplot(df, base_aes) +
    ggplot2::geom_histogram(
      bins = settings$bins,
      alpha = 0.7,
      color = "white",
      position = "identity"
    ) +
    ggplot2::labs(
      title = settings$title,
      subtitle = settings$subtitle,
      x = settings$x_label,
      y = settings$y_label,
      fill = fill_col
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(size = settings$x_text_size),
      axis.title.y = ggplot2::element_text(size = settings$y_text_size),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::scale_fill_brewer(palette = "Set2")
}

prepare_interactive_plot_data <- function(df, max_points = 60000L) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(
      data = df,
      sampled = FALSE,
      original_n = 0L,
      used_n = 0L
    ))
  }

  n <- nrow(df)
  max_points <- as.integer(max_points)

  if (is.na(max_points) || max_points <= 0L) {
    max_points <- 60000L
  }

  if (n <= max_points) {
    return(list(
      data = df,
      sampled = FALSE,
      original_n = as.integer(n),
      used_n = as.integer(n)
    ))
  }

  # Deterministisches Downsampling über gleichmäßig verteilte Indizes.
  idx <- unique(round(seq(1, n, length.out = max_points)))
  sampled_df <- df[idx, , drop = FALSE]

  list(
    data = sampled_df,
    sampled = TRUE,
    original_n = as.integer(n),
    used_n = as.integer(nrow(sampled_df))
  )
}

make_interactive_plot <- function(ggplot_obj, tooltip = "text", use_webgl = TRUE) {
  p <- plotly::ggplotly(ggplot_obj, tooltip = tooltip)

  if (isTRUE(use_webgl)) {
    p <- plotly::toWebGL(p)
  }

  p
}
