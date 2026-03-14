sample_analysis_axis_choices <- function(df, include_categorical = TRUE) {
  df <- tibble::as_tibble(df)

  choices <- c(
    "Sample" = "sample",
    "Well" = "well",
    "Partition" = "partition",
    "Messpunktindex" = "sample_point_index",
    "Fluoreszenz" = "rfu",
    "Threshold" = "threshold",
    "Volumen" = "volume"
  )

  choices <- choices[choices %in% names(df)]

  if (!isTRUE(include_categorical)) {
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    choices <- choices[choices %in% numeric_cols]
  }

  choices
}

sample_analysis_prepare_data <- function(df) {
  df <- coerce_dpcr_schema(df)

  if (nrow(df) == 0) {
    df$sample_point_index <- integer()
    df$measurement_key <- character()
    return(df)
  }

  df |>
    dplyr::arrange(plate_id, sample, well, channel, partition) |>
    dplyr::group_by(plate_id, sample, well, channel) |>
    dplyr::mutate(sample_point_index = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      measurement_key = paste(plate_id, sample, well, partition, sep = "|")
    )
}

available_sample_names <- function(df) {
  samples <- unique(trimws(as.character(df$sample)))
  samples <- samples[!is.na(samples) & samples != ""]
  sort(samples)
}

available_channel_names <- function(df) {
  channels <- unique(trimws(as.character(df$channel)))
  channels <- channels[!is.na(channels) & channels != ""]
  sort(channels)
}

default_channel_axis_label <- function(channel_value, axis_fallback = "X") {
  channel_value <- trimws(as.character(channel_value))
  channel_value <- channel_value[!is.na(channel_value) & channel_value != ""]

  if (length(channel_value) == 0) {
    return(sprintf("Fluoreszenzkanal %s", axis_fallback))
  }

  sprintf("Fluoreszenzkanal %s", channel_value[[1]])
}

compute_otsu_threshold <- function(values, bins = 128L) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]

  if (length(vals) < 2 || length(unique(vals)) < 2) {
    return(NA_real_)
  }

  bins <- max(8L, min(as.integer(bins), length(unique(vals))))
  hist_info <- graphics::hist(vals, breaks = bins, plot = FALSE)

  probs <- hist_info$counts / sum(hist_info$counts)
  mids <- hist_info$mids
  omega <- cumsum(probs)
  mu <- cumsum(probs * mids)
  mu_total <- sum(probs * mids)

  sigma_between <- (mu_total * omega - mu)^2 / (omega * (1 - omega))
  sigma_between[!is.finite(sigma_between)] <- NA_real_

  if (all(is.na(sigma_between))) {
    return(NA_real_)
  }

  mids[[which.max(sigma_between)]]
}

compute_kmeans_threshold <- function(values) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]

  if (length(vals) < 2 || length(unique(vals)) < 2) {
    return(NA_real_)
  }

  fit <- tryCatch(
    stats::kmeans(vals, centers = 2, nstart = 10),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(NA_real_)
  }

  centers <- sort(as.numeric(fit$centers))
  mean(centers)
}

compute_auto_threshold <- function(values) {
  threshold <- compute_otsu_threshold(values)

  if (is.finite(threshold)) {
    return(threshold)
  }

  compute_kmeans_threshold(values)
}

build_sample_analysis_scatter_plot <- function(df, settings) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(build_empty_plot("Keine Daten verfuegbar"))
  }

  x_col <- settings$x_col
  y_col <- settings$y_col
  color_by <- settings$color_by

  if (!all(c(x_col, y_col) %in% names(df))) {
    return(build_empty_plot("Ausgewaehlte Plot-Spalten nicht vorhanden"))
  }

  plot_df <- tibble::as_tibble(df)
  plot_df$tooltip_text <- paste0(
    "Sample: ", plot_df$sample,
    "<br>Well: ", plot_df$well,
    "<br>Kanal: ", plot_df$channel,
    "<br>", x_col, ": ", plot_df[[x_col]],
    "<br>", y_col, ": ", signif(plot_df[[y_col]], 5)
  )

  if (!is.null(color_by) && color_by %in% names(plot_df) && color_by != "none") {
    plot_df$tooltip_text <- paste0(
      plot_df$tooltip_text,
      "<br>", color_by, ": ", plot_df[[color_by]]
    )
  }

  if ("threshold_status" %in% names(plot_df)) {
    plot_df$tooltip_text <- paste0(
      plot_df$tooltip_text,
      "<br>Threshold-Status: ", plot_df$threshold_status
    )
  }

  aes_base <- ggplot2::aes(
    x = .data[[x_col]],
    y = .data[[y_col]],
    text = tooltip_text
  )

  if (!is.null(color_by) && color_by %in% names(plot_df) && color_by != "none") {
    aes_base <- ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      color = .data[[color_by]],
      text = tooltip_text
    )
  }

  p <- ggplot2::ggplot(plot_df, aes_base)

  if (is.character(plot_df[[x_col]]) || is.factor(plot_df[[x_col]])) {
    p <- p + ggplot2::geom_point(
      alpha = settings$alpha,
      size = 1.5,
      position = ggplot2::position_jitter(width = 0.18, height = 0)
    )
  } else {
    p <- p + ggplot2::geom_point(alpha = settings$alpha, size = 1.5)
  }

  p <- p +
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
    )

  if (!is.null(color_by) && color_by %in% names(plot_df) && color_by != "none") {
    p <- p + ggplot2::scale_color_brewer(palette = "Dark2")
  }

  if (isTRUE(settings$threshold_enabled) &&
      !is.null(settings$threshold_y) &&
      is.finite(settings$threshold_y) &&
      is.numeric(plot_df[[y_col]])) {
    p <- p + ggplot2::geom_hline(
      yintercept = settings$threshold_y,
      linetype = "dashed",
      linewidth = 0.8,
      color = "#c92a2a"
    )
  }

  if (isTRUE(settings$threshold_enabled) &&
      !is.null(settings$threshold_x) &&
      is.finite(settings$threshold_x) &&
      is.numeric(plot_df[[x_col]])) {
    p <- p + ggplot2::geom_vline(
      xintercept = settings$threshold_x,
      linetype = "dashed",
      linewidth = 0.8,
      color = "#c92a2a"
    )
  }

  p
}

build_plot_a_data <- function(df, samples = NULL, channel = NULL) {
  data <- sample_analysis_prepare_data(df)

  if (!is.null(samples) && length(samples) > 0) {
    data <- dplyr::filter(data, sample %in% samples)
  }

  if (!is.null(channel) && nzchar(channel)) {
    data <- dplyr::filter(data, .data$channel == .env$channel)
  }

  data
}

build_plot_a_table <- function(df, y_col, threshold_value) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !y_col %in% names(df)) {
    return(tibble::tibble())
  }

  values <- suppressWarnings(as.numeric(df[[y_col]]))
  threshold_value <- suppressWarnings(as.numeric(threshold_value))

  tibble::as_tibble(df) |>
    dplyr::mutate(
      metric_value = values,
      oberhalb_threshold = metric_value >= threshold_value,
      unterhalb_threshold = metric_value < threshold_value
    ) |>
    dplyr::group_by(sample) |>
    dplyr::summarise(
      gesamt = sum(!is.na(metric_value)),
      signal_positiv = sum(oberhalb_threshold, na.rm = TRUE),
      signal_negativ = sum(unterhalb_threshold, na.rm = TRUE),
      positiv_prozent = dplyr::if_else(gesamt > 0, 100 * signal_positiv / gesamt, NA_real_),
      negativ_prozent = dplyr::if_else(gesamt > 0, 100 * signal_negativ / gesamt, NA_real_),
      .groups = "drop"
    )
}

build_plot_b_data <- function(df, channel_x, channel_y, samples = NULL) {
  data <- sample_analysis_prepare_data(df)

  if (!is.null(samples) && length(samples) > 0) {
    data <- dplyr::filter(data, sample %in% samples)
  }

  if (is.null(channel_x) || is.null(channel_y) || !nzchar(channel_x) || !nzchar(channel_y)) {
    return(tibble::tibble())
  }

  if (identical(channel_x, channel_y)) {
    return(tibble::tibble())
  }

  key_cols <- c("plate_name", "plate_id", "plate_type", "well", "sample", "partition")

  data_x <- data |>
    dplyr::filter(channel == channel_x) |>
    dplyr::select(
      dplyr::all_of(key_cols),
      x_value = rfu,
      x_threshold_default = threshold,
      x_color_channel = color_channel
    )

  data_y <- data |>
    dplyr::filter(channel == channel_y) |>
    dplyr::select(
      dplyr::all_of(key_cols),
      y_value = rfu,
      y_threshold_default = threshold,
      y_color_channel = color_channel
    )

  dplyr::inner_join(data_x, data_y, by = key_cols) |>
    dplyr::arrange(sample, well, partition) |>
    dplyr::group_by(sample) |>
    dplyr::mutate(sample_point_index = dplyr::row_number()) |>
    dplyr::ungroup()
}

build_plot_b_table <- function(df, threshold_x, threshold_y) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  threshold_x <- suppressWarnings(as.numeric(threshold_x))
  threshold_y <- suppressWarnings(as.numeric(threshold_y))

  tibble::as_tibble(df) |>
    dplyr::mutate(
      q_x_pos_y_pos = x_value >= threshold_x & y_value >= threshold_y,
      q_x_pos_y_neg = x_value >= threshold_x & y_value < threshold_y,
      q_x_neg_y_pos = x_value < threshold_x & y_value >= threshold_y,
      q_x_neg_y_neg = x_value < threshold_x & y_value < threshold_y
    ) |>
    dplyr::group_by(sample) |>
    dplyr::summarise(
      gesamt = dplyr::n(),
      quadrant_x_pos_y_pos = sum(q_x_pos_y_pos, na.rm = TRUE),
      quadrant_x_pos_y_neg = sum(q_x_pos_y_neg, na.rm = TRUE),
      quadrant_x_neg_y_pos = sum(q_x_neg_y_pos, na.rm = TRUE),
      quadrant_x_neg_y_neg = sum(q_x_neg_y_neg, na.rm = TRUE),
      .groups = "drop"
    )
}

build_sampling_info_message <- function(sampled_info, use_webgl = FALSE) {
  if (!isTRUE(sampled_info$sampled)) {
    return(NULL)
  }

  mode_label <- if (isTRUE(use_webgl)) {
    "Downsampling + WebGL"
  } else {
    "Downsampling"
  }

  shiny::tags$div(
    class = "alert alert-info",
    sprintf(
      "Interaktiver Plot optimiert: %s von %s Punkten dargestellt (%s).",
      format(sampled_info$used_n, big.mark = ".", scientific = FALSE),
      format(sampled_info$original_n, big.mark = ".", scientific = FALSE),
      mode_label
    )
  )
}
