infer_positive_partition <- function(positive_control, rfu, threshold) {
  positive_control <- parse_logical_like(positive_control)

  positive_from_signal <- ifelse(
    !is.na(rfu) & !is.na(threshold),
    rfu >= threshold,
    NA
  )

  ifelse(!is.na(positive_control), positive_control, positive_from_signal)
}

classify_rain_partition <- function(rfu, threshold, band_fraction = 0.10) {
  band_fraction <- suppressWarnings(as.numeric(band_fraction))
  if (is.na(band_fraction) || band_fraction <= 0) {
    band_fraction <- 0.10
  }

  has_values <- !is.na(rfu) & !is.na(threshold)
  out <- rep(NA, length(rfu))

  threshold_band <- abs(threshold) * band_fraction
  out[has_values] <- abs(rfu[has_values] - threshold[has_values]) <= threshold_band[has_values]

  as.logical(out)
}

clopper_pearson_ci <- function(x, n, conf_level = 0.95) {
  alpha <- 1 - conf_level

  lower <- ifelse(
    n <= 0,
    NA_real_,
    ifelse(x <= 0, 0, stats::qbeta(alpha / 2, x, n - x + 1))
  )

  upper <- ifelse(
    n <= 0,
    NA_real_,
    ifelse(x >= n, 1, stats::qbeta(1 - alpha / 2, x + 1, n - x))
  )

  list(lower = lower, upper = upper)
}

fraction_to_lambda <- function(p) {
  p <- pmax(pmin(p, 1), 0)
  lambda <- -log1p(-p)
  lambda[p >= 1] <- Inf
  lambda
}

summarize_quality_qc <- function(
    df,
    conf_level = 0.95,
    rain_band_fraction = 0.10,
    min_accepted = 10000,
    rain_limit_fraction = 0.025) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  conf_level <- suppressWarnings(as.numeric(conf_level))
  if (is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    conf_level <- 0.95
  }

  min_accepted <- suppressWarnings(as.numeric(min_accepted))
  if (is.na(min_accepted) || min_accepted < 1) {
    min_accepted <- 10000
  }

  rain_limit_fraction <- suppressWarnings(as.numeric(rain_limit_fraction))
  if (is.na(rain_limit_fraction) || rain_limit_fraction < 0) {
    rain_limit_fraction <- 0.025
  }

  metrics_base <- df |>
    dplyr::mutate(
      accepted_partition = is.na(invalid_partition) | !invalid_partition,
      positive_partition = infer_positive_partition(positive_control, rfu, threshold),
      rain_partition = classify_rain_partition(rfu, threshold, band_fraction = rain_band_fraction)
    ) |>
    dplyr::group_by(plate_name, plate_id, well, sample, channel) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_accepted = sum(accepted_partition, na.rm = TRUE),
      n_positive = sum(positive_partition & accepted_partition, na.rm = TRUE),
      rain_n = sum(rain_partition & accepted_partition, na.rm = TRUE),
      rfu_mittel = mean(rfu[accepted_partition], na.rm = TRUE),
      rfu_sd = stats::sd(rfu[accepted_partition], na.rm = TRUE),
      threshold_mittel = mean(threshold[accepted_partition], na.rm = TRUE),
      .groups = "drop"
    )

  metrics_base <- dplyr::mutate(
    metrics_base,
    positive_fraction = dplyr::if_else(n_accepted > 0, n_positive / n_accepted, NA_real_),
    rain_fraction = dplyr::if_else(n_accepted > 0, rain_n / n_accepted, NA_real_)
  )

  ci <- clopper_pearson_ci(metrics_base$n_positive, metrics_base$n_accepted, conf_level = conf_level)

  metrics <- dplyr::mutate(
    metrics_base,
    qc_id = paste(plate_id, well, sample, channel, sep = "|"),
    positive_ci_low = ci$lower,
    positive_ci_high = ci$upper,
    lambda = fraction_to_lambda(positive_fraction),
    lambda_ci_low = fraction_to_lambda(positive_ci_low),
    lambda_ci_high = fraction_to_lambda(positive_ci_high),
    positive_percent = 100 * positive_fraction,
    rain_percent = 100 * rain_fraction,
    accepted_qc = dplyr::if_else(n_accepted < min_accepted, "kritisch", "ok"),
    rain_qc = dplyr::if_else(rain_fraction > rain_limit_fraction, "kritisch", "ok"),
    qc_status = dplyr::if_else(accepted_qc == "ok" & rain_qc == "ok", "ok", "kritisch")
  )

  metrics
}

is_dpcr_density_available <- function() {
  requireNamespace("dpcR", quietly = TRUE) &&
    exists("dpcr_density", where = asNamespace("dpcR"), inherits = FALSE)
}

run_dpcr_density <- function(
    k,
    n,
    average = TRUE,
    method = "wilson",
    conf_level = 0.95,
    plot = FALSE,
    bars = FALSE,
    ...) {
  if (!is_dpcr_density_available()) {
    stop("dpcR::dpcr_density ist nicht verfügbar.")
  }

  k <- suppressWarnings(as.numeric(k))
  k <- k[is.finite(k)]
  if (length(k) == 0) {
    stop("Ungültiger Wert für k.")
  }
  k <- k[[1]]

  n <- suppressWarnings(as.numeric(n))
  n <- n[is.finite(n)]
  if (length(n) == 0) {
    stop("Ungültiger Wert für n.")
  }
  n <- n[[1]]

  conf_level <- suppressWarnings(as.numeric(conf_level))
  conf_level <- conf_level[is.finite(conf_level)]
  conf_level <- if (length(conf_level) == 0) 0.95 else conf_level[[1]]

  method <- as.character(method)
  method <- method[!is.na(method) & nzchar(method)]
  method <- if (length(method) == 0) "wilson" else method[[1]]

  average <- as.logical(average)
  average <- average[!is.na(average)]
  average <- if (length(average) == 0) TRUE else average[[1]]

  bars <- as.logical(bars)
  bars <- bars[!is.na(bars)]
  bars <- if (length(bars) == 0) FALSE else bars[[1]]

  if (is.na(k) || is.na(n) || n <= 0 || k < 0) {
    stop("Ungültige Eingaben für dpcR::dpcr_density (k/n).")
  }

  dpcR::dpcr_density(
    k = k,
    n = n,
    average = average,
    methods = method,
    conf.level = conf_level,
    plot = isTRUE(plot),
    bars = bars,
    ...
  )
}

extract_dpcr_density_interval <- function(density_result, average = TRUE) {
  if (is.null(density_result) || !is.data.frame(density_result) || nrow(density_result) == 0) {
    return(tibble::tibble())
  }

  estimate_col <- if (isTRUE(average)) {
    if ("lambda" %in% names(density_result)) "lambda" else names(density_result)[[1]]
  } else {
    if ("mean" %in% names(density_result)) "mean" else names(density_result)[[1]]
  }

  method <- if ("method" %in% names(density_result)) density_result$method[[1]] else NA_character_
  k <- if ("k" %in% names(density_result)) density_result$k[[1]] else NA_real_
  n <- if ("n" %in% names(density_result)) density_result$n[[1]] else NA_real_
  est <- if (estimate_col %in% names(density_result)) density_result[[estimate_col]][[1]] else NA_real_
  lower <- if ("lower" %in% names(density_result)) density_result$lower[[1]] else NA_real_
  upper <- if ("upper" %in% names(density_result)) density_result$upper[[1]] else NA_real_

  tibble::tibble(
    method = as.character(method),
    k = as.numeric(k),
    n = as.numeric(n),
    estimate = as.numeric(est),
    lower = as.numeric(lower),
    upper = as.numeric(upper)
  )
}

build_dpcr_density_plot <- function(
    k,
    n,
    average = TRUE,
    method = "wilson",
    conf_level = 0.95,
    bars = FALSE,
    title = "dpcR-Dichteplot",
    subtitle = NULL) {
  if (!is_dpcr_density_available()) {
    return(build_empty_plot("dpcR ist nicht verfügbar"))
  }

  ci <- run_dpcr_density(
    k = k,
    n = n,
    average = average,
    method = method,
    conf_level = conf_level,
    plot = FALSE,
    bars = bars
  )
  ci_df <- extract_dpcr_density_interval(ci, average = average)

  dens <- as.data.frame(dpcR:::dpcr_calculator(k = as.numeric(k)[1], n = as.numeric(n)[1], average = isTRUE(average)))
  names(dens) <- c("x", "density")

  if (nrow(dens) == 0 || nrow(ci_df) == 0) {
    return(build_empty_plot("Keine dpcR-Dichtewerte verfügbar"))
  }

  x_lab <- if (isTRUE(average)) "Moleküle pro Partition (λ)" else "Positive Partitionen"
  sub_txt <- if (!is.null(subtitle)) subtitle else sprintf("Methode: %s | %.0f%% CI", ci_df$method[[1]], 100 * conf_level)

  p <- ggplot2::ggplot(dens, ggplot2::aes(x = x, y = density)) +
    ggplot2::geom_area(fill = "#9ecae1", alpha = 0.45) +
    ggplot2::geom_line(color = "#24527a", linewidth = 0.9) +
    ggplot2::geom_rect(
      inherit.aes = FALSE,
      data = ci_df,
      ggplot2::aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
      fill = "#2b8a3e",
      alpha = 0.14
    ) +
    ggplot2::geom_vline(
      data = ci_df,
      ggplot2::aes(xintercept = estimate),
      color = "#2b8a3e",
      linewidth = 1.0
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sub_txt,
      x = x_lab,
      y = "Dichte"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(bars)) {
    p <- p + ggplot2::geom_segment(
      data = dens,
      ggplot2::aes(x = x, xend = x, y = 0, yend = density),
      inherit.aes = FALSE,
      color = "#6c757d",
      alpha = 0.35,
      linewidth = 0.35
    )
  }

  p
}

build_lambda_density_plot <- function(
    metrics,
    metric = c("lambda", "positive_fraction"),
    group_by = c("channel", "sample", "plate_name"),
    title = "Dichteplot",
    subtitle = "Verteilung pro Gruppe") {
  metric <- match.arg(metric)
  group_by <- match.arg(group_by)

  if (is.null(metrics) || !is.data.frame(metrics) || nrow(metrics) == 0) {
    return(build_empty_plot("Keine QC-Daten verfügbar"))
  }

  value_col <- if (metric == "lambda") "lambda" else "positive_percent"
  value_label <- if (metric == "lambda") "Lambda (λ)" else "Positive Partitionen (%)"

  plot_df <- metrics |>
    dplyr::mutate(value = .data[[value_col]]) |>
    dplyr::filter(!is.na(value) & is.finite(value))

  if (nrow(plot_df) < 2) {
    return(build_empty_plot("Zu wenige Werte für Dichteplot"))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = value,
      color = .data[[group_by]],
      fill = .data[[group_by]],
      text = paste0(
        "Well: ", well,
        "<br>Sample: ", sample,
        "<br>Kanal: ", channel,
        "<br>", value_label, ": ", signif(value, 4)
      )
    )
  ) +
    ggplot2::geom_density(alpha = 0.22, adjust = 1.1) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = value_label,
      y = "Dichte",
      color = group_by,
      fill = group_by
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_fill_brewer(palette = "Pastel2")
}

build_lambda_ci_plot <- function(
    metrics,
    metric = c("lambda", "positive_fraction"),
    title = "Konfidenzintervalle",
    subtitle = "Clopper-Pearson-basiert") {
  metric <- match.arg(metric)

  if (is.null(metrics) || !is.data.frame(metrics) || nrow(metrics) == 0) {
    return(build_empty_plot("Keine QC-Daten verfügbar"))
  }

  plot_df <- metrics

  if (metric == "lambda") {
    value <- plot_df$lambda
    lower <- plot_df$lambda_ci_low
    upper <- plot_df$lambda_ci_high
    y_label <- "Lambda (λ)"
  } else {
    value <- plot_df$positive_percent
    lower <- 100 * plot_df$positive_ci_low
    upper <- 100 * plot_df$positive_ci_high
    y_label <- "Positive Partitionen (%)"
  }

  plot_df <- dplyr::mutate(
    plot_df,
    ci_value = value,
    ci_low = lower,
    ci_high = upper,
    group_id = paste(well, channel, sep = " | ")
  ) |>
    dplyr::filter(!is.na(ci_value) & is.finite(ci_value))

  if (nrow(plot_df) == 0) {
    return(build_empty_plot("Keine gültigen CI-Werte verfügbar"))
  }

  plot_df <- dplyr::arrange(plot_df, dplyr::desc(ci_value))
  plot_df$group_id <- factor(plot_df$group_id, levels = rev(unique(plot_df$group_id)))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = group_id,
      y = ci_value,
      ymin = ci_low,
      ymax = ci_high,
      color = channel,
      text = paste0(
        "Well/Kanal: ", group_id,
        "<br>Sample: ", sample,
        "<br>Wert: ", signif(ci_value, 4),
        "<br>CI: [", signif(ci_low, 4), "; ", signif(ci_high, 4), "]",
        "<br>Akzeptierte Partitionen (n): ", n_accepted,
        "<br>Rain (%): ", signif(rain_percent, 3)
      )
    )
  ) +
    ggplot2::geom_pointrange(alpha = 0.85, size = 0.35) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Well | Kanal",
      y = y_label,
      color = "Kanal"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2")
}

summarize_qc_flags <- function(metrics, min_accepted = 10000, rain_limit_fraction = 0.025) {
  if (is.null(metrics) || !is.data.frame(metrics) || nrow(metrics) == 0) {
    return(list(
      wells_total = 0L,
      wells_low_accepted = 0L,
      wells_high_rain = 0L
    ))
  }

  wells_total <- nrow(metrics)
  wells_low_accepted <- sum(metrics$n_accepted < min_accepted, na.rm = TRUE)
  wells_high_rain <- sum(metrics$rain_fraction > rain_limit_fraction, na.rm = TRUE)

  list(
    wells_total = as.integer(wells_total),
    wells_low_accepted = as.integer(wells_low_accepted),
    wells_high_rain = as.integer(wells_high_rain)
  )
}
