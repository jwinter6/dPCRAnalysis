is_twoddpcr_available <- function() {
  requireNamespace("twoddpcr", quietly = TRUE)
}

available_twoddpcr_channels <- function(df, exclude_reference = TRUE) {
  channels <- available_channel_names(df)

  if (!isTRUE(exclude_reference)) {
    return(channels)
  }

  non_reference <- channels[!toupper(channels) %in% c("REF", "REFERENCE")]
  if (length(non_reference) >= 2) {
    return(non_reference)
  }

  channels
}

default_twoddpcr_channel_pair <- function(df) {
  channels <- available_twoddpcr_channels(df, exclude_reference = TRUE)

  if (length(channels) < 2) {
    channels <- available_channel_names(df)
  }

  c(
    x = if (length(channels) >= 1) channels[[1]] else "",
    y = if (length(channels) >= 2) channels[[2]] else if (length(channels) >= 1) channels[[1]] else ""
  )
}

default_twoddpcr_abbrev <- function(channel_value, fallback = "Ch") {
  channel_value <- trimws(as.character(channel_value))
  channel_value <- channel_value[!is.na(channel_value) & channel_value != ""]

  if (length(channel_value) == 0) {
    return(fallback)
  }

  channel_value[[1]]
}

twoddpcr_abbrev_labels <- function(ch1_abbrev, ch2_abbrev) {
  c(
    paste0(ch1_abbrev, "-", ch2_abbrev, "-"),
    paste0(ch1_abbrev, "-", ch2_abbrev, "+"),
    paste0(ch1_abbrev, "+", ch2_abbrev, "-"),
    paste0(ch1_abbrev, "+", ch2_abbrev, "+")
  )
}

twoddpcr_method_label <- function(method_name) {
  switch(
    method_name,
    kmeans = "K-means Clustering",
    thresholds = "Thresholds",
    grid = "Grid",
    knn = "K-Nearest Neighbour",
    kmeansMahRain = "K-means Clustering + Mahalanobis Rain",
    thresholdsMahRain = "Thresholds + Mahalanobis Rain",
    gridMahRain = "Grid + Mahalanobis Rain",
    knnMahRain = "K-Nearest Neighbour + Mahalanobis Rain",
    kmeansSdRain = "K-means Clustering + Standard Deviation Rain",
    thresholdsSdRain = "Thresholds + Standard Deviation Rain",
    gridSdRain = "Grid + Standard Deviation Rain",
    knnSdRain = "K-Nearest Neighbour + Standard Deviation Rain",
    method_name
  )
}

twoddpcr_rain_label <- function(method_name) {
  if (is.null(method_name) || !nzchar(method_name)) {
    return("None")
  }

  if (grepl("MahRain$", method_name)) {
    return("Mahalanobis")
  }

  if (grepl("SdRain$", method_name)) {
    return("Standard deviation")
  }

  "None"
}

twoddpcr_base_method <- function(method_name) {
  if (is.null(method_name) || !nzchar(method_name)) {
    return("")
  }

  sub("(MahRain|SdRain)$", "", method_name)
}

twoddpcr_class_codes <- function() {
  c("NN", "NP", "PN", "PP")
}

available_twoddpcr_methods <- function(plate) {
  if (!is_twoddpcr_available() || is.null(plate) || length(plate) == 0) {
    return(c("Keine Klassifikation verfügbar" = ""))
  }

  methods <- twoddpcr::commonClassificationMethod(plate)
  methods <- methods[methods != "None"]

  if (length(methods) == 0) {
    return(c("Keine Klassifikation verfügbar" = ""))
  }

  stats::setNames(methods, vapply(methods, twoddpcr_method_label, character(1)))
}

compute_twoddpcr_plot_limits <- function(pair_data, pad_fraction = 0.08, min_padding = 1) {
  if (is.null(pair_data) || !is.data.frame(pair_data) || nrow(pair_data) == 0) {
    return(list(x = c(0, 1), y = c(0, 1)))
  }

  calc_axis_limits <- function(values) {
    finite_values <- values[is.finite(values)]

    if (length(finite_values) == 0) {
      return(c(0, 1))
    }

    value_range <- range(finite_values)
    spread <- diff(value_range)
    padding <- if (spread <= 0) {
      max(min_padding, abs(value_range[[1]]) * 0.1 + min_padding)
    } else {
      max(min_padding, spread * pad_fraction)
    }

    c(value_range[[1]] - padding, value_range[[2]] + padding)
  }

  list(
    x = calc_axis_limits(pair_data$x_value),
    y = calc_axis_limits(pair_data$y_value)
  )
}

compute_twoddpcr_threshold_defaults <- function(pair_data) {
  if (is.null(pair_data) || !is.data.frame(pair_data) || nrow(pair_data) == 0) {
    return(list(ch1_threshold = NA_real_, ch2_threshold = NA_real_))
  }

  list(
    ch1_threshold = compute_auto_threshold(pair_data$y_value),
    ch2_threshold = compute_auto_threshold(pair_data$x_value)
  )
}

compute_twoddpcr_grid_defaults <- function(pair_data) {
  thresholds <- compute_twoddpcr_threshold_defaults(pair_data)

  if (!is.finite(thresholds$ch1_threshold) || !is.finite(thresholds$ch2_threshold)) {
    return(list(
      ch1NNThreshold = 6500,
      ch2NNThreshold = 1900,
      ch1NPThreshold = 6500,
      ch2NPThreshold = 5000,
      ch1PNThreshold = 10000,
      ch2PNThreshold = 2900,
      ch1PPThreshold = 7500,
      ch2PPThreshold = 5000
    ))
  }

  list(
    ch1NNThreshold = thresholds$ch1_threshold * 0.9,
    ch2NNThreshold = thresholds$ch2_threshold * 0.9,
    ch1NPThreshold = thresholds$ch1_threshold * 0.9,
    ch2NPThreshold = thresholds$ch2_threshold * 1.1,
    ch1PNThreshold = thresholds$ch1_threshold * 1.1,
    ch2PNThreshold = thresholds$ch2_threshold * 0.9,
    ch1PPThreshold = thresholds$ch1_threshold * 1.05,
    ch2PPThreshold = thresholds$ch2_threshold * 1.05
  )
}

normalize_twoddpcr_grid_thresholds <- function(grid_thresholds, fallback = NULL) {
  expected_names <- c(
    "ch1NNThreshold", "ch2NNThreshold",
    "ch1NPThreshold", "ch2NPThreshold",
    "ch1PNThreshold", "ch2PNThreshold",
    "ch1PPThreshold", "ch2PPThreshold"
  )

  if (is.null(fallback)) {
    fallback <- compute_twoddpcr_grid_defaults(tibble::tibble())
  }

  values <- as.list(fallback)
  if (!is.null(grid_thresholds)) {
    input_values <- as.list(grid_thresholds)
    for (name in intersect(names(input_values), expected_names)) {
      candidate <- suppressWarnings(as.numeric(input_values[[name]]))
      if (length(candidate) == 1 && is.finite(candidate)) {
        values[[name]] <- candidate
      }
    }
  }

  values[expected_names]
}

build_twoddpcr_training_data <- function(plate,
                                         source = c("grid", "results"),
                                         grid_thresholds = NULL,
                                         c_method = NULL) {
  if (!is_twoddpcr_available()) {
    stop("Das Paket 'twoddpcr' ist nicht verfügbar.")
  }

  source <- match.arg(source)

  if (source == "grid") {
    gt <- normalize_twoddpcr_grid_thresholds(grid_thresholds)
    classified <- twoddpcr::gridClassify(
      plate,
      ch1NNThreshold = gt$ch1NNThreshold,
      ch2NNThreshold = gt$ch2NNThreshold,
      ch1NPThreshold = gt$ch1NPThreshold,
      ch2NPThreshold = gt$ch2NPThreshold,
      ch1PNThreshold = gt$ch1PNThreshold,
      ch2PNThreshold = gt$ch2PNThreshold,
      ch1PPThreshold = gt$ch1PPThreshold,
      ch2PPThreshold = gt$ch2PPThreshold,
      classMethodLabel = "class"
    )
    training_rows <- twoddpcr::removeDropletClasses(classified, "class")
  } else {
    if (is.null(c_method) || !nzchar(c_method)) {
      stop("Für Trainingsdaten aus Results muss eine Klassifikationsmethode angegeben werden.")
    }

    training_rows <- twoddpcr::removeDropletClasses(plate, c_method)
  }

  training_df <- dplyr::bind_rows(training_rows)
  if (nrow(training_df) == 0) {
    stop("Es konnten keine Trainingsdaten erzeugt werden.")
  }

  class_column <- setdiff(names(training_df), c("Ch1.Amplitude", "Ch2.Amplitude"))
  class_column <- class_column[[1]]

  if (is.null(class_column) || !nzchar(class_column)) {
    stop("Die Trainingsdaten enthalten keine Klassifikationsspalte.")
  }

  training_df <- training_df |>
    dplyr::transmute(
      Ch1.Amplitude = .data[["Ch1.Amplitude"]],
      Ch2.Amplitude = .data[["Ch2.Amplitude"]],
      class = factor(.data[[class_column]], levels = twoddpcr_class_codes())
    ) |>
    dplyr::filter(!is.na(class))

  if (nrow(training_df) == 0) {
    stop("Die Trainingsdaten enthalten keine klassifizierten Droplets.")
  }

  training_df
}

normalize_twoddpcr_cluster_parameters <- function(values, default_value) {
  classes <- twoddpcr_class_codes()

  if (is.null(values)) {
    return(stats::setNames(as.list(rep(default_value, length(classes))), classes))
  }

  if (is.list(values)) {
    input_values <- values
  } else {
    input_values <- as.list(values)
    if (length(input_values) == length(classes) && is.null(names(input_values))) {
      names(input_values) <- classes
    }
  }

  out <- stats::setNames(as.list(rep(default_value, length(classes))), classes)
  for (class_name in classes) {
    candidate <- suppressWarnings(as.numeric(input_values[[class_name]]))
    if (length(candidate) == 1 && is.finite(candidate)) {
      out[[class_name]] <- candidate
    }
  }

  out
}

build_twoddpcr_grid_rectangles <- function(grid_thresholds) {
  gt <- normalize_twoddpcr_grid_thresholds(grid_thresholds)

  list(
    NN = data.frame(xmin = -Inf, xmax = gt$ch2NNThreshold, ymin = -Inf, ymax = gt$ch1NNThreshold),
    NP = data.frame(xmin = gt$ch2NPThreshold, xmax = Inf, ymin = -Inf, ymax = gt$ch1NPThreshold),
    PN = data.frame(xmin = -Inf, xmax = gt$ch2PNThreshold, ymin = gt$ch1PNThreshold, ymax = Inf),
    PP = data.frame(xmin = gt$ch2PPThreshold, xmax = Inf, ymin = gt$ch1PPThreshold, ymax = Inf)
  )
}

adapt_dpcr_to_twoddpcr <- function(df,
                                   samples = NULL,
                                   channel_x = NULL,
                                   channel_y = NULL,
                                   plate_filter = NULL,
                                   exclude_invalid = TRUE) {
  if (!is_twoddpcr_available()) {
    return(list(
      ok = FALSE,
      message = "Das Paket 'twoddpcr' ist nicht verfügbar.",
      plate = twoddpcr::ddpcrPlate(),
      pair_data = tibble::tibble(),
      well_map = tibble::tibble()
    ))
  }

  data <- coerce_dpcr_schema(df)
  data <- dplyr::filter(data, !is.na(sample) & trimws(sample) != "")

  if (!is.null(plate_filter) && nzchar(plate_filter) && plate_filter != "__all__") {
    data <- dplyr::filter(data, plate_name == plate_filter | plate_id == plate_filter)
  }

  if (!is.null(samples) && length(samples) > 0) {
    data <- dplyr::filter(data, sample %in% samples)
  }

  if (isTRUE(exclude_invalid)) {
    data <- dplyr::filter(data, is.na(invalid_partition) | !invalid_partition)
  }

  channel_pair <- default_twoddpcr_channel_pair(data)
  if (is.null(channel_x) || !nzchar(channel_x)) {
    channel_x <- channel_pair[["x"]]
  }
  if (is.null(channel_y) || !nzchar(channel_y)) {
    channel_y <- channel_pair[["y"]]
  }

  if (!nzchar(channel_x) || !nzchar(channel_y) || identical(channel_x, channel_y)) {
    return(list(
      ok = FALSE,
      message = "Bitte wählen Sie zwei unterschiedliche Kanäle für die Detailanalyse.",
      plate = twoddpcr::ddpcrPlate(),
      pair_data = tibble::tibble(),
      well_map = tibble::tibble(),
      channel_x = channel_x,
      channel_y = channel_y
    ))
  }

  pair_data <- build_plot_b_data(data, channel_x = channel_x, channel_y = channel_y)
  pair_data <- dplyr::filter(pair_data, is.finite(x_value), is.finite(y_value))

  if (nrow(pair_data) == 0) {
    return(list(
      ok = FALSE,
      message = "Keine vollständigen Zwei-Kanal-Daten für die gewählten Samples/Kanäle gefunden.",
      plate = twoddpcr::ddpcrPlate(),
      pair_data = pair_data,
      well_map = tibble::tibble(),
      channel_x = channel_x,
      channel_y = channel_y
    ))
  }

  well_keys <- pair_data |>
    dplyr::distinct(plate_name, sample, well) |>
    dplyr::mutate(
      well_key = paste(plate_name, sample, well, sep = "__"),
      well_id = make.unique(gsub("[^[:alnum:]_.-]+", "_", well_key))
    )

  pair_data <- pair_data |>
    dplyr::left_join(well_keys, by = c("plate_name", "sample", "well")) |>
    dplyr::mutate(
      Ch1.Amplitude = y_value,
      Ch2.Amplitude = x_value
    )

  well_map <- pair_data |>
    dplyr::distinct(well_id, plate_name, plate_id, sample, well)

  wells <- split(pair_data[, c("Ch1.Amplitude", "Ch2.Amplitude")], pair_data$well_id)
  wells <- lapply(wells, function(x) {
    as.data.frame(x, check.names = FALSE)
  })

  list(
    ok = TRUE,
    message = "",
    plate = twoddpcr::ddpcrPlate(wells = wells),
    pair_data = pair_data,
    well_map = well_map,
    channel_x = channel_x,
    channel_y = channel_y,
    plot_limits = compute_twoddpcr_plot_limits(pair_data),
    thresholds = compute_twoddpcr_threshold_defaults(pair_data)
  )
}

configure_twoddpcr_droplet_volume <- function(droplet_volume_nl = 0.85) {
  if (!is_twoddpcr_available()) {
    return(invisible(FALSE))
  }

  droplet_volume_nl <- suppressWarnings(as.numeric(droplet_volume_nl))
  if (length(droplet_volume_nl) != 1 || is.na(droplet_volume_nl) || !is.finite(droplet_volume_nl) || droplet_volume_nl <= 0) {
    droplet_volume_nl <- TWODDPCR_DROPLET_VOLUME_DEFAULT
  }

  twoddpcr::setDropletVolume(volume = droplet_volume_nl * 0.001)
  invisible(TRUE)
}

classify_twoddpcr_plate <- function(plate,
                                    method = c("kmeans", "thresholds", "grid", "knn"),
                                    kmeans_centers = 4L,
                                    ch1_threshold = NA_real_,
                                    ch2_threshold = NA_real_,
                                    grid_thresholds = NULL,
                                    knn_training_data = NULL,
                                    knn_k = TWODDPCR_KNN_K_DEFAULT,
                                    knn_prob = TWODDPCR_KNN_PROB_DEFAULT) {
  if (!is_twoddpcr_available()) {
    stop("Das Paket 'twoddpcr' ist nicht verfügbar.")
  }

  method <- match.arg(method)
  out_plate <- plate

  if (method == "kmeans") {
    centres <- suppressWarnings(as.integer(kmeans_centers))
    if (!is.finite(centres) || centres < 1L || centres > 4L) {
      centres <- 4L
    }

    out_plate <- twoddpcr::kmeansClassify(out_plate, centres = centres)
  } else if (method == "thresholds") {
    ch1_threshold <- suppressWarnings(as.numeric(ch1_threshold))
    ch2_threshold <- suppressWarnings(as.numeric(ch2_threshold))

    if (!is.finite(ch1_threshold) || !is.finite(ch2_threshold)) {
      stop("Für 'Thresholds' müssen gültige Ch1/Ch2-Thresholds gesetzt sein.")
    }

    out_plate <- twoddpcr::thresholdClassify(
      out_plate,
      ch1Threshold = ch1_threshold,
      ch2Threshold = ch2_threshold
    )
  } else if (method == "grid") {
    gt <- normalize_twoddpcr_grid_thresholds(grid_thresholds)

    out_plate <- twoddpcr::gridClassify(
      out_plate,
      ch1NNThreshold = gt$ch1NNThreshold,
      ch2NNThreshold = gt$ch2NNThreshold,
      ch1NPThreshold = gt$ch1NPThreshold,
      ch2NPThreshold = gt$ch2NPThreshold,
      ch1PNThreshold = gt$ch1PNThreshold,
      ch2PNThreshold = gt$ch2PNThreshold,
      ch1PPThreshold = gt$ch1PPThreshold,
      ch2PPThreshold = gt$ch2PPThreshold,
      classMethodLabel = "grid"
    )
  } else {
    if (is.null(knn_training_data) || !is.data.frame(knn_training_data) || nrow(knn_training_data) == 0) {
      stop("Für 'K-Nearest Neighbour' werden Trainingsdaten benötigt.")
    }

    required_columns <- c("Ch1.Amplitude", "Ch2.Amplitude", "class")
    if (!all(required_columns %in% names(knn_training_data))) {
      stop("Trainingsdaten für 'K-Nearest Neighbour' müssen Ch1.Amplitude, Ch2.Amplitude und class enthalten.")
    }

    k <- suppressWarnings(as.integer(knn_k))
    if (!is.finite(k) || k < 1L) {
      k <- TWODDPCR_KNN_K_DEFAULT
    }

    prob <- suppressWarnings(as.numeric(knn_prob))
    if (length(prob) != 1 || is.na(prob) || !is.finite(prob)) {
      prob <- TWODDPCR_KNN_PROB_DEFAULT
    }
    prob <- min(max(prob, 0), 1)

    out_plate <- twoddpcr::knnClassify(
      out_plate,
      trainData = knn_training_data[, c("Ch1.Amplitude", "Ch2.Amplitude"), drop = FALSE],
      cl = knn_training_data$class,
      k = k,
      prob = prob
    )
  }

  list(
    plate = out_plate,
    base_method = method
  )
}

apply_twoddpcr_rain_to_plate <- function(plate,
                                         base_method,
                                         rain_type = c("none", "mahalanobis", "sd"),
                                         mahalanobis_max = 30,
                                         sd_error = 5) {
  if (!is_twoddpcr_available()) {
    stop("Das Paket 'twoddpcr' ist nicht verfügbar.")
  }

  rain_type <- match.arg(rain_type)

  if (rain_type == "none") {
    return(list(plate = plate, method = base_method))
  }

  if (rain_type == "mahalanobis") {
    max_distances <- normalize_twoddpcr_cluster_parameters(mahalanobis_max, TWODDPCR_MAHALANOBIS_MAX_DEFAULT)
    plate <- twoddpcr::mahalanobisRain(plate, cMethod = base_method, maxDistances = max_distances)
    return(list(plate = plate, method = paste0(base_method, "MahRain")))
  }

  error_levels <- normalize_twoddpcr_cluster_parameters(sd_error, TWODDPCR_SD_ERROR_DEFAULT)
  plate <- twoddpcr::sdRain(plate, cMethod = base_method, errorLevel = error_levels)
  list(plate = plate, method = paste0(base_method, "SdRain"))
}

build_twoddpcr_classify_plot <- function(plate,
                                         plot_limits,
                                         ch1_label,
                                         ch2_label,
                                         binwidth = 50,
                                         method = c("kmeans", "thresholds", "grid", "knn"),
                                         ch1_threshold = NA_real_,
                                         ch2_threshold = NA_real_,
                                         grid_thresholds = NULL) {
  method <- match.arg(method)

  p <- twoddpcr::heatPlot(
    plate,
    ch1Label = ch1_label,
    ch2Label = ch2_label,
    binwidth = binwidth,
    plotLimits = plot_limits
  )

  if (method == "thresholds" && is.finite(ch1_threshold) && is.finite(ch2_threshold)) {
    p <- p +
      ggplot2::geom_hline(yintercept = ch1_threshold, linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = ch2_threshold, linewidth = 0.8)
  } else if (method == "grid") {
    rectangles <- build_twoddpcr_grid_rectangles(grid_thresholds)
    p <- p +
      ggplot2::geom_rect(data = rectangles$NN, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE, fill = "green", colour = "green4", alpha = 0.18) +
      ggplot2::geom_rect(data = rectangles$NP, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE, fill = "green", colour = "green4", alpha = 0.18) +
      ggplot2::geom_rect(data = rectangles$PN, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE, fill = "green", colour = "green4", alpha = 0.18) +
      ggplot2::geom_rect(data = rectangles$PP, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE, fill = "green", colour = "green4", alpha = 0.18)
  }

  p
}

build_twoddpcr_results_plot <- function(plate,
                                        c_method,
                                        plot_limits,
                                        ch1_label,
                                        ch2_label,
                                        ch1_abbrev,
                                        ch2_abbrev,
                                        show_thresholds = FALSE,
                                        ch1_threshold = NA_real_,
                                        ch2_threshold = NA_real_,
                                        show_final_centres = FALSE,
                                        grid_thresholds = NULL) {
  labels <- c(twoddpcr_abbrev_labels(ch1_abbrev, ch2_abbrev), "Rain", "N/A")

  final_centres <- NULL
  if (isTRUE(show_final_centres)) {
    final_centres <- tryCatch(
      twoddpcr::combinedCentres(plate, cMethod = c_method),
      error = function(e) NULL
    )
  }

  p <- twoddpcr::dropletPlot(
    plate,
    ch1Label = ch1_label,
    ch2Label = ch2_label,
    cMethod = c_method,
    plotLimits = plot_limits,
    legendLabels = labels,
    finalCentres = final_centres
  )

  if (isTRUE(show_thresholds)) {
    base_method <- twoddpcr_base_method(c_method)

    if (identical(base_method, "thresholds") && is.finite(ch1_threshold) && is.finite(ch2_threshold)) {
      p <- p +
        ggplot2::geom_hline(yintercept = ch1_threshold, linewidth = 0.8) +
        ggplot2::geom_vline(xintercept = ch2_threshold, linewidth = 0.8)
    } else if (identical(base_method, "grid")) {
      rectangles <- build_twoddpcr_grid_rectangles(grid_thresholds)
      p <- p +
        ggplot2::geom_rect(data = rectangles$NN, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE, fill = NA, colour = "green4", linewidth = 0.7) +
        ggplot2::geom_rect(data = rectangles$NP, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE, fill = NA, colour = "green4", linewidth = 0.7) +
        ggplot2::geom_rect(data = rectangles$PN, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE, fill = NA, colour = "green4", linewidth = 0.7) +
        ggplot2::geom_rect(data = rectangles$PP, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE, fill = NA, colour = "green4", linewidth = 0.7)
    }
  }

  p
}

build_twoddpcr_summary_table <- function(plate,
                                         c_method,
                                         well_map,
                                         ch1_abbrev,
                                         ch2_abbrev,
                                         type = c("full", "results")) {
  type <- match.arg(type)

  summary_df <- twoddpcr::plateSummary(
    plate,
    cMethod = c_method,
    ch1Label = ch1_abbrev,
    ch2Label = ch2_abbrev
  )

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(tibble::tibble())
  }

  summary_df <- summary_df |>
    as.data.frame(check.names = FALSE)

  plate_well_ids <- names(plate)
  if (!is.null(plate_well_ids) && length(plate_well_ids) == nrow(summary_df)) {
    summary_df$well_id <- plate_well_ids
  } else {
    summary_df$well_id <- rownames(summary_df)
  }

  out <- summary_df |>
    tibble::as_tibble() |>
    dplyr::left_join(well_map, by = "well_id")

  if (type == "results") {
    keep_cols <- c("plate_name", "plate_id", "sample", "well", "PP", "PN", "NP", "NN", "AcceptedDroplets")
    return(dplyr::select(out, dplyr::any_of(keep_cols)))
  }

  leading_cols <- c("plate_name", "plate_id", "sample", "well")
  remaining_cols <- setdiff(names(out), c("well_id", leading_cols))
  dplyr::select(out, dplyr::any_of(leading_cols), dplyr::all_of(remaining_cols))
}
