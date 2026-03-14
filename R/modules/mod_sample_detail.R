mod_sample_detail_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinydashboard::box(
          width = NULL,
          title = "Globaler Filter",
          status = "warning",
          solidHeader = TRUE,
          shiny::selectInput(ns("detail_plate_select"), "Plate", choices = c("Bitte laden Sie zunächst Daten" = "")),
          shiny::selectizeInput(
            ns("detail_samples"),
            "Samples",
            choices = character(),
            selected = character(),
            multiple = TRUE
          ),
          shiny::selectInput(ns("detail_channel_x"), "Kanal für X-Achse", choices = character()),
          shiny::selectInput(ns("detail_channel_y"), "Kanal für Y-Achse", choices = character()),
          shiny::textInput(ns("detail_ch1_label"), "Label Y / Ch1", value = "Fluoreszenzkanal Y"),
          shiny::textInput(ns("detail_ch1_abbrev"), "Abkürzung Y / Ch1", value = "Y"),
          shiny::textInput(ns("detail_ch2_label"), "Label X / Ch2", value = "Fluoreszenzkanal X"),
          shiny::textInput(ns("detail_ch2_abbrev"), "Abkürzung X / Ch2", value = "X"),
          shiny::checkboxInput(ns("detail_exclude_invalid"), "Ungültige Partitionen ausschließen", value = TRUE),
          shiny::numericInput(
            ns("detail_droplet_volume"),
            "Droplet/Partition-Volumen (nl)",
            value = TWODDPCR_DROPLET_VOLUME_DEFAULT,
            min = 0.01,
            step = 0.01
          )
        )
      ),
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = NULL,
          title = "Adapter-Status",
          status = "info",
          solidHeader = TRUE,
          shiny::uiOutput(ns("twoddpcr_availability")),
          shiny::uiOutput(ns("adapter_status_ui"))
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Classify",
        status = "primary",
        solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::selectInput(
              ns("classify_mode"),
              "Klassifikationsmodus",
              choices = c(
                "K-means Clustering" = "kmeans",
                "Thresholds" = "thresholds",
                "Grid" = "grid",
                "K-Nearest Neighbour" = "knn"
              ),
              selected = "kmeans"
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'kmeans'", ns("classify_mode")),
              shiny::numericInput(ns("kmeans_centers"), "Anzahl Zentren", value = 4, min = 1, max = 4, step = 1)
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'thresholds'", ns("classify_mode")),
              shiny::numericInput(ns("detail_ch1_threshold"), "Threshold Y / Ch1", value = NA_real_, step = 0.1),
              shiny::numericInput(ns("detail_ch2_threshold"), "Threshold X / Ch2", value = NA_real_, step = 0.1),
              shiny::actionButton(ns("detail_auto_thresholds"), "Thresholds automatisch bestimmen", class = "btn-primary")
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'grid'", ns("classify_mode")),
              shiny::actionButton(ns("detail_auto_grid_thresholds"), "Grid-Thresholds ableiten", class = "btn-primary"),
              shiny::br(),
              shiny::br(),
              shiny::strong("NN"),
              shiny::numericInput(ns("grid_ch1_nn"), "Ch1 Threshold (NN)", value = NA_real_, step = 0.1),
              shiny::numericInput(ns("grid_ch2_nn"), "Ch2 Threshold (NN)", value = NA_real_, step = 0.1),
              shiny::strong("NP"),
              shiny::numericInput(ns("grid_ch1_np"), "Ch1 Threshold (NP)", value = NA_real_, step = 0.1),
              shiny::numericInput(ns("grid_ch2_np"), "Ch2 Threshold (NP)", value = NA_real_, step = 0.1),
              shiny::strong("PN"),
              shiny::numericInput(ns("grid_ch1_pn"), "Ch1 Threshold (PN)", value = NA_real_, step = 0.1),
              shiny::numericInput(ns("grid_ch2_pn"), "Ch2 Threshold (PN)", value = NA_real_, step = 0.1),
              shiny::strong("PP"),
              shiny::numericInput(ns("grid_ch1_pp"), "Ch1 Threshold (PP)", value = NA_real_, step = 0.1),
              shiny::numericInput(ns("grid_ch2_pp"), "Ch2 Threshold (PP)", value = NA_real_, step = 0.1)
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'knn'", ns("classify_mode")),
              shiny::radioButtons(
                ns("knn_training_source"),
                "Trainingsdaten",
                choices = c(
                  "Grid-Trainingsdaten" = "grid",
                  "Aktuelle Results-Klassifikation" = "results"
                ),
                selected = "grid"
              ),
              shiny::numericInput(
                ns("knn_k"),
                "k (# nächste Nachbarn)",
                value = TWODDPCR_KNN_K_DEFAULT,
                min = 1,
                step = 1
              ),
              shiny::sliderInput(
                ns("knn_prob"),
                "Min. Stimmanteil",
                min = 0,
                max = 1,
                value = TWODDPCR_KNN_PROB_DEFAULT,
                step = 0.05
              ),
              shiny::uiOutput(ns("knn_training_status_ui"))
            ),
            shiny::numericInput(ns("classify_binwidth"), "Bin Width", value = 0.5, min = 0.05, step = 0.05),
            shiny::actionButton(ns("run_classification"), "Klassifikation ausführen", class = "btn-success"),
            shiny::br(),
            shiny::br(),
            shiny::uiOutput(ns("classify_status_ui"))
          ),
          shiny::column(
            width = 9,
            shiny::tabsetPanel(
              shiny::tabPanel("Plot", shiny::plotOutput(ns("classify_plot"), height = "520px")),
              shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("classify_plotly"), height = "520px"))
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Results",
        status = "primary",
        solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::selectInput(ns("results_method"), "Klassifikation anzeigen", choices = c("Keine Klassifikation verfügbar" = "")),
            shiny::radioButtons(
              ns("rain_type"),
              "Rain-Behandlung",
              choices = c("Keine Rain-Entfernung" = "none", "Mahalanobis" = "mahalanobis", "Standard Deviation" = "sd"),
              selected = "none"
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'mahalanobis'", ns("rain_type")),
              shiny::uiOutput(ns("mahalanobis_rain_ui"))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'sd'", ns("rain_type")),
              shiny::uiOutput(ns("sd_rain_ui"))
            ),
            shiny::checkboxInput(ns("show_thresholds"), "Thresholds im Plot anzeigen", value = FALSE),
            shiny::checkboxInput(ns("show_final_centres"), "Finale Zentren anzeigen", value = FALSE),
            shiny::actionButton(ns("apply_rain"), "Results aktualisieren", class = "btn-primary"),
            shiny::br(),
            shiny::br(),
            shiny::downloadButton(ns("download_amplitudes_zip"), "Amplituden exportieren"),
            shiny::br(),
            shiny::br(),
            shiny::uiOutput(ns("results_status_ui"))
          ),
          shiny::column(
            width = 9,
            shiny::tabsetPanel(
              shiny::tabPanel("Plot", shiny::plotOutput(ns("results_plot"), height = "520px")),
              shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("results_plotly"), height = "520px"))
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Results-Tabelle",
        status = "info",
        solidHeader = TRUE,
        DT::DTOutput(ns("results_table"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Summary",
        status = "primary",
        solidHeader = TRUE,
        shiny::downloadButton(ns("download_summary_csv"), "Summary als CSV herunterladen"),
        shiny::downloadButton(ns("download_html_report"), "HTML-Report herunterladen"),
        shiny::br(),
        shiny::br(),
        DT::DTOutput(ns("summary_table"))
      )
    )
  )
}

mod_sample_detail_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    preserve_single_value <- function(current, choices, default = NULL) {
      values <- as.character(unname(choices))
      values <- values[!is.na(values)]

      if (!is.null(current) && length(current) == 1 && !is.na(current) && current %in% values) {
        return(as.character(current))
      }

      if (!is.null(default) && length(default) == 1 && !is.na(default) && default %in% values) {
        return(as.character(default))
      }

      if (length(values) == 0) {
        return(character())
      }

      values[[1]]
    }

    preserve_multi_value <- function(current, choices) {
      values <- as.character(unname(choices))
      values <- values[!is.na(values)]

      if (length(values) == 0) {
        return(character())
      }

      if (is.null(current)) {
        return(values)
      }

      current[current %in% values]
    }

    normalize_multi_value <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(character())
      }

      sort(unique(as.character(x)))
    }

    is_single_nonempty_string <- function(x) {
      length(x) == 1 && !is.na(x) && nzchar(x)
    }

    grid_input_map <- c(
      grid_ch1_nn = "ch1NNThreshold",
      grid_ch2_nn = "ch2NNThreshold",
      grid_ch1_np = "ch1NPThreshold",
      grid_ch2_np = "ch2NPThreshold",
      grid_ch1_pn = "ch1PNThreshold",
      grid_ch2_pn = "ch2PNThreshold",
      grid_ch1_pp = "ch1PPThreshold",
      grid_ch2_pp = "ch2PPThreshold"
    )

    get_grid_threshold_inputs <- function() {
      input_values <- lapply(names(grid_input_map), function(input_id) suppressWarnings(as.numeric(input[[input_id]])))
      names(input_values) <- unname(grid_input_map)
      normalize_twoddpcr_grid_thresholds(input_values)
    }

    update_grid_threshold_inputs <- function(values, overwrite_auto_only = TRUE) {
      values <- normalize_twoddpcr_grid_thresholds(values)

      for (input_id in names(grid_input_map)) {
        threshold_name <- grid_input_map[[input_id]]
        current_value <- suppressWarnings(as.numeric(isolate(input[[input_id]])))
        should_update <- !overwrite_auto_only ||
          !is.finite(current_value) ||
          identical(current_value, ui_state$auto_grid_thresholds[[threshold_name]])

        if (isTRUE(should_update)) {
          shiny::updateNumericInput(session, input_id, value = round(values[[threshold_name]], 3))
        }
      }

      ui_state$auto_grid_thresholds <- values
      invisible(values)
    }

    get_cluster_parameter_inputs <- function(prefix, default_value) {
      ids <- stats::setNames(
        paste0(prefix, c("nn", "np", "pn", "pp")),
        twoddpcr_class_codes()
      )

      values <- lapply(ids, function(input_id) suppressWarnings(as.numeric(input[[input_id]])))
      normalize_twoddpcr_cluster_parameters(values, default_value)
    }

    same_choice_vector <- function(lhs, rhs) {
      identical(names(lhs), names(rhs)) &&
        identical(as.character(unname(lhs)), as.character(unname(rhs)))
    }

    same_multi_selection <- function(lhs, rhs) {
      identical(normalize_multi_value(lhs), normalize_multi_value(rhs))
    }

    ui_state <- shiny::reactiveValues(
      plate_choices = NULL,
      plate_selected = character(),
      sample_choices = NULL,
      sample_selected = character(),
      channel_x_choices = NULL,
      channel_x_selected = character(),
      channel_y_choices = NULL,
      channel_y_selected = character(),
      auto_ch1_label = "Fluoreszenzkanal Y",
      auto_ch1_abbrev = "Y",
      auto_ch2_label = "Fluoreszenzkanal X",
      auto_ch2_abbrev = "X",
      auto_ch1_threshold = NA_real_,
      auto_ch2_threshold = NA_real_,
      auto_grid_thresholds = normalize_twoddpcr_grid_thresholds(NULL),
      results_method_choices = NULL,
      results_method_selected = character()
    )

    analysis_state <- shiny::reactiveValues(
      classified_plate = NULL,
      rainy_plate = NULL,
      base_method = NULL
    )

    update_selectize_if_needed <- function(input_id, choices, selected, choice_key, selected_key, server = TRUE) {
      choices_changed <- !same_choice_vector(choices, ui_state[[choice_key]])
      selected_changed <- !same_multi_selection(selected, ui_state[[selected_key]])

      if (!choices_changed && !selected_changed) {
        return(invisible(NULL))
      }

      shiny::freezeReactiveValue(input, input_id)
      shiny::updateSelectizeInput(session, input_id, choices = choices, selected = selected, server = server)

      ui_state[[choice_key]] <- choices
      ui_state[[selected_key]] <- normalize_multi_value(selected)
      invisible(NULL)
    }

    update_select_if_needed <- function(input_id, choices, selected, choice_key, selected_key) {
      choices_changed <- !same_choice_vector(choices, ui_state[[choice_key]])
      selected_changed <- !identical(as.character(selected), as.character(ui_state[[selected_key]]))

      if (!choices_changed && !selected_changed) {
        return(invisible(NULL))
      }

      shiny::freezeReactiveValue(input, input_id)
      shiny::updateSelectInput(session, input_id, choices = choices, selected = selected)

      ui_state[[choice_key]] <- choices
      ui_state[[selected_key]] <- as.character(selected)
      invisible(NULL)
    }

    plate_choices <- shiny::reactive({
      df <- state$dpcr_data
      if (is.null(df) || nrow(df) == 0) {
        return(c("Bitte laden Sie zunächst Daten" = ""))
      }

      plate_vec <- ifelse(
        !is.na(df$plate_name) & trimws(df$plate_name) != "",
        df$plate_name,
        df$plate_id
      )
      plate_vec <- sort(unique(plate_vec[!is.na(plate_vec) & plate_vec != ""]))

      c("Alle Platten" = "__all__", stats::setNames(plate_vec, plate_vec))
    })

    base_plate_filtered_data <- shiny::reactive({
      df <- state$dpcr_data
      if (is.null(df) || nrow(df) == 0) {
        return(new_empty_dpcr_data())
      }

      selected_plate <- input$detail_plate_select
      if (!is.null(selected_plate) && nzchar(selected_plate) && selected_plate != "__all__") {
        df <- dplyr::filter(df, plate_name == selected_plate | plate_id == selected_plate)
      }

      df
    })

    sample_choices <- shiny::reactive({
      samples <- available_sample_names(base_plate_filtered_data())
      stats::setNames(samples, samples)
    })

    channel_choices <- shiny::reactive({
      channels <- available_twoddpcr_channels(base_plate_filtered_data(), exclude_reference = TRUE)
      stats::setNames(channels, channels)
    })

    shiny::observeEvent(plate_choices(), {
      update_select_if_needed(
        input_id = "detail_plate_select",
        choices = plate_choices(),
        selected = preserve_single_value(isolate(input$detail_plate_select), plate_choices(), default = "__all__"),
        choice_key = "plate_choices",
        selected_key = "plate_selected"
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(sample_choices(), {
      update_selectize_if_needed(
        input_id = "detail_samples",
        choices = sample_choices(),
        selected = preserve_multi_value(isolate(input$detail_samples), sample_choices()),
        choice_key = "sample_choices",
        selected_key = "sample_selected",
        server = TRUE
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(channel_choices(), {
      channels <- channel_choices()
      defaults <- default_twoddpcr_channel_pair(base_plate_filtered_data())

      update_select_if_needed(
        input_id = "detail_channel_x",
        choices = channels,
        selected = preserve_single_value(isolate(input$detail_channel_x), channels, default = defaults[["x"]]),
        choice_key = "channel_x_choices",
        selected_key = "channel_x_selected"
      )

      update_select_if_needed(
        input_id = "detail_channel_y",
        choices = channels,
        selected = preserve_single_value(isolate(input$detail_channel_y), channels, default = defaults[["y"]]),
        choice_key = "channel_y_choices",
        selected_key = "channel_y_selected"
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(input$detail_channel_x, input$detail_channel_y), {
      new_ch1_label <- default_channel_axis_label(input$detail_channel_y, axis_fallback = "Y")
      new_ch1_abbrev <- default_twoddpcr_abbrev(input$detail_channel_y, fallback = "Y")
      new_ch2_label <- default_channel_axis_label(input$detail_channel_x, axis_fallback = "X")
      new_ch2_abbrev <- default_twoddpcr_abbrev(input$detail_channel_x, fallback = "X")

      current_ch1_label <- isolate(input$detail_ch1_label)
      current_ch1_abbrev <- isolate(input$detail_ch1_abbrev)
      current_ch2_label <- isolate(input$detail_ch2_label)
      current_ch2_abbrev <- isolate(input$detail_ch2_abbrev)

      if (is.null(current_ch1_label) || identical(current_ch1_label, "") || identical(current_ch1_label, ui_state$auto_ch1_label)) {
        shiny::updateTextInput(session, "detail_ch1_label", value = new_ch1_label)
      }
      if (is.null(current_ch1_abbrev) || identical(current_ch1_abbrev, "") || identical(current_ch1_abbrev, ui_state$auto_ch1_abbrev)) {
        shiny::updateTextInput(session, "detail_ch1_abbrev", value = new_ch1_abbrev)
      }
      if (is.null(current_ch2_label) || identical(current_ch2_label, "") || identical(current_ch2_label, ui_state$auto_ch2_label)) {
        shiny::updateTextInput(session, "detail_ch2_label", value = new_ch2_label)
      }
      if (is.null(current_ch2_abbrev) || identical(current_ch2_abbrev, "") || identical(current_ch2_abbrev, ui_state$auto_ch2_abbrev)) {
        shiny::updateTextInput(session, "detail_ch2_abbrev", value = new_ch2_abbrev)
      }

      ui_state$auto_ch1_label <- new_ch1_label
      ui_state$auto_ch1_abbrev <- new_ch1_abbrev
      ui_state$auto_ch2_label <- new_ch2_label
      ui_state$auto_ch2_abbrev <- new_ch2_abbrev
    }, ignoreInit = FALSE)

    build_detail_adapter <- function() {
      adapt_dpcr_to_twoddpcr(
        state$dpcr_data,
        samples = input$detail_samples,
        channel_x = input$detail_channel_x,
        channel_y = input$detail_channel_y,
        plate_filter = input$detail_plate_select,
        exclude_invalid = isTRUE(input$detail_exclude_invalid)
      )
    }

    adapter <- shiny::reactive({
      build_detail_adapter()
    })

    shiny::observeEvent(adapter(), {
      detail_adapter <- adapter()
      defaults <- detail_adapter$thresholds

      if (is.null(defaults)) {
        return()
      }

      current_ch1_threshold <- suppressWarnings(as.numeric(isolate(input$detail_ch1_threshold)))
      current_ch2_threshold <- suppressWarnings(as.numeric(isolate(input$detail_ch2_threshold)))

      if (!is.finite(current_ch1_threshold) || identical(current_ch1_threshold, ui_state$auto_ch1_threshold)) {
        shiny::updateNumericInput(session, "detail_ch1_threshold", value = round(defaults$ch1_threshold, 3))
      }

      if (!is.finite(current_ch2_threshold) || identical(current_ch2_threshold, ui_state$auto_ch2_threshold)) {
        shiny::updateNumericInput(session, "detail_ch2_threshold", value = round(defaults$ch2_threshold, 3))
      }

      ui_state$auto_ch1_threshold <- defaults$ch1_threshold
      ui_state$auto_ch2_threshold <- defaults$ch2_threshold

      update_grid_threshold_inputs(
        compute_twoddpcr_grid_defaults(detail_adapter$pair_data),
        overwrite_auto_only = TRUE
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(
      list(input$detail_plate_select, input$detail_samples, input$detail_channel_x, input$detail_channel_y, input$detail_exclude_invalid),
      {
        analysis_state$classified_plate <- NULL
        analysis_state$rainy_plate <- NULL
        analysis_state$base_method <- NULL
      },
      ignoreInit = TRUE
    )

    current_analysis_plate <- shiny::reactive({
      if (!is.null(analysis_state$rainy_plate)) {
        return(analysis_state$rainy_plate)
      }

      analysis_state$classified_plate
    })

    output$twoddpcr_availability <- shiny::renderUI({
      if (is_twoddpcr_available()) {
        shiny::tags$div(
          class = "alert alert-success",
          "twoddpcr ist verfügbar. Adapter und Plot-/Summary-Funktionen werden direkt aus dem Paket verwendet."
        )
      } else {
        shiny::tags$div(
          class = "alert alert-danger",
          "twoddpcr ist nicht installiert. Die Detailanalyse kann ohne dieses Paket nicht ausgeführt werden."
        )
      }
    })

    output$mahalanobis_rain_ui <- shiny::renderUI({
      labels <- twoddpcr_abbrev_labels(
        default_twoddpcr_abbrev(input$detail_ch1_abbrev, fallback = "Y"),
        default_twoddpcr_abbrev(input$detail_ch2_abbrev, fallback = "X")
      )

      shiny::tagList(
        shiny::sliderInput(ns("mvn_rain_nn"), labels[[1]], min = 1, max = 100, value = TWODDPCR_MAHALANOBIS_MAX_DEFAULT),
        shiny::sliderInput(ns("mvn_rain_np"), labels[[2]], min = 1, max = 100, value = TWODDPCR_MAHALANOBIS_MAX_DEFAULT),
        shiny::sliderInput(ns("mvn_rain_pn"), labels[[3]], min = 1, max = 100, value = TWODDPCR_MAHALANOBIS_MAX_DEFAULT),
        shiny::sliderInput(ns("mvn_rain_pp"), labels[[4]], min = 1, max = 100, value = TWODDPCR_MAHALANOBIS_MAX_DEFAULT)
      )
    })

    output$sd_rain_ui <- shiny::renderUI({
      labels <- twoddpcr_abbrev_labels(
        default_twoddpcr_abbrev(input$detail_ch1_abbrev, fallback = "Y"),
        default_twoddpcr_abbrev(input$detail_ch2_abbrev, fallback = "X")
      )

      shiny::tagList(
        shiny::sliderInput(ns("sd_rain_nn"), labels[[1]], min = 1, max = 100, value = TWODDPCR_SD_ERROR_DEFAULT),
        shiny::sliderInput(ns("sd_rain_np"), labels[[2]], min = 1, max = 100, value = TWODDPCR_SD_ERROR_DEFAULT),
        shiny::sliderInput(ns("sd_rain_pn"), labels[[3]], min = 1, max = 100, value = TWODDPCR_SD_ERROR_DEFAULT),
        shiny::sliderInput(ns("sd_rain_pp"), labels[[4]], min = 1, max = 100, value = TWODDPCR_SD_ERROR_DEFAULT)
      )
    })

    output$adapter_status_ui <- shiny::renderUI({
      detail_adapter <- adapter()

      if (!isTRUE(detail_adapter$ok)) {
        return(shiny::tags$div(class = "alert alert-warning", detail_adapter$message))
      }

      shiny::tagList(
        shiny::tags$div(
          class = "alert alert-info",
          sprintf(
            "Adapter erfolgreich: %s Wells, %s vollständige Zwei-Kanal-Partitionen. Ch1/Y = %s, Ch2/X = %s.",
            format(length(detail_adapter$plate), big.mark = ".", decimal.mark = ","),
            format(nrow(detail_adapter$pair_data), big.mark = ".", decimal.mark = ","),
            detail_adapter$channel_y,
            detail_adapter$channel_x
          )
        ),
        shiny::tags$p(
          class = "text-muted",
          "Mapping für twoddpcr: Y-Kanal wird als Ch1.Amplitude, X-Kanal als Ch2.Amplitude an ddpcrWell/ddpcrPlate übergeben."
        )
      )
    })

    build_training_data_internal <- function(detail_adapter = build_detail_adapter()) {
      training_source <- if (is_single_nonempty_string(input$knn_training_source)) input$knn_training_source else "grid"

      if (identical(training_source, "results")) {
        current_plate <- current_analysis_plate()
        selected_method <- if (is_single_nonempty_string(input$results_method)) input$results_method else analysis_state$base_method

        if (is.null(current_plate) || !is_single_nonempty_string(selected_method)) {
          stop("Für K-NN mit Results-Trainingsdaten muss zuerst eine Klassifikation in Results vorliegen.")
        }

        return(build_twoddpcr_training_data(
          current_plate,
          source = "results",
          c_method = selected_method
        ))
      }

      build_twoddpcr_training_data(
        detail_adapter$plate,
        source = "grid",
        grid_thresholds = get_grid_threshold_inputs()
      )
    }

    output$knn_training_status_ui <- shiny::renderUI({
      if (!identical(input$classify_mode, "knn")) {
        return(NULL)
      }

      status <- tryCatch(
        {
          td <- build_training_data_internal()
          shiny::tags$div(
            class = "alert alert-info",
            sprintf("Trainingsdaten bereit: %s Droplets.", format(nrow(td), big.mark = ".", decimal.mark = ","))
          )
        },
        error = function(e) shiny::tags$div(class = "alert alert-warning", e$message)
      )

      status
    })

    output$classify_status_ui <- shiny::renderUI({
      if (is.null(analysis_state$classified_plate)) {
        return(shiny::tags$div(class = "alert alert-light", "Noch keine Klassifikation ausgeführt."))
      }

      shiny::tags$div(
        class = "alert alert-success",
        sprintf("Klassifikation erfolgreich: %s", twoddpcr_method_label(analysis_state$base_method))
      )
    })

    output$results_status_ui <- shiny::renderUI({
      current_plate <- current_analysis_plate()
      if (is.null(current_plate)) {
        return(shiny::tags$div(class = "alert alert-light", "Bitte zuerst im Abschnitt 'Classify' eine Klassifikation ausführen."))
      }

      shiny::tags$div(
        class = "alert alert-info",
        sprintf(
          "Aktive Darstellung: %s",
          twoddpcr_method_label(if (is_single_nonempty_string(input$results_method)) input$results_method else analysis_state$base_method)
        )
      )
    })

    shiny::observeEvent(input$detail_auto_thresholds, {
      defaults <- adapter()$thresholds

      if (is.null(defaults) || !is.finite(defaults$ch1_threshold) || !is.finite(defaults$ch2_threshold)) {
        shiny::showNotification("Automatische Thresholds konnten nicht bestimmt werden.", type = "error")
        return()
      }

      shiny::updateNumericInput(session, "detail_ch1_threshold", value = round(defaults$ch1_threshold, 3))
      shiny::updateNumericInput(session, "detail_ch2_threshold", value = round(defaults$ch2_threshold, 3))
      ui_state$auto_ch1_threshold <- defaults$ch1_threshold
      ui_state$auto_ch2_threshold <- defaults$ch2_threshold
      shiny::showNotification("Thresholds aktualisiert.", type = "message")
    })

    shiny::observeEvent(input$detail_auto_grid_thresholds, {
      grid_defaults <- compute_twoddpcr_grid_defaults(adapter()$pair_data)
      update_grid_threshold_inputs(grid_defaults, overwrite_auto_only = FALSE)
      shiny::showNotification("Grid-Thresholds aktualisiert.", type = "message")
    })

    run_classification_internal <- function() {
      detail_adapter <- build_detail_adapter()

      if (!isTRUE(detail_adapter$ok)) {
        stop(detail_adapter$message)
      }

      configure_twoddpcr_droplet_volume(input$detail_droplet_volume)

      classified <- classify_twoddpcr_plate(
        detail_adapter$plate,
        method = input$classify_mode,
        kmeans_centers = input$kmeans_centers,
        ch1_threshold = input$detail_ch1_threshold,
        ch2_threshold = input$detail_ch2_threshold,
        grid_thresholds = get_grid_threshold_inputs(),
        knn_training_data = if (identical(input$classify_mode, "knn")) build_training_data_internal(detail_adapter) else NULL,
        knn_k = input$knn_k,
        knn_prob = input$knn_prob
      )

      analysis_state$classified_plate <- classified$plate
      analysis_state$rainy_plate <- NULL
      analysis_state$base_method <- classified$base_method

      invisible(classified)
    }

    shiny::observeEvent(input$run_classification, {
      tryCatch({
        run_classification_internal()
        shiny::showNotification("Klassifikation erfolgreich ausgeführt.", type = "message")
      }, error = function(e) {
        analysis_state$classified_plate <- NULL
        analysis_state$rainy_plate <- NULL
        analysis_state$base_method <- NULL
        shiny::showNotification(sprintf("Klassifikation fehlgeschlagen: %s", e$message), type = "error")
      })
    })

    shiny::observe({
      methods_choices <- available_twoddpcr_methods(current_analysis_plate())
      values <- unname(methods_choices)
      valid_values <- values[values != ""]

      selected <- input$results_method
      if (length(valid_values) == 0) {
        selected <- ""
      } else if (is.null(selected) || !selected %in% valid_values) {
        selected <- valid_values[[1]]
      }

      update_select_if_needed(
        input_id = "results_method",
        choices = methods_choices,
        selected = selected,
        choice_key = "results_method_choices",
        selected_key = "results_method_selected"
      )
    })

    apply_rain_internal <- function() {
      if (is.null(analysis_state$classified_plate) || is.null(analysis_state$base_method)) {
        stop("Bitte zuerst eine Klassifikation ausführen.")
      }

      rain_state <- apply_twoddpcr_rain_to_plate(
        analysis_state$classified_plate,
        base_method = analysis_state$base_method,
        rain_type = input$rain_type,
        mahalanobis_max = get_cluster_parameter_inputs("mvn_rain_", TWODDPCR_MAHALANOBIS_MAX_DEFAULT),
        sd_error = get_cluster_parameter_inputs("sd_rain_", TWODDPCR_SD_ERROR_DEFAULT)
      )

      if (identical(input$rain_type, "none")) {
        analysis_state$rainy_plate <- NULL
      } else {
        analysis_state$rainy_plate <- rain_state$plate
      }

      shiny::updateSelectInput(session, "results_method", selected = rain_state$method)
      invisible(rain_state)
    }

    shiny::observeEvent(input$apply_rain, {
      tryCatch({
        rain_state <- apply_rain_internal()
        shiny::showNotification("Results aktualisiert.", type = "message")
      }, error = function(e) {
        shiny::showNotification(sprintf("Results-Aktualisierung fehlgeschlagen: %s", e$message), type = "error")
      })
    })

    classify_plot <- shiny::reactive({
      detail_adapter <- adapter()

      if (!isTRUE(detail_adapter$ok)) {
        return(build_empty_plot(detail_adapter$message))
      }

      build_twoddpcr_classify_plot(
        detail_adapter$plate,
        plot_limits = detail_adapter$plot_limits,
        ch1_label = input$detail_ch1_label,
        ch2_label = input$detail_ch2_label,
        binwidth = input$classify_binwidth,
        method = input$classify_mode,
        ch1_threshold = input$detail_ch1_threshold,
        ch2_threshold = input$detail_ch2_threshold,
        grid_thresholds = get_grid_threshold_inputs()
      )
    })

    output$classify_plot <- shiny::renderPlot({
      classify_plot()
    }, res = 96)

    output$classify_plotly <- plotly::renderPlotly({
      plotly::ggplotly(classify_plot())
    })

    results_plot <- shiny::reactive({
      current_plate <- current_analysis_plate()
      detail_adapter <- adapter()
      selected_method <- input$results_method

      if (is.null(current_plate) || !is_single_nonempty_string(selected_method)) {
        return(build_empty_plot("Bitte zuerst im Abschnitt 'Classify' eine Klassifikation ausführen."))
      }

      configure_twoddpcr_droplet_volume(input$detail_droplet_volume)

      build_twoddpcr_results_plot(
        current_plate,
        c_method = selected_method,
        plot_limits = detail_adapter$plot_limits,
        ch1_label = input$detail_ch1_label,
        ch2_label = input$detail_ch2_label,
        ch1_abbrev = input$detail_ch1_abbrev,
        ch2_abbrev = input$detail_ch2_abbrev,
        show_thresholds = isTRUE(input$show_thresholds),
        ch1_threshold = input$detail_ch1_threshold,
        ch2_threshold = input$detail_ch2_threshold,
        show_final_centres = isTRUE(input$show_final_centres),
        grid_thresholds = get_grid_threshold_inputs()
      )
    })

    output$results_plot <- shiny::renderPlot({
      results_plot()
    }, res = 96)

    output$results_plotly <- plotly::renderPlotly({
      plotly::ggplotly(results_plot())
    })

    results_table_data <- shiny::reactive({
      current_plate <- current_analysis_plate()
      selected_method <- input$results_method

      if (is.null(current_plate) || !is_single_nonempty_string(selected_method)) {
        return(tibble::tibble())
      }

      configure_twoddpcr_droplet_volume(input$detail_droplet_volume)

      build_twoddpcr_summary_table(
        current_plate,
        c_method = selected_method,
        well_map = adapter()$well_map,
        ch1_abbrev = input$detail_ch1_abbrev,
        ch2_abbrev = input$detail_ch2_abbrev,
        type = "results"
      )
    })

    output$results_table <- DT::renderDT({
      table_df <- results_table_data()

      if (nrow(table_df) == 0) {
        table_df <- tibble::tibble(Hinweis = "Noch keine Results-Tabelle verfügbar.")
      }

      DT::datatable(
        table_df,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    summary_table_data <- shiny::reactive({
      current_plate <- current_analysis_plate()
      selected_method <- input$results_method

      if (is.null(current_plate) || !is_single_nonempty_string(selected_method)) {
        return(tibble::tibble())
      }

      configure_twoddpcr_droplet_volume(input$detail_droplet_volume)

      build_twoddpcr_summary_table(
        current_plate,
        c_method = selected_method,
        well_map = adapter()$well_map,
        ch1_abbrev = input$detail_ch1_abbrev,
        ch2_abbrev = input$detail_ch2_abbrev,
        type = "full"
      )
    })

    output$summary_table <- DT::renderDT({
      summary_df <- summary_table_data()

      if (nrow(summary_df) == 0) {
        summary_df <- tibble::tibble(Hinweis = "Noch keine Summary verfügbar.")
      }

      table_dt <- DT::datatable(
        summary_df,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )

      numeric_cols <- names(summary_df)[vapply(summary_df, is.numeric, logical(1))]
      if (length(numeric_cols) > 0) {
        table_dt <- DT::formatRound(table_dt, columns = numeric_cols, digits = 3)
      }

      table_dt
    })

    output$download_summary_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("twoddpcr_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        summary_df <- summary_table_data()

        if (nrow(summary_df) == 0) {
          stop("Keine Summary-Daten zum Export verfügbar.")
        }

        readr::write_csv(summary_df, file)
      }
    )

    output$download_amplitudes_zip <- shiny::downloadHandler(
      filename = function() {
        plate_label <- input$detail_plate_select
        if (is.null(plate_label) || !nzchar(plate_label) || identical(plate_label, "__all__")) {
          plate_label <- "twoddpcr"
        }

        paste0(plate_label, "_", input$results_method, "-amplitudes.zip")
      },
      content = function(file) {
        current_plate <- current_analysis_plate()
        selected_method <- input$results_method

        if (is.null(current_plate) || !is_single_nonempty_string(selected_method)) {
          stop("Keine klassifizierten Amplituden zum Export verfügbar.")
        }

        twoddpcr::exportZip(
          current_plate,
          location = file,
          cMethod = selected_method,
          prefix = ""
        )
      },
      contentType = "application/zip"
    )

    output$download_html_report <- shiny::downloadHandler(
      filename = function() {
        plate_label <- input$detail_plate_select
        if (is.null(plate_label) || !nzchar(plate_label) || identical(plate_label, "__all__")) {
          plate_label <- "twoddpcr"
        }

        paste0(plate_label, "_", input$results_method, "-report.html")
      },
      content = function(file) {
        if (!requireNamespace("rmarkdown", quietly = TRUE)) {
          stop("Das Paket 'rmarkdown' ist für den HTML-Report erforderlich.")
        }

        current_plate <- current_analysis_plate()
        selected_method <- input$results_method
        summary_df <- summary_table_data()

        if (is.null(current_plate) || !is_single_nonempty_string(selected_method) || nrow(summary_df) == 0) {
          stop("Keine Summary-Daten für den HTML-Report verfügbar.")
        }

        temp_report <- file.path(normalizePath(tempdir()), "twoddpcr-report-html.Rmd")
        template_path <- system.file("rmd", "report-html.Rmd", package = "twoddpcr")

        if (!nzchar(template_path) || !file.exists(template_path)) {
          stop("Die Report-Vorlage aus 'twoddpcr' konnte nicht gefunden werden.")
        }

        file.copy(template_path, temp_report, overwrite = TRUE)

        plate_label <- input$detail_plate_select
        if (is.null(plate_label) || !nzchar(plate_label) || identical(plate_label, "__all__")) {
          plate_label <- "Ausgewählte Samples"
        }

        params <- list(
          plateName = plate_label,
          plate = current_plate,
          cMethod = selected_method,
          longMode = twoddpcr_method_label(analysis_state$base_method),
          rainType = twoddpcr_rain_label(selected_method),
          summaryTable = as.data.frame(summary_df)
        )

        rmarkdown::render(
          input = temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
      }
    )

    invisible(list(
      adapter = adapter,
      build_detail_adapter = build_detail_adapter,
      build_training_data_internal = build_training_data_internal,
      run_classification_internal = run_classification_internal,
      apply_rain_internal = apply_rain_internal,
      current_analysis_plate = current_analysis_plate,
      results_table_data = results_table_data,
      summary_table_data = summary_table_data
    ))
  })
}
