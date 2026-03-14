mod_sample_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinydashboard::box(
          width = NULL,
          title = "Darstellung",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          shiny::sliderInput(
            ns("scatter_alpha"),
            "Alpha-Wert Punkte",
            min = 0.05,
            max = 1,
            value = SCATTER_ALPHA_DEFAULT,
            step = 0.05
          ),
          shiny::helpText("Gilt fuer Plot A und Plot B.")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinydashboard::box(
          width = NULL,
          title = "Plot A - Filter",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          shiny::selectizeInput(
            ns("plot_a_samples"),
            "Samples",
            choices = character(),
            selected = character(),
            multiple = TRUE
          ),
          shiny::selectInput(ns("plot_a_channel"), "Farbkanal / Target", choices = character()),
          shiny::selectInput(ns("plot_a_x_col"), "X-Achse", choices = character()),
          shiny::selectInput(ns("plot_a_y_col"), "Y-Achse", choices = character()),
          shiny::selectInput(
            ns("plot_a_color_by"),
            "Farbe nach",
            choices = c("sample", "well", "channel", "threshold_status", "none"),
            selected = "sample"
          ),
          shiny::textInput(ns("plot_a_title"), "Titel", value = "Sample vs Fluoreszenz"),
          shiny::textInput(ns("plot_a_subtitle"), "Untertitel", value = "Uebersicht ueber Fluoreszenz je Sample"),
          shiny::textInput(ns("plot_a_x_label"), "X-Achsenlabel", value = "Sample"),
          shiny::textInput(ns("plot_a_y_label"), "Y-Achsenlabel", value = "Fluoreszenz"),
          shiny::numericInput(ns("plot_a_x_size"), "Schriftgroesse X", value = 12, min = 8, max = 24),
          shiny::numericInput(ns("plot_a_y_size"), "Schriftgroesse Y", value = 12, min = 8, max = 24),
          shiny::checkboxInput(ns("plot_a_threshold_enabled"), "Threshold im Plot anzeigen", value = TRUE),
          shiny::numericInput(ns("plot_a_threshold"), "Threshold", value = 100, step = 0.1),
          shiny::actionButton(ns("plot_a_auto_threshold"), "Threshold automatisch bestimmen", class = "btn-primary"),
          shiny::numericInput(
            ns("plot_a_max_points"),
            "Max. Punkte (Interaktiver Plot)",
            value = 60000,
            min = 10000,
            max = 1000000,
            step = 10000
          ),
          shiny::checkboxInput(
            ns("plot_a_use_webgl"),
            "WebGL fuer Interaktiven Plot nutzen",
            value = PLOTLY_USE_WEBGL_DEFAULT
          )
        )
      ),
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = NULL,
          title = "Plot A - Sample vs Fluoreszenz",
          status = "primary",
          solidHeader = TRUE,
          shiny::uiOutput(ns("plot_a_sampling_info")),
          shiny::tabsetPanel(
            shiny::tabPanel("Plot", shiny::plotOutput(ns("plot_a_plot"), height = "460px")),
            shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("plot_a_plotly"), height = "460px"))
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 4),
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = NULL,
          title = "Tabelle Plot A",
          status = "info",
          solidHeader = TRUE,
          DT::DTOutput(ns("plot_a_table"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinydashboard::box(
          width = NULL,
          title = "Plot B - Filter",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          shiny::selectizeInput(
            ns("plot_b_samples"),
            "Samples",
            choices = character(),
            selected = character(),
            multiple = TRUE
          ),
          shiny::selectInput(ns("plot_b_channel_x"), "Kanal fuer X", choices = character()),
          shiny::selectInput(ns("plot_b_channel_y"), "Kanal fuer Y", choices = character()),
          shiny::selectInput(
            ns("plot_b_color_by"),
            "Farbe nach",
            choices = c("sample", "well", "plate_name", "none"),
            selected = "sample"
          ),
          shiny::textInput(ns("plot_b_title"), "Titel", value = "Kanal vs Kanal"),
          shiny::textInput(ns("plot_b_subtitle"), "Untertitel", value = "Vergleich zweier Fluoreszenzkanaele"),
          shiny::textInput(ns("plot_b_x_label"), "X-Achsenlabel", value = "Fluoreszenzkanal X"),
          shiny::textInput(ns("plot_b_y_label"), "Y-Achsenlabel", value = "Fluoreszenzkanal Y"),
          shiny::numericInput(ns("plot_b_x_size"), "Schriftgroesse X", value = 12, min = 8, max = 24),
          shiny::numericInput(ns("plot_b_y_size"), "Schriftgroesse Y", value = 12, min = 8, max = 24),
          shiny::checkboxInput(ns("plot_b_threshold_enabled"), "Thresholds im Plot anzeigen", value = TRUE),
          shiny::numericInput(ns("plot_b_threshold_x"), "X-Threshold", value = 100, step = 0.1),
          shiny::numericInput(ns("plot_b_threshold_y"), "Y-Threshold", value = 100, step = 0.1),
          shiny::actionButton(ns("plot_b_auto_threshold"), "Threshold automatisch bestimmen", class = "btn-primary"),
          shiny::numericInput(
            ns("plot_b_max_points"),
            "Max. Punkte (Interaktiver Plot)",
            value = 60000,
            min = 10000,
            max = 1000000,
            step = 10000
          ),
          shiny::checkboxInput(
            ns("plot_b_use_webgl"),
            "WebGL fuer Interaktiven Plot nutzen",
            value = PLOTLY_USE_WEBGL_DEFAULT
          )
        )
      ),
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = NULL,
          title = "Plot B - Kanal vs Kanal",
          status = "primary",
          solidHeader = TRUE,
          shiny::uiOutput(ns("plot_b_sampling_info")),
          shiny::tabsetPanel(
            shiny::tabPanel("Plot", shiny::plotOutput(ns("plot_b_plot"), height = "460px")),
            shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("plot_b_plotly"), height = "460px"))
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 4),
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = NULL,
          title = "Tabelle Plot B",
          status = "info",
          solidHeader = TRUE,
          DT::DTOutput(ns("plot_b_table"))
        )
      )
    )
  )
}

mod_sample_analysis_server <- function(id, state) {
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

    same_choice_vector <- function(lhs, rhs) {
      identical(names(lhs), names(rhs)) &&
        identical(as.character(unname(lhs)), as.character(unname(rhs)))
    }

    same_multi_selection <- function(lhs, rhs) {
      identical(normalize_multi_value(lhs), normalize_multi_value(rhs))
    }

    ui_state <- shiny::reactiveValues(
      plot_a_samples_choices = NULL,
      plot_a_samples_selected = character(),
      plot_a_channel_choices = NULL,
      plot_a_channel_selected = character(),
      plot_a_x_choices = NULL,
      plot_a_x_selected = character(),
      plot_a_y_choices = NULL,
      plot_a_y_selected = character(),
      plot_b_samples_choices = NULL,
      plot_b_samples_selected = character(),
      plot_b_channel_x_choices = NULL,
      plot_b_channel_x_selected = character(),
      plot_b_channel_y_choices = NULL,
      plot_b_channel_y_selected = character(),
      plot_b_auto_x_label = default_channel_axis_label(character(), axis_fallback = "X"),
      plot_b_auto_y_label = default_channel_axis_label(character(), axis_fallback = "Y")
    )

    update_selectize_if_needed <- function(input_id, choices, selected, choice_key, selected_key, server = TRUE) {
      choices_changed <- !same_choice_vector(choices, ui_state[[choice_key]])
      selected_changed <- !same_multi_selection(selected, ui_state[[selected_key]])

      if (!choices_changed && !selected_changed) {
        return(invisible(NULL))
      }

      shiny::freezeReactiveValue(input, input_id)
      shiny::updateSelectizeInput(
        session,
        input_id,
        choices = choices,
        selected = selected,
        server = server
      )

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
      shiny::updateSelectInput(
        session,
        input_id,
        choices = choices,
        selected = selected
      )

      ui_state[[choice_key]] <- choices
      ui_state[[selected_key]] <- as.character(selected)
      invisible(NULL)
    }

    base_data <- shiny::reactive({
      sample_analysis_prepare_data(state$dpcr_data)
    })

    sample_choices <- shiny::reactive({
      samples <- available_sample_names(base_data())
      stats::setNames(samples, samples)
    })

    channel_choices <- shiny::reactive({
      channels <- available_channel_names(base_data())
      stats::setNames(channels, channels)
    })

    plot_a_axis_choices <- shiny::reactive({
      sample_analysis_axis_choices(base_data(), include_categorical = TRUE)
    })

    plot_a_numeric_choices <- shiny::reactive({
      sample_analysis_axis_choices(base_data(), include_categorical = FALSE)
    })

    shiny::observeEvent(sample_choices(), {
      samples <- sample_choices()
      selected <- preserve_multi_value(isolate(input$plot_a_samples), samples)

      update_selectize_if_needed(
        input_id = "plot_a_samples",
        choices = samples,
        selected = selected,
        choice_key = "plot_a_samples_choices",
        selected_key = "plot_a_samples_selected",
        server = TRUE
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(sample_choices(), {
      samples <- sample_choices()
      selected <- preserve_multi_value(isolate(input$plot_b_samples), samples)

      update_selectize_if_needed(
        input_id = "plot_b_samples",
        choices = samples,
        selected = selected,
        choice_key = "plot_b_samples_choices",
        selected_key = "plot_b_samples_selected",
        server = TRUE
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(channel_choices(), {
      channels <- channel_choices()
      selected <- preserve_single_value(isolate(input$plot_a_channel), channels)

      update_select_if_needed(
        input_id = "plot_a_channel",
        choices = channels,
        selected = selected,
        choice_key = "plot_a_channel_choices",
        selected_key = "plot_a_channel_selected"
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(plot_a_axis_choices(), plot_a_numeric_choices()), {
      x_choices <- plot_a_axis_choices()
      y_choices <- plot_a_numeric_choices()

      update_select_if_needed(
        input_id = "plot_a_x_col",
        choices = x_choices,
        selected = preserve_single_value(isolate(input$plot_a_x_col), x_choices, default = "sample"),
        choice_key = "plot_a_x_choices",
        selected_key = "plot_a_x_selected"
      )

      update_select_if_needed(
        input_id = "plot_a_y_col",
        choices = y_choices,
        selected = preserve_single_value(isolate(input$plot_a_y_col), y_choices, default = "rfu"),
        choice_key = "plot_a_y_choices",
        selected_key = "plot_a_y_selected"
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(channel_choices(), {
      channels <- channel_choices()
      channel_x_default <- if (length(channels) >= 1) unname(channels)[[1]] else NULL
      channel_y_default <- if (length(channels) >= 2) unname(channels)[[2]] else channel_x_default

      update_select_if_needed(
        input_id = "plot_b_channel_x",
        choices = channels,
        selected = preserve_single_value(isolate(input$plot_b_channel_x), channels, default = channel_x_default),
        choice_key = "plot_b_channel_x_choices",
        selected_key = "plot_b_channel_x_selected"
      )

      update_select_if_needed(
        input_id = "plot_b_channel_y",
        choices = channels,
        selected = preserve_single_value(isolate(input$plot_b_channel_y), channels, default = channel_y_default),
        choice_key = "plot_b_channel_y_choices",
        selected_key = "plot_b_channel_y_selected"
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(input$plot_b_channel_x, input$plot_b_channel_y), {
      new_x_label <- default_channel_axis_label(input$plot_b_channel_x, axis_fallback = "X")
      new_y_label <- default_channel_axis_label(input$plot_b_channel_y, axis_fallback = "Y")

      current_x_label <- isolate(input$plot_b_x_label)
      current_y_label <- isolate(input$plot_b_y_label)

      if (is.null(current_x_label) || identical(current_x_label, "") || identical(current_x_label, ui_state$plot_b_auto_x_label)) {
        shiny::updateTextInput(session, "plot_b_x_label", value = new_x_label)
      }

      if (is.null(current_y_label) || identical(current_y_label, "") || identical(current_y_label, ui_state$plot_b_auto_y_label)) {
        shiny::updateTextInput(session, "plot_b_y_label", value = new_y_label)
      }

      ui_state$plot_b_auto_x_label <- new_x_label
      ui_state$plot_b_auto_y_label <- new_y_label
    }, ignoreInit = FALSE)

    plot_a_data <- shiny::reactive({
      build_plot_a_data(
        base_data(),
        samples = input$plot_a_samples,
        channel = input$plot_a_channel
      )
    })

    plot_a_threshold_value <- shiny::reactive({
      suppressWarnings(as.numeric(input$plot_a_threshold))
    })

    plot_a_table_data <- shiny::reactive({
      build_plot_a_table(
        plot_a_data(),
        y_col = input$plot_a_y_col,
        threshold_value = plot_a_threshold_value()
      )
    })

    plot_a_plot_data <- shiny::reactive({
      df <- plot_a_data()
      y_col <- input$plot_a_y_col
      threshold_value <- plot_a_threshold_value()

      if (nrow(df) == 0 || !y_col %in% names(df)) {
        return(df)
      }

      values <- suppressWarnings(as.numeric(df[[y_col]]))
      df$threshold_status <- ifelse(
        is.finite(values) & is.finite(threshold_value) & values >= threshold_value,
        "Oberhalb Threshold",
        "Unterhalb Threshold"
      )
      df
    })

    plot_a_sampling <- shiny::reactive({
      prepare_interactive_plot_data(
        plot_a_plot_data(),
        max_points = input$plot_a_max_points
      )
    })

    plot_a_static_plot <- shiny::reactive({
      build_sample_analysis_scatter_plot(
        plot_a_plot_data(),
        settings = list(
          title = input$plot_a_title,
          subtitle = input$plot_a_subtitle,
          x_col = input$plot_a_x_col,
          y_col = input$plot_a_y_col,
          x_label = input$plot_a_x_label,
          y_label = input$plot_a_y_label,
          x_text_size = input$plot_a_x_size,
          y_text_size = input$plot_a_y_size,
          color_by = input$plot_a_color_by,
          alpha = input$scatter_alpha,
          threshold_enabled = input$plot_a_threshold_enabled,
          threshold_y = plot_a_threshold_value(),
          threshold_x = NULL
        )
      )
    })

    plot_a_interactive_plot <- shiny::reactive({
      sampled <- plot_a_sampling()
      build_sample_analysis_scatter_plot(
        sampled$data,
        settings = list(
          title = input$plot_a_title,
          subtitle = input$plot_a_subtitle,
          x_col = input$plot_a_x_col,
          y_col = input$plot_a_y_col,
          x_label = input$plot_a_x_label,
          y_label = input$plot_a_y_label,
          x_text_size = input$plot_a_x_size,
          y_text_size = input$plot_a_y_size,
          color_by = input$plot_a_color_by,
          alpha = input$scatter_alpha,
          threshold_enabled = input$plot_a_threshold_enabled,
          threshold_y = plot_a_threshold_value(),
          threshold_x = NULL
        )
      )
    })

    output$plot_a_sampling_info <- shiny::renderUI({
      build_sampling_info_message(plot_a_sampling(), use_webgl = isTRUE(input$plot_a_use_webgl))
    })

    output$plot_a_plot <- shiny::renderPlot({
      plot_a_static_plot()
    }, res = 96)

    output$plot_a_plotly <- plotly::renderPlotly({
      make_interactive_plot(
        plot_a_interactive_plot(),
        tooltip = "text",
        use_webgl = isTRUE(input$plot_a_use_webgl)
      )
    })

    output$plot_a_table <- DT::renderDT({
      table_df <- plot_a_table_data()
      is_summary <- nrow(table_df) > 0

      if (!is_summary) {
        table_df <- tibble::tibble(Hinweis = "Keine Daten fuer Plot A verfuegbar")
      }

      table_dt <- DT::datatable(
        table_df,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 8, scrollX = TRUE)
      )

      if (is_summary) {
        table_dt <- table_dt |>
          DT::formatRound(columns = c("positiv_prozent", "negativ_prozent"), digits = 2)
      }

      table_dt
    })

    shiny::observeEvent(input$plot_a_auto_threshold, {
      df <- plot_a_data()
      y_col <- input$plot_a_y_col

      if (nrow(df) == 0 || !y_col %in% names(df)) {
        shiny::showNotification("Zu wenig Daten fuer automatische Threshold-Berechnung in Plot A.", type = "error")
        return()
      }

      threshold <- compute_auto_threshold(df[[y_col]])
      if (!is.finite(threshold)) {
        shiny::showNotification("Automatischer Threshold fuer Plot A konnte nicht bestimmt werden.", type = "error")
        return()
      }

      shiny::updateNumericInput(session, "plot_a_threshold", value = round(threshold, 3))
      shiny::showNotification("Threshold fuer Plot A aktualisiert.", type = "message")
    })

    plot_b_samples <- shiny::reactive({
      input$plot_b_samples
    })

    plot_b_data <- shiny::reactive({
      build_plot_b_data(
        base_data(),
        channel_x = input$plot_b_channel_x,
        channel_y = input$plot_b_channel_y,
        samples = plot_b_samples()
      )
    })

    plot_b_threshold_x <- shiny::reactive({
      suppressWarnings(as.numeric(input$plot_b_threshold_x))
    })

    plot_b_threshold_y <- shiny::reactive({
      suppressWarnings(as.numeric(input$plot_b_threshold_y))
    })

    plot_b_table_data <- shiny::reactive({
      build_plot_b_table(
        plot_b_data(),
        threshold_x = plot_b_threshold_x(),
        threshold_y = plot_b_threshold_y()
      )
    })

    plot_b_plot_data <- shiny::reactive({
      df <- plot_b_data()

      if (nrow(df) == 0) {
        return(df)
      }

      df$channel <- paste(input$plot_b_channel_x, input$plot_b_channel_y, sep = " vs ")
      df$threshold_status <- dplyr::case_when(
        df$x_value >= plot_b_threshold_x() & df$y_value >= plot_b_threshold_y() ~ "x>=Tx, y>=Ty",
        df$x_value >= plot_b_threshold_x() & df$y_value < plot_b_threshold_y() ~ "x>=Tx, y<Ty",
        df$x_value < plot_b_threshold_x() & df$y_value >= plot_b_threshold_y() ~ "x<Tx, y>=Ty",
        TRUE ~ "x<Tx, y<Ty"
      )

      df
    })

    plot_b_sampling <- shiny::reactive({
      prepare_interactive_plot_data(
        plot_b_plot_data(),
        max_points = input$plot_b_max_points
      )
    })

    plot_b_static_plot <- shiny::reactive({
      build_sample_analysis_scatter_plot(
        plot_b_plot_data(),
        settings = list(
          title = input$plot_b_title,
          subtitle = input$plot_b_subtitle,
          x_col = "x_value",
          y_col = "y_value",
          x_label = input$plot_b_x_label,
          y_label = input$plot_b_y_label,
          x_text_size = input$plot_b_x_size,
          y_text_size = input$plot_b_y_size,
          color_by = input$plot_b_color_by,
          alpha = input$scatter_alpha,
          threshold_enabled = input$plot_b_threshold_enabled,
          threshold_y = plot_b_threshold_y(),
          threshold_x = plot_b_threshold_x()
        )
      )
    })

    plot_b_interactive_plot <- shiny::reactive({
      sampled <- plot_b_sampling()
      build_sample_analysis_scatter_plot(
        sampled$data,
        settings = list(
          title = input$plot_b_title,
          subtitle = input$plot_b_subtitle,
          x_col = "x_value",
          y_col = "y_value",
          x_label = input$plot_b_x_label,
          y_label = input$plot_b_y_label,
          x_text_size = input$plot_b_x_size,
          y_text_size = input$plot_b_y_size,
          color_by = input$plot_b_color_by,
          alpha = input$scatter_alpha,
          threshold_enabled = input$plot_b_threshold_enabled,
          threshold_y = plot_b_threshold_y(),
          threshold_x = plot_b_threshold_x()
        )
      )
    })

    output$plot_b_sampling_info <- shiny::renderUI({
      build_sampling_info_message(plot_b_sampling(), use_webgl = isTRUE(input$plot_b_use_webgl))
    })

    output$plot_b_plot <- shiny::renderPlot({
      if (identical(input$plot_b_channel_x, input$plot_b_channel_y)) {
        return(build_empty_plot("Bitte zwei unterschiedliche Kanaele fuer Plot B waehlen."))
      }

      plot_b_static_plot()
    }, res = 96)

    output$plot_b_plotly <- plotly::renderPlotly({
      if (identical(input$plot_b_channel_x, input$plot_b_channel_y)) {
        return(plotly::ggplotly(build_empty_plot("Bitte zwei unterschiedliche Kanaele fuer Plot B waehlen.")))
      }

      make_interactive_plot(
        plot_b_interactive_plot(),
        tooltip = "text",
        use_webgl = isTRUE(input$plot_b_use_webgl)
      )
    })

    output$plot_b_table <- DT::renderDT({
      table_df <- plot_b_table_data()

      if (nrow(table_df) == 0) {
        table_df <- tibble::tibble(Hinweis = "Keine Daten fuer Plot B verfuegbar")
      }

      DT::datatable(
        table_df,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$plot_b_auto_threshold, {
      df <- plot_b_data()

      if (nrow(df) == 0) {
        shiny::showNotification("Zu wenig Daten fuer automatische Threshold-Berechnung in Plot B.", type = "error")
        return()
      }

      threshold_x <- compute_auto_threshold(df$x_value)
      threshold_y <- compute_auto_threshold(df$y_value)

      if (!is.finite(threshold_x) || !is.finite(threshold_y)) {
        shiny::showNotification("Automatische Thresholds fuer Plot B konnten nicht bestimmt werden.", type = "error")
        return()
      }

      shiny::updateNumericInput(session, "plot_b_threshold_x", value = round(threshold_x, 3))
      shiny::updateNumericInput(session, "plot_b_threshold_y", value = round(threshold_y, 3))
      shiny::showNotification("Thresholds fuer Plot B aktualisiert.", type = "message")
    })
  })
}
