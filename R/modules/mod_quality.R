mod_quality_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 4,
        title = "Qualitätsfilter",
        status = "info",
        solidHeader = TRUE,
        shiny::selectInput(ns("plate_select"), "Plate", choices = c("Bitte laden Sie zunächst Daten" = "")),
        shiny::checkboxInput(ns("exclude_invalid"), "Ungültige Partitionen ausblenden", value = TRUE)
      ),
      shinydashboard::box(
        width = 8,
        title = "Qualitätsmetriken pro Well/Kanal",
        status = "primary",
        solidHeader = TRUE,
        DT::DTOutput(ns("quality_table"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 3,
        title = "Plot 1 Einstellungen",
        status = "warning",
        solidHeader = TRUE,
        shiny::textInput(ns("q1_title"), "Titel", value = "QC-Scatter: Partition vs RFU"),
        shiny::textInput(ns("q1_subtitle"), "Untertitel", value = "Qualitätsansicht pro Platte"),
        shiny::selectInput(ns("q1_x_col"), "X-Spalte", choices = c("partition", "rfu"), selected = "partition"),
        shiny::selectInput(ns("q1_y_col"), "Y-Spalte", choices = c("rfu", "threshold"), selected = "rfu"),
        shiny::textInput(ns("q1_x_label"), "X-Achsenlabel", value = "Partition"),
        shiny::textInput(ns("q1_y_label"), "Y-Achsenlabel", value = "RFU"),
        shiny::numericInput(ns("q1_x_size"), "Schriftgröße X", value = 12, min = 8, max = 24),
        shiny::numericInput(ns("q1_y_size"), "Schriftgröße Y", value = 12, min = 8, max = 24)
      ),
      shinydashboard::box(
        width = 9,
        title = "QC-Scatterplot",
        status = "primary",
        solidHeader = TRUE,
        shiny::tabsetPanel(
          shiny::tabPanel("ggplot", shiny::plotOutput(ns("q1_plot"), height = "460px")),
          shiny::tabPanel("plotly", plotly::plotlyOutput(ns("q1_plotly"), height = "460px"))
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 3,
        title = "Plot 2 Einstellungen",
        status = "warning",
        solidHeader = TRUE,
        shiny::textInput(ns("q2_title"), "Titel", value = "Verteilung numerischer QC-Werte"),
        shiny::textInput(ns("q2_subtitle"), "Untertitel", value = "Histogramm für Qualitätsprüfung"),
        shiny::selectInput(ns("q2_x_col"), "X-Spalte", choices = c("rfu", "partition"), selected = "rfu"),
        shiny::selectInput(ns("q2_fill_by"), "Füllfarbe nach", choices = c("channel", "sample", "well", "none"), selected = "channel"),
        shiny::textInput(ns("q2_x_label"), "X-Achsenlabel", value = "RFU"),
        shiny::textInput(ns("q2_y_label"), "Y-Achsenlabel", value = "Anzahl Partitionen"),
        shiny::numericInput(ns("q2_x_size"), "Schriftgröße X", value = 12, min = 8, max = 24),
        shiny::numericInput(ns("q2_y_size"), "Schriftgröße Y", value = 12, min = 8, max = 24),
        shiny::numericInput(ns("q2_bins"), "Anzahl Bins", value = 40, min = 10, max = 200)
      ),
      shinydashboard::box(
        width = 9,
        title = "QC-Histogramm",
        status = "primary",
        solidHeader = TRUE,
        shiny::tabsetPanel(
          shiny::tabPanel("ggplot", shiny::plotOutput(ns("q2_plot"), height = "460px")),
          shiny::tabPanel("plotly", plotly::plotlyOutput(ns("q2_plotly"), height = "460px"))
        )
      )
    )
  )
}

mod_quality_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
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

    shiny::observe({
      shiny::updateSelectInput(
        session,
        "plate_select",
        choices = plate_choices(),
        selected = plate_choices()[[1]]
      )
    })

    filtered_data <- shiny::reactive({
      df <- state$dpcr_data
      if (is.null(df) || nrow(df) == 0) {
        return(new_empty_dpcr_data())
      }

      selected_plate <- input$plate_select
      if (!is.null(selected_plate) && selected_plate != "" && selected_plate != "__all__") {
        df <- dplyr::filter(df, plate_name == selected_plate | plate_id == selected_plate)
      }

      if (isTRUE(input$exclude_invalid)) {
        df <- dplyr::filter(df, is.na(invalid_partition) | !invalid_partition)
      }

      df
    })

    shiny::observe({
      cols <- available_numeric_columns(filtered_data())
      if (length(cols) == 0) {
        cols <- c("partition", "rfu")
      }

      shiny::updateSelectInput(session, "q1_x_col", choices = cols, selected = if ("partition" %in% cols) "partition" else cols[[1]])
      shiny::updateSelectInput(session, "q1_y_col", choices = cols, selected = if ("rfu" %in% cols) "rfu" else cols[[1]])
      shiny::updateSelectInput(session, "q2_x_col", choices = cols, selected = if ("rfu" %in% cols) "rfu" else cols[[1]])
    })

    quality_metrics <- shiny::reactive({
      df <- filtered_data()
      if (nrow(df) == 0) {
        return(tibble::tibble())
      }

      metrics <- df |>
        dplyr::group_by(well, sample, channel) |>
        dplyr::summarise(
          partitionen = dplyr::n(),
          rfu_mittel = mean(rfu, na.rm = TRUE),
          rfu_sd = stats::sd(rfu, na.rm = TRUE),
          invalid_rate = mean(invalid_partition, na.rm = TRUE),
          positiv_rate = mean(positive_control, na.rm = TRUE),
          .groups = "drop"
        )

      metrics$invalid_rate[is.nan(metrics$invalid_rate)] <- NA_real_
      metrics$positiv_rate[is.nan(metrics$positiv_rate)] <- NA_real_
      metrics
    })

    output$quality_table <- DT::renderDT({
      metrics <- quality_metrics()

      DT::datatable(
        metrics,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    q1_plot <- shiny::reactive({
      build_scatter_plot(
        filtered_data(),
        settings = list(
          title = input$q1_title,
          subtitle = input$q1_subtitle,
          x_col = input$q1_x_col,
          y_col = input$q1_y_col,
          x_label = input$q1_x_label,
          y_label = input$q1_y_label,
          x_text_size = input$q1_x_size,
          y_text_size = input$q1_y_size,
          color_by = "channel",
          alpha = 0.55
        )
      )
    })

    output$q1_plot <- shiny::renderPlot({
      q1_plot()
    }, res = 96)

    output$q1_plotly <- plotly::renderPlotly({
      plotly::ggplotly(q1_plot(), tooltip = "text")
    })

    q2_plot <- shiny::reactive({
      build_histogram_plot(
        filtered_data(),
        settings = list(
          title = input$q2_title,
          subtitle = input$q2_subtitle,
          x_col = input$q2_x_col,
          fill_by = input$q2_fill_by,
          x_label = input$q2_x_label,
          y_label = input$q2_y_label,
          x_text_size = input$q2_x_size,
          y_text_size = input$q2_y_size,
          bins = input$q2_bins
        )
      )
    })

    output$q2_plot <- shiny::renderPlot({
      q2_plot()
    }, res = 96)

    output$q2_plotly <- plotly::renderPlotly({
      plotly::ggplotly(q2_plot())
    })
  })
}
