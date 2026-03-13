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
        shiny::checkboxInput(ns("exclude_invalid"), "Ungültige Partitionen in Plot 1/2 ausblenden", value = TRUE),
        shiny::numericInput(ns("min_accepted"), "Minimum akzeptierte Partitionen (n)", value = 10000, min = 1000, step = 500),
        shiny::numericInput(ns("rain_limit_percent"), "Rain-Grenzwert (%)", value = 2.5, min = 0, max = 100, step = 0.1),
        shiny::numericInput(ns("rain_band_percent"), "Rain-Band um Threshold (%)", value = 10, min = 1, max = 50, step = 1),
        shiny::sliderInput(ns("conf_level"), "Konfidenzniveau", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        shiny::uiOutput(ns("dpcr_density_info"))
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
        width = 12,
        title = "QC-Hinweise (dMIQE2020-orientiert)",
        status = "warning",
        solidHeader = TRUE,
        shiny::uiOutput(ns("qc_flags_ui"))
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
          shiny::tabPanel("Plot", shiny::plotOutput(ns("q1_plot"), height = "420px")),
          shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("q1_plotly"), height = "420px"))
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
          shiny::tabPanel("Plot", shiny::plotOutput(ns("q2_plot"), height = "420px")),
          shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("q2_plotly"), height = "420px"))
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 3,
        title = "dpcR Dichteparameter",
        status = "warning",
        solidHeader = TRUE,
        shiny::selectInput(ns("q3_target"), "Well/Sample/Kanal", choices = c("Bitte laden Sie zunächst Daten" = "")),
        shiny::radioButtons(
          ns("q3_density_mode"),
          "Dichte berechnen für",
          choices = c("Lambda (λ, average = TRUE)" = "lambda", "Positive Moleküle (average = FALSE)" = "positive"),
          selected = "lambda"
        ),
        shiny::selectInput(
          ns("q3_density_method"),
          "CI-Methode (dpcR)",
          multiple = FALSE,
          choices = c(
            "wilson", "agresti-coull", "exact", "prop.test", "profile",
            "lrt", "asymptotic", "bayes", "cloglog", "logit", "probit"
          ),
          selected = "wilson"
        ),
        shiny::checkboxInput(ns("q3_density_bars"), "Diskrete Balken einblenden", value = FALSE),
        shiny::textInput(ns("q3_density_title"), "Titel Dichteplot", value = "dpcR-Dichteplot"),
        shiny::textInput(ns("q3_density_subtitle"), "Untertitel Dichteplot", value = "Konfidenzintervall je ausgewähltem Well/Kanal"),
        shiny::radioButtons(
          ns("q3_metric"),
          "Metrik für CI-Übersichtsplot",
          choices = c("Lambda (λ)" = "lambda", "Positive Partitionen (%)" = "positive_fraction"),
          selected = "lambda"
        )
      ),
      shinydashboard::box(
        width = 9,
        title = "Dichteplot (dpcR::dpcr_density)",
        status = "primary",
        solidHeader = TRUE,
        shiny::tabsetPanel(
          shiny::tabPanel("Plot", shiny::plotOutput(ns("q3_density_plot"), height = "420px")),
          shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("q3_density_plotly"), height = "420px"))
        ),
        shiny::br(),
        DT::DTOutput(ns("q3_density_table"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Konfidenzplot (λ / Positive Partitionen)",
        status = "primary",
        solidHeader = TRUE,
        shiny::tabsetPanel(
          shiny::tabPanel("Plot", shiny::plotOutput(ns("q3_ci_plot"), height = "520px")),
          shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("q3_ci_plotly"), height = "520px"))
        )
      )
    )
  )
}

mod_quality_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    empty_plotly_message <- function(msg) {
      plotly::plot_ly(type = "scatter", mode = "markers", x = c(0), y = c(0), marker = list(opacity = 0)) |>
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE)
          )
        )
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

    shiny::observe({
      shiny::updateSelectInput(
        session,
        "plate_select",
        choices = plate_choices(),
        selected = plate_choices()[[1]]
      )
    })

    plate_filtered_data <- shiny::reactive({
      df <- state$dpcr_data
      if (is.null(df) || nrow(df) == 0) {
        return(new_empty_dpcr_data())
      }

      selected_plate <- input$plate_select
      if (!is.null(selected_plate) && selected_plate != "" && selected_plate != "__all__") {
        df <- dplyr::filter(df, plate_name == selected_plate | plate_id == selected_plate)
      }

      df
    })

    plot_filtered_data <- shiny::reactive({
      df <- plate_filtered_data()

      if (isTRUE(input$exclude_invalid)) {
        df <- dplyr::filter(df, is.na(invalid_partition) | !invalid_partition)
      }

      df
    })

    shiny::observe({
      cols <- available_numeric_columns(plot_filtered_data())
      if (length(cols) == 0) {
        cols <- c("partition", "rfu")
      }

      shiny::updateSelectInput(session, "q1_x_col", choices = cols, selected = if ("partition" %in% cols) "partition" else cols[[1]])
      shiny::updateSelectInput(session, "q1_y_col", choices = cols, selected = if ("rfu" %in% cols) "rfu" else cols[[1]])
      shiny::updateSelectInput(session, "q2_x_col", choices = cols, selected = if ("rfu" %in% cols) "rfu" else cols[[1]])
    })

    quality_metrics <- shiny::reactive({
      summarize_quality_qc(
        plate_filtered_data(),
        conf_level = input$conf_level,
        rain_band_fraction = input$rain_band_percent / 100,
        min_accepted = input$min_accepted,
        rain_limit_fraction = input$rain_limit_percent / 100
      )
    })

    shiny::observe({
      metrics <- quality_metrics()

      if (nrow(metrics) == 0) {
        shiny::updateSelectInput(session, "q3_target", choices = c("Bitte laden Sie zunächst Daten" = ""), selected = "")
        return()
      }

      labels <- paste0(
        metrics$well, " | ", metrics$sample, " | ", metrics$channel,
        " (n=", metrics$n_accepted, ", k=", metrics$n_positive, ")"
      )
      choices <- stats::setNames(metrics$qc_id, labels)

      selected <- input$q3_target
      if (is.null(selected) || !selected %in% choices) {
        selected <- choices[[1]]
      }

      shiny::updateSelectInput(session, "q3_target", choices = choices, selected = selected)
    })

    selected_density_row <- shiny::reactive({
      metrics <- quality_metrics()
      if (nrow(metrics) == 0) {
        return(tibble::tibble())
      }

      selected <- input$q3_target
      out <- dplyr::filter(metrics, qc_id == selected)

      if (nrow(out) == 0) {
        return(metrics[1, , drop = FALSE])
      }

      out[1, , drop = FALSE]
    })

    density_average <- shiny::reactive({
      identical(input$q3_density_mode, "lambda")
    })

    density_result <- shiny::reactive({
      row <- selected_density_row()
      if (nrow(row) == 0) {
        return(NULL)
      }

      if (!is_dpcr_density_available()) {
        return(NULL)
      }

      if (is.na(row$n_accepted[[1]]) || row$n_accepted[[1]] <= 0) {
        return(NULL)
      }

      tryCatch(
        run_dpcr_density(
          k = row$n_positive[[1]],
          n = row$n_accepted[[1]],
          average = density_average(),
          method = input$q3_density_method,
          conf_level = input$conf_level,
          plot = FALSE,
          bars = input$q3_density_bars
        ),
        error = function(e) NULL
      )
    })

    output$dpcr_density_info <- shiny::renderUI({
      if (is_dpcr_density_available()) {
        shiny::tags$div(
          class = "alert alert-success",
          "dpcR erkannt: Dichteplot wird direkt mit dpcR::dpcr_density berechnet."
        )
      } else {
        shiny::tags$div(
          class = "alert alert-secondary",
          "dpcR ist nicht installiert. Es wird ein äquivalenter Poisson/Binomial-Ansatz mit Konfidenzintervallen verwendet."
        )
      }
    })

    output$qc_flags_ui <- shiny::renderUI({
      metrics <- quality_metrics()
      flags <- summarize_qc_flags(
        metrics,
        min_accepted = input$min_accepted,
        rain_limit_fraction = input$rain_limit_percent / 100
      )

      if (flags$wells_total == 0) {
        return(shiny::tags$div(class = "alert alert-light", "Noch keine QC-Daten verfügbar."))
      }

      shiny::tagList(
        shiny::tags$div(
          class = "alert alert-info",
          sprintf(
            "Wells gesamt: %s | Wells mit n < %s: %s | Wells mit Rain > %.1f%%: %s",
            format(flags$wells_total, big.mark = "."),
            format(as.integer(input$min_accepted), big.mark = "."),
            format(flags$wells_low_accepted, big.mark = "."),
            input$rain_limit_percent,
            format(flags$wells_high_rain, big.mark = ".")
          )
        ),
        shiny::tags$p(
          class = "text-muted",
          "Hinweis: dMIQE2020/Bio-Rad-orientierter QC-Check (n akzeptierte Partitionen, Rain-Anteil)."
        )
      )
    })

    output$quality_table <- DT::renderDT({
      metrics <- quality_metrics()

      if (nrow(metrics) == 0) {
        return(DT::datatable(tibble::tibble(Hinweis = "Bitte laden Sie zunächst Daten"), options = list(dom = "t"), rownames = FALSE))
      }

      shown <- metrics |>
        dplyr::select(
          plate_name,
          well,
          sample,
          channel,
          n_total,
          n_accepted,
          n_positive,
          positive_percent,
          lambda,
          lambda_ci_low,
          lambda_ci_high,
          rain_percent,
          accepted_qc,
          rain_qc,
          qc_status
        )

      DT::datatable(
        shown,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      ) |>
        DT::formatRound(columns = c("positive_percent", "lambda", "lambda_ci_low", "lambda_ci_high", "rain_percent"), digits = 3)
    })

    q1_plot <- shiny::reactive({
      build_scatter_plot(
        plot_filtered_data(),
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
      make_interactive_plot(q1_plot(), tooltip = "text", use_webgl = PLOTLY_USE_WEBGL_DEFAULT)
    })

    q2_plot <- shiny::reactive({
      build_histogram_plot(
        plot_filtered_data(),
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
      make_interactive_plot(q2_plot(), tooltip = c("x", "y"), use_webgl = FALSE)
    })

    output$q3_density_plot <- shiny::renderPlot({
      row <- selected_density_row()

      if (nrow(row) == 0 || row$n_accepted[[1]] <= 0) {
        print(build_empty_plot("Keine Daten für dpcR-Dichteplot verfügbar"))
        return()
      }

      if (!is_dpcr_density_available()) {
        print(build_empty_plot("dpcR ist nicht verfügbar"))
        return()
      }

      sub <- sprintf(
        "%s | Well: %s | Sample: %s | Kanal: %s | n=%s, k=%s",
        input$q3_density_subtitle,
        row$well[[1]],
        row$sample[[1]],
        row$channel[[1]],
        row$n_accepted[[1]],
        row$n_positive[[1]]
      )

      p <- tryCatch(
        build_dpcr_density_plot(
          k = row$n_positive[[1]],
          n = row$n_accepted[[1]],
          average = density_average(),
          method = input$q3_density_method,
          conf_level = input$conf_level,
          bars = input$q3_density_bars,
          title = input$q3_density_title,
          subtitle = sub
        ),
        error = function(e) {
          build_empty_plot(paste("dpcR-Fehler:", e$message))
        }
      )

      print(p)
    }, res = 96)

    output$q3_density_table <- DT::renderDT({
      row <- selected_density_row()
      ci <- density_result()

      if (nrow(row) == 0 || is.null(ci)) {
        return(
          DT::datatable(
            tibble::tibble(Hinweis = "Keine dpcR-Dichteergebnisse verfügbar"),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }

      summary_df <- extract_dpcr_density_interval(ci, average = density_average()) |>
        dplyr::mutate(
          plate_name = row$plate_name[[1]],
          well = row$well[[1]],
          sample = row$sample[[1]],
          channel = row$channel[[1]],
          mode = if (density_average()) "lambda" else "positive_molecules",
          conf_level = input$conf_level
        ) |>
        dplyr::select(
          plate_name,
          well,
          sample,
          channel,
          mode,
          method,
          k,
          n,
          estimate,
          lower,
          upper,
          conf_level
        )

      DT::datatable(
        summary_df,
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      ) |>
        DT::formatRound(columns = c("estimate", "lower", "upper", "conf_level"), digits = 4)
    })

    output$q3_density_plotly <- plotly::renderPlotly({
      row <- selected_density_row()
      ci <- density_result()

      if (nrow(row) == 0 || is.null(ci)) {
        return(empty_plotly_message("Keine dpcR-Dichtewerte verfügbar"))
      }

      ci_df <- extract_dpcr_density_interval(ci, average = density_average())
      if (nrow(ci_df) == 0 || is.na(ci_df$estimate[[1]])) {
        return(empty_plotly_message("dpcR-Dichte konnte nicht berechnet werden"))
      }

      est <- ci_df$estimate[[1]]
      low <- ci_df$lower[[1]]
      high <- ci_df$upper[[1]]
      y_label <- if (density_average()) "Lambda (λ)" else "Positive Moleküle"

      plotly::plot_ly(
        x = c(est),
        y = c(1),
        type = "scatter",
        mode = "markers",
        marker = list(size = 12, color = "#24527a"),
        error_x = list(
          type = "data",
          symmetric = FALSE,
          array = c(high - est),
          arrayminus = c(est - low),
          color = "#24527a",
          thickness = 2
        ),
        text = paste0(
          "Well: ", row$well[[1]],
          "<br>Sample: ", row$sample[[1]],
          "<br>Kanal: ", row$channel[[1]],
          "<br>Estimate: ", signif(est, 5),
          "<br>CI: [", signif(low, 5), "; ", signif(high, 5), "]",
          "<br>k = ", row$n_positive[[1]],
          "<br>n = ", row$n_accepted[[1]]
        ),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          title = list(text = "Interaktive CI-Ansicht des dpcR-Dichteergebnisses"),
          xaxis = list(title = y_label, zeroline = FALSE),
          yaxis = list(title = "", visible = FALSE, zeroline = FALSE),
          showlegend = FALSE
        )
    })

    q3_ci_plot <- shiny::reactive({
      build_lambda_ci_plot(
        quality_metrics(),
        metric = input$q3_metric,
        title = "Konfidenzintervalle je Well/Kanal",
        subtitle = sprintf("Konfidenzniveau: %.0f%%", 100 * input$conf_level)
      )
    })

    output$q3_ci_plot <- shiny::renderPlot({
      q3_ci_plot()
    }, res = 96)

    output$q3_ci_plotly <- plotly::renderPlotly({
      make_interactive_plot(q3_ci_plot(), tooltip = "text", use_webgl = FALSE)
    })
  })
}
