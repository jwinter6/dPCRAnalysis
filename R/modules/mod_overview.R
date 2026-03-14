mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Platte auswählen",
        status = "info",
        solidHeader = TRUE,
        shiny::selectInput(ns("plate_select"), "Plate", choices = c("Bitte laden Sie zunächst Daten" = ""))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 3,
        title = "Plot-Parameter",
        status = "warning",
        solidHeader = TRUE,
        shiny::textInput(ns("plot_title"), "Titel", value = "Scatterplot: Partition vs RFU"),
        shiny::textInput(ns("plot_subtitle"), "Untertitel", value = "Interaktive Übersicht"),
        shiny::selectInput(ns("x_col"), "X-Spalte", choices = c("partition", "rfu"), selected = "partition"),
        shiny::selectInput(ns("y_col"), "Y-Spalte", choices = c("rfu", "partition"), selected = "rfu"),
        shiny::selectInput(
          ns("color_by"),
          "Farbe nach",
          choices = c("channel", "reference", "sample", "plate_name", "none"),
          selected = "channel"
        ),
        shiny::textInput(ns("x_label"), "X-Achsenlabel", value = "Partition"),
        shiny::textInput(ns("y_label"), "Y-Achsenlabel", value = "RFU"),
        shiny::numericInput(ns("x_size"), "Schriftgröße X", value = 12, min = 8, max = 24, step = 1),
        shiny::numericInput(ns("y_size"), "Schriftgröße Y", value = 12, min = 8, max = 24, step = 1),
        shiny::sliderInput(
          ns("scatter_alpha"),
          "Alpha-Wert Punkte",
          min = 0.05,
          max = 1,
          value = SCATTER_ALPHA_DEFAULT,
          step = 0.05
        ),
        shiny::numericInput(
          ns("max_interactive_points"),
          "Max. Punkte (Interaktiver Plot)",
          value = 60000,
          min = 10000,
          max = 1000000,
          step = 10000
        ),
        shiny::checkboxInput(
          ns("use_webgl"),
          "WebGL für Interaktiven Plot nutzen",
          value = PLOTLY_USE_WEBGL_DEFAULT
        ),
        shiny::helpText("Große Datensätze werden für den interaktiven Plot automatisch reduziert (Downsampling).")
      ),
      shinydashboard::box(
        width = 9,
        title = "Scatterplot",
        status = "primary",
        solidHeader = TRUE,
        shiny::uiOutput(ns("plotly_sampling_info")),
        shiny::tabsetPanel(
          shiny::tabPanel("Plot", shiny::plotOutput(ns("scatter_plot"), height = "520px")),
          shiny::tabPanel("Interaktiver Plot", plotly::plotlyOutput(ns("scatter_plotly"), height = "520px"))
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Plattendaten (Tabelle)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        DT::DTOutput(ns("overview_table"))
      )
    )
  )
}

mod_overview_server <- function(id, state) {
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
      if (is.null(selected_plate) || selected_plate == "" || selected_plate == "__all__") {
        return(df)
      }

      dplyr::filter(
        df,
        plate_name == selected_plate | plate_id == selected_plate
      )
    })

    shiny::observe({
      cols <- available_numeric_columns(filtered_data())
      if (length(cols) == 0) {
        cols <- c("partition", "rfu")
      }

      shiny::updateSelectInput(session, "x_col", choices = cols, selected = if ("partition" %in% cols) "partition" else cols[[1]])
      shiny::updateSelectInput(session, "y_col", choices = cols, selected = if ("rfu" %in% cols) "rfu" else cols[[1]])
    })

    output$overview_table <- DT::renderDT({
      df <- filtered_data()

      if (nrow(df) == 0) {
        return(
          DT::datatable(
            empty_issues_table(),
            rownames = FALSE,
            options = list(dom = "t")
          )
        )
      }

      DT::datatable(
        df,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    scatter_settings <- shiny::reactive({
      list(
        title = input$plot_title,
        subtitle = input$plot_subtitle,
        x_col = input$x_col,
        y_col = input$y_col,
        x_label = input$x_label,
        y_label = input$y_label,
        x_text_size = input$x_size,
        y_text_size = input$y_size,
        color_by = input$color_by,
        alpha = input$scatter_alpha
      )
    })

    scatter_plot_reactive <- shiny::reactive({
      build_scatter_plot(filtered_data(), scatter_settings())
    })

    interactive_scatter_context <- shiny::reactive({
      sampled <- prepare_interactive_plot_data(
        filtered_data(),
        max_points = input$max_interactive_points
      )

      list(
        plot = build_scatter_plot(sampled$data, scatter_settings()),
        sampled = sampled$sampled,
        original_n = sampled$original_n,
        used_n = sampled$used_n
      )
    })

    output$plotly_sampling_info <- shiny::renderUI({
      info <- interactive_scatter_context()

      if (!isTRUE(info$sampled)) {
        return(NULL)
      }

      mode_label <- if (isTRUE(input$use_webgl)) {
        "Downsampling + WebGL"
      } else {
        "Downsampling (ohne WebGL)"
      }

      shiny::tags$div(
        class = "alert alert-info",
        sprintf(
          "Interaktiver Plot optimiert: %s von %s Punkten dargestellt (%s).",
          format(info$used_n, big.mark = ".", scientific = FALSE),
          format(info$original_n, big.mark = ".", scientific = FALSE),
          mode_label
        )
      )
    })

    output$scatter_plot <- shiny::renderPlot({
      scatter_plot_reactive()
    }, res = 96)

    output$scatter_plotly <- plotly::renderPlotly({
      info <- interactive_scatter_context()
      make_interactive_plot(info$plot, tooltip = "text", use_webgl = isTRUE(input$use_webgl))
    })
  })
}
