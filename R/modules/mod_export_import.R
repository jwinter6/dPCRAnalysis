mod_export_import_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 6,
        title = "Analyse exportieren",
        status = "primary",
        solidHeader = TRUE,
        shiny::p("Speichert den aktuellen Stand als .RData (inkl. Daten, Validierungsreport, Metadaten)."),
        shiny::downloadButton(ns("download_rdata"), "Exportieren")
      ),
      shinydashboard::box(
        width = 6,
        title = "Analyse laden",
        status = "warning",
        solidHeader = TRUE,
        shiny::fileInput(ns("upload_rdata"), "Analyse laden", accept = c(".RData")),
        shiny::actionButton(ns("load_btn"), "Analyse laden", class = "btn-primary")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Aktuelle Analyse-Zusammenfassung",
        status = "info",
        solidHeader = TRUE,
        DT::DTOutput(ns("summary_table"))
      )
    )
  )
}

mod_export_import_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$download_rdata <- shiny::downloadHandler(
      filename = function() {
        paste0("dpcr_analyse_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData")
      },
      content = function(file) {
        if (is.null(state$dpcr_data) || nrow(state$dpcr_data) == 0) {
          stop("Bitte laden Sie zunächst Daten.")
        }

        save_analysis_rdata(
          path = file,
          dpcr_data = state$dpcr_data,
          validation_report = state$validation_report,
          metadata = state$metadata
        )
      }
    )

    shiny::observeEvent(input$load_btn, {
      shiny::req(input$upload_rdata)

      loaded <- tryCatch(
        load_analysis_rdata(input$upload_rdata$datapath),
        error = function(e) {
          shiny::showNotification(sprintf("Import fehlgeschlagen: %s", e$message), type = "error")
          NULL
        }
      )

      if (is.null(loaded)) {
        return()
      }

      state$dpcr_data <- loaded$dpcr_data
      state$validation_report <- loaded$validation_report
      state$metadata <- c(
        loaded$metadata,
        list(
          loaded_at = Sys.time(),
          source = "RData"
        )
      )

      shiny::showNotification("Import erfolgreich", type = "message")
      shinydashboard::updateTabItems(session, inputId = "sidebar", selected = "overview")
    })

    output$summary_table <- DT::renderDT({
      summary_df <- if (is.null(state$dpcr_data) || nrow(state$dpcr_data) == 0) {
        tibble::tibble(Kennzahl = "Status", Wert = "Bitte laden Sie zunächst Daten")
      } else {
        build_analysis_summary(state$dpcr_data)
      }

      DT::datatable(
        summary_df,
        rownames = FALSE,
        options = list(dom = "t")
      )
    })
  })
}
