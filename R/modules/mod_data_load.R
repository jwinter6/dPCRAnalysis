mod_data_load_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 8,
        title = "CSV-Dateien laden",
        status = "primary",
        solidHeader = TRUE,
        shiny::fileInput(
          ns("csv_files"),
          label = "Dateien auswählen",
          multiple = TRUE,
          accept = c(".csv", "text/csv")
        ),
        shiny::actionButton(
          ns("analyze_btn"),
          label = "Analyse starten",
          class = "btn-success"
        ),
        shiny::br(),
        shiny::br(),
        shiny::uiOutput(ns("example_data_hint"))
      ),
      shinydashboard::box(
        width = 4,
        title = "Importstatus",
        status = "info",
        solidHeader = TRUE,
        shiny::uiOutput(ns("status_ui"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Dateiliste und Geräteerkennung",
        status = "primary",
        solidHeader = TRUE,
        DT::DTOutput(ns("file_table"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Validierungsreport",
        status = "warning",
        solidHeader = TRUE,
        DT::DTOutput(ns("validation_table"))
      )
    )
  )
}

mod_data_load_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    status_message <- shiny::reactiveVal(list(type = "muted", text = "Bitte laden Sie zunächst Daten."))

    output$example_data_hint <- shiny::renderUI({
      files <- list_example_data_files()

      if (length(files) == 0) {
        shiny::tags$p(class = "text-muted", "Keine Beispieldaten im Projektordner gefunden.")
      } else {
        shiny::tags$div(
          class = "alert alert-secondary",
          shiny::tags$b("Hinweis:"),
          sprintf(" %s Beispieldatei(en) gefunden in '%s'.", length(files), dirname(files[[1]]))
        )
      }
    })

    file_info <- shiny::reactive({
      req <- shiny::req(input$csv_files)
      data.frame(
        file_name = req$name,
        file_path = req$datapath,
        size_mb = round(req$size / (1024 * 1024), 2),
        device_type = vapply(req$datapath, detect_device_type, FUN.VALUE = character(1)),
        stringsAsFactors = FALSE
      )
    })

    output$file_table <- DT::renderDT({
      info <- file_info()
      DT::datatable(
        info[, c("file_name", "size_mb", "device_type")],
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    output$status_ui <- shiny::renderUI({
      msg <- status_message()
      class_name <- switch(
        msg$type,
        success = "alert alert-success",
        warning = "alert alert-warning",
        danger = "alert alert-danger",
        "alert alert-light"
      )

      shiny::tags$div(class = class_name, msg$text)
    })

    output$validation_table <- DT::renderDT({
      issues <- state$validation_report

      if (is.null(issues) || nrow(issues) == 0) {
        issues <- empty_issues_table()
      }

      DT::datatable(
        issues,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$analyze_btn, {
      shiny::req(input$csv_files)
      info <- file_info()

      imported_data <- list()
      import_issues <- empty_issues_table()

      for (i in seq_len(nrow(info))) {
        file_path <- info$file_path[[i]]
        file_name <- info$file_name[[i]]
        device <- info$device_type[[i]]

        imported_df <- tryCatch(
          {
            if (device == "qiaquity") {
              import_qiaquity_csv(file_path)
            } else if (device == "roche") {
              import_roche_csv(file_path)
            } else if (device == "biorad") {
              import_biorad_csv(file_path)
            } else {
              new_empty_dpcr_data()
            }
          },
          error = function(e) {
            import_issues <<- dplyr::bind_rows(
              import_issues,
              tibble::tibble(
                severity = "error",
                field = "import",
                message = sprintf("Fehler beim Import von '%s': %s", file_name, e$message),
                n_rows = NA_integer_
              )
            )
            new_empty_dpcr_data()
          }
        )

        if (device %in% c("roche", "biorad")) {
          import_issues <- dplyr::bind_rows(
            import_issues,
            tibble::tibble(
              severity = "warning",
              field = "import",
              message = sprintf("%s erkannt, aber Importer ist noch Platzhalter ('%s').", device, file_name),
              n_rows = NA_integer_
            )
          )
        }

        if (device == "unknown") {
          import_issues <- dplyr::bind_rows(
            import_issues,
            tibble::tibble(
              severity = "error",
              field = "device_type",
              message = sprintf("Gerätetyp für Datei '%s' konnte nicht erkannt werden.", file_name),
              n_rows = NA_integer_
            )
          )
        }

        if (nrow(imported_df) > 0) {
          imported_data[[length(imported_data) + 1]] <- imported_df
        }
      }

      combined <- if (length(imported_data) > 0) {
        dplyr::bind_rows(imported_data)
      } else {
        new_empty_dpcr_data()
      }

      validation <- validate_dpcr_data(combined)
      full_issues <- dplyr::bind_rows(import_issues, validation$issues)

      if (nrow(combined) == 0) {
        full_issues <- dplyr::bind_rows(
          full_issues,
          tibble::tibble(
            severity = "error",
            field = "import",
            message = "Es konnten keine importierbaren Datenzeilen erzeugt werden.",
            n_rows = 0L
          )
        )
      }

      has_errors <- any(full_issues$severity == "error")

      state$dpcr_data <- validation$clean_data
      state$validation_report <- full_issues
      state$metadata <- list(
        imported_at = Sys.time(),
        source_files = info$file_name,
        detected_devices = stats::setNames(info$device_type, info$file_name),
        n_rows = nrow(validation$clean_data)
      )

      if (!has_errors) {
        status_message(list(type = "success", text = "Import erfolgreich"))
        shiny::showNotification("Import erfolgreich", type = "message")
        shinydashboard::updateTabItems(session, inputId = "sidebar", selected = "overview")
      } else {
        status_message(list(type = "warning", text = "Validierungsfehler"))
        shiny::showNotification("Validierungsfehler", type = "error")
      }
    })
  })
}
