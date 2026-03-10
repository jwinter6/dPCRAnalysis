mod_help_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Hilfe",
        status = "primary",
        solidHeader = TRUE,
        shiny::h4("Dateiformate"),
        shiny::tags$ul(
          shiny::tags$li("QIAcuity CSV: Komma-separiert, erste Zeile wird übersprungen (z. B. 'sep=,')."),
          shiny::tags$li("Export/Import: .RData mit Objekten dpcr_data, validation_report, metadata.")
        ),
        shiny::h4("FAQ"),
        shiny::tags$dl(
          shiny::tags$dt("Warum werden Validierungswarnungen angezeigt?"),
          shiny::tags$dd("Warnungen markieren auffällige, aber nicht zwingend ungültige Werte (z. B. negative RFU)."),
          shiny::tags$dt("Welche Geräte sind derzeit vollständig unterstützt?"),
          shiny::tags$dd("Aktuell Qiagen QIAcuity. Roche und Bio-Rad sind als Erweiterung vorbereitet."),
          shiny::tags$dt("Wie setze ich die Analyse fort?"),
          shiny::tags$dd("Unter 'Export/Import' die Analyse als .RData speichern und später wieder laden.")
        )
      )
    )
  )
}

mod_help_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(state)
  })
}
