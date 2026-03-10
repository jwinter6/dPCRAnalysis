mod_report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Report",
        status = "info",
        solidHeader = TRUE,
        shiny::p("Platzhalter: Diese Seite wird um einen strukturierten Ergebnisreport ergänzt."),
        shiny::p("Geplant sind PDF/HTML-Ausgaben mit KPI-Tabellen und den wichtigsten Qualitätsplots.")
      )
    )
  )
}

mod_report_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(state)
  })
}
