mod_sample_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Sample-Analyse",
        status = "info",
        solidHeader = TRUE,
        shiny::p("Platzhalter: Diese Seite wird in einer späteren Phase um sample-spezifische Analysen erweitert."),
        shiny::p("Geplant sind u. a. Konzentrationsabschätzung, Mutationslast und gruppierte Sample-Vergleiche.")
      )
    )
  )
}

mod_sample_analysis_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(state)
  })
}
