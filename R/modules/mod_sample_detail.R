mod_sample_detail_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Detailanalyse Sample",
        status = "info",
        solidHeader = TRUE,
        shiny::p("Platzhalter: Hier folgen detaillierte Drilldown-Ansichten für einzelne Samples."),
        shiny::p("Mögliche Inhalte: Well-Heatmaps, Kanalvergleich und zeitliche Vergleichsansichten.")
      )
    )
  )
}

mod_sample_detail_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(state)
  })
}
