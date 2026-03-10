mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Willkommen",
        status = "primary",
        solidHeader = TRUE,
        shiny::h4(APP_NAME),
        shiny::p("Diese Web-App unterstützt die Analyse digitaler PCR-Daten mit Fokus auf Qiagen QIAcuity."),
        shiny::tags$ol(
          shiny::tags$li("Schritt 1: Daten auf der Seite 'Daten laden' als CSV auswählen."),
          shiny::tags$li("Schritt 2: 'Analyse starten' klicken und Validierungsreport prüfen."),
          shiny::tags$li("Schritt 3: Ergebnisse in 'Übersicht' und 'Qualität' interaktiv untersuchen."),
          shiny::tags$li("Schritt 4: Analyse als .RData exportieren und später wieder laden.")
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 6,
        title = "Unterstützte Geräte",
        status = "info",
        solidHeader = TRUE,
        shiny::tags$ul(
          shiny::tags$li("Qiagen QIAcuity: vollständig implementiert"),
          shiny::tags$li("Roche Digital LightCycler: Platzhalter (Importer vorbereitet)"),
          shiny::tags$li("Bio-Rad QX 100/200/400/600/700/800: Platzhalter (Importer vorbereitet)")
        )
      ),
      shinydashboard::box(
        width = 6,
        title = "Hinweis zu Beispieldaten",
        status = "success",
        solidHeader = TRUE,
        shiny::p("Beispieldateien liegen in 'Example_Data' (oder 'exampledata')."),
        shiny::p("Nutzen Sie diese Dateien direkt im Upload auf der Seite 'Daten laden'.")
      )
    )
  )
}

mod_home_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(state)
  })
}
