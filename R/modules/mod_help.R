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
          shiny::tags$li("Export/Import: .RData mit Objekten dpcr_data, validation_report, metadata, app_settings und custom_palettes.")
        ),
        shiny::h4("Einstellungen"),
        shiny::tags$ul(
          shiny::tags$li("Die Seite 'Einstellungen' steuert die globale Standardpalette für ggplot2-Plots."),
          shiny::tags$li("Verfügbar sind die originale ggplot2-Standardpalette, weitere vordefinierte Paletten und eigene Hex-Paletten."),
          shiny::tags$li("Plots lassen sich über 'Download Plot' mit den Exportdefaults 5 × 5, 96 dpi und PNG herunterladen."),
          shiny::tags$li("In der Sample-Analyse erscheint 'Dateiname' unter 'Farbe nach', sobald Daten aus mehr als einer Datei vorliegen.")
        ),
        shiny::h4("FAQ"),
        shiny::tags$dl(
          shiny::tags$dt("Warum werden Validierungswarnungen angezeigt?"),
          shiny::tags$dd("Warnungen markieren auffällige, aber nicht zwingend ungültige Werte (z. B. negative RFU)."),
          shiny::tags$dt("Welche Geräte sind derzeit vollständig unterstützt?"),
          shiny::tags$dd("Aktuell Qiagen QIAcuity. Roche und Bio-Rad sind als Erweiterung vorbereitet."),
          shiny::tags$dt("Wie setze ich die Analyse fort?"),
          shiny::tags$dd("Unter 'Export/Import' die Analyse als .RData speichern und später wieder laden."),
          shiny::tags$dt("Wo ändere ich Plotfarben und Plot-Export?"),
          shiny::tags$dd("Auf der Seite 'Einstellungen'. Dort werden Palette, Exportgröße, DPI und Dateiformat global verwaltet.")
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
