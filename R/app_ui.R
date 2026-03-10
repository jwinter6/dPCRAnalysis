app_ui <- function() {
  shinydashboard::dashboardPage(
    skin = "blue",
    header = shinydashboard::dashboardHeader(title = APP_NAME),
    sidebar = shinydashboard::dashboardSidebar(
      width = 280,
      shinydashboard::sidebarMenu(
        id = "sidebar",
        shinydashboard::menuItem("Start", tabName = "home", icon = shiny::icon("house")),
        shinydashboard::menuItem("Daten laden", tabName = "data_load", icon = shiny::icon("file-csv")),
        shinydashboard::menuItem("Übersicht", tabName = "overview", icon = shiny::icon("table")),
        shinydashboard::menuItem("Qualität", tabName = "quality", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Sample-Analyse", tabName = "sample_analysis", icon = shiny::icon("vial")),
        shinydashboard::menuItem("Detailanalyse Sample", tabName = "sample_detail", icon = shiny::icon("magnifying-glass-chart")),
        shinydashboard::menuItem("Report", tabName = "report", icon = shiny::icon("file-lines")),
        shinydashboard::menuItem("Export/Import", tabName = "export_import", icon = shiny::icon("right-left")),
        shinydashboard::menuItem("Hilfe", tabName = "help", icon = shiny::icon("circle-question"))
      )
    ),
    body = shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "home", mod_home_ui("home")),
        shinydashboard::tabItem(tabName = "data_load", mod_data_load_ui("data_load")),
        shinydashboard::tabItem(tabName = "overview", mod_overview_ui("overview")),
        shinydashboard::tabItem(tabName = "quality", mod_quality_ui("quality")),
        shinydashboard::tabItem(tabName = "sample_analysis", mod_sample_analysis_ui("sample_analysis")),
        shinydashboard::tabItem(tabName = "sample_detail", mod_sample_detail_ui("sample_detail")),
        shinydashboard::tabItem(tabName = "report", mod_report_ui("report")),
        shinydashboard::tabItem(tabName = "export_import", mod_export_import_ui("export_import")),
        shinydashboard::tabItem(tabName = "help", mod_help_ui("help"))
      )
    )
  )
}
