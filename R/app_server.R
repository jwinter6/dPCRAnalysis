app_server <- function(input, output, session) {
  state <- shiny::reactiveValues(
    dpcr_data = new_empty_dpcr_data(),
    validation_report = empty_issues_table(),
    metadata = list(created_at = Sys.time(), app_version = APP_VERSION)
  )

  mod_home_server("home", state)
  mod_data_load_server("data_load", state)
  mod_overview_server("overview", state)
  mod_quality_server("quality", state)
  mod_sample_analysis_server("sample_analysis", state)
  mod_sample_detail_server("sample_detail", state)
  mod_report_server("report", state)
  mod_export_import_server("export_import", state)
  mod_help_server("help", state)
}
