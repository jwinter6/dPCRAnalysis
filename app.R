source_files <- sort(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
invisible(lapply(source_files, source))

shiny::shinyApp(ui = app_ui(), server = app_server)
