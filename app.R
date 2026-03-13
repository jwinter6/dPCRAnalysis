source_files <- sort(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
invisible(lapply(source_files, source))

# Erhöht die maximal erlaubte Upload-Größe auf 500 MB.
options(shiny.maxRequestSize = 500 * 1024^2)

shiny::shinyApp(ui = app_ui(), server = app_server)
