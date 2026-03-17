mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 6,
        title = "Globale Farbpalette",
        status = "primary",
        solidHeader = TRUE,
        shiny::selectInput(
          ns("palette_id"),
          "Standardpalette",
          choices = palette_choice_labels(),
          selected = APP_DEFAULT_PALETTE_ID
        ),
        shiny::helpText("Die Auswahl gilt für alle ggplot2-Plots der App."),
        shiny::uiOutput(ns("active_palette_preview"))
      ),
      shinydashboard::box(
        width = 6,
        title = "Plot-Export",
        status = "warning",
        solidHeader = TRUE,
        shiny::numericInput(ns("export_width"), "Breite", value = PLOT_EXPORT_WIDTH_DEFAULT, min = 0.1, step = 0.1),
        shiny::numericInput(ns("export_height"), "Höhe", value = PLOT_EXPORT_HEIGHT_DEFAULT, min = 0.1, step = 0.1),
        shiny::numericInput(ns("export_dpi"), "Auflösung (dpi)", value = PLOT_EXPORT_DPI_DEFAULT, min = 1, step = 1),
        shiny::selectInput(
          ns("export_format"),
          "Dateiformat",
          choices = stats::setNames(PLOT_EXPORT_FORMATS, toupper(PLOT_EXPORT_FORMATS)),
          selected = PLOT_EXPORT_FORMAT_DEFAULT
        ),
        shiny::helpText("Standard: 5 × 5, 96 dpi, PNG."),
        shiny::uiOutput(ns("export_validation")),
        shiny::actionButton(ns("reset_export_defaults"), "Exportdefaults wiederherstellen")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Eigene Paletten",
        status = "info",
        solidHeader = TRUE,
        shiny::selectInput(
          ns("custom_palette_select"),
          "Eigene Palette bearbeiten",
          choices = c("Neue Palette" = "__new__"),
          selected = "__new__"
        ),
        shiny::textInput(ns("custom_palette_name"), "Palettenname", value = ""),
        shiny::textAreaInput(
          ns("custom_palette_colors"),
          "Farben",
          value = "",
          rows = 4,
          placeholder = "#1F77B4, #FF7F0E, #2CA02C"
        ),
        shiny::helpText("Farben als Hex-Werte (#RRGGBB) per Komma, Leerzeichen oder Zeilenumbruch trennen."),
        shiny::uiOutput(ns("custom_palette_validation")),
        shiny::uiOutput(ns("custom_palette_preview")),
        shiny::actionButton(ns("save_custom_palette"), "Palette speichern", class = "btn-primary"),
        shiny::actionButton(ns("delete_custom_palette"), "Palette löschen")
      )
    )
  )
}

mod_settings_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    build_palette_preview <- function(colors) {
      shiny::tags$div(
        class = "palette-preview-row",
        lapply(colors, function(color) {
          shiny::tags$span(
            class = "palette-swatch",
            style = sprintf("background:%s;", color),
            title = color
          )
        })
      )
    }

    current_settings <- shiny::reactive({
      sanitize_app_settings(state$app_settings, state$custom_palettes)
    })

    current_custom_palettes <- shiny::reactive({
      sanitize_custom_palettes(state$custom_palettes)
    })

    export_validation_messages <- shiny::reactive({
      messages <- character()

      width <- suppressWarnings(as.numeric(input$export_width))
      height <- suppressWarnings(as.numeric(input$export_height))
      dpi <- suppressWarnings(as.numeric(input$export_dpi))

      if (!is.null(input$export_width) && !is.na(width) && width <= 0) {
        messages <- c(messages, "Breite muss größer als 0 sein.")
      }

      if (!is.null(input$export_height) && !is.na(height) && height <= 0) {
        messages <- c(messages, "Höhe muss größer als 0 sein.")
      }

      if (!is.null(input$export_dpi) && !is.na(dpi) && dpi <= 0) {
        messages <- c(messages, "DPI muss größer als 0 sein.")
      }

      if (!is.null(input$export_dpi) && !is.na(dpi) && (dpi < 72 || dpi > 600)) {
        messages <- c(messages, "Empfohlener DPI-Bereich: 72 bis 600.")
      }

      unique(messages)
    })

    custom_palette_validation <- shiny::reactive({
      validate_hex_colors(parse_palette_color_input(input$custom_palette_colors))
    })

    shiny::observe({
      shiny::updateSelectInput(
        session,
        "palette_id",
        choices = palette_choice_labels(current_custom_palettes()),
        selected = current_settings()$palette_id
      )
    })

    shiny::observe({
      custom_choices <- c("Neue Palette" = "__new__", stats::setNames(names(current_custom_palettes()), names(current_custom_palettes())))

      selected <- input$custom_palette_select
      if (is.null(selected) || !selected %in% unname(custom_choices)) {
        selected <- "__new__"
      }

      shiny::updateSelectInput(
        session,
        "custom_palette_select",
        choices = custom_choices,
        selected = selected
      )
    })

    shiny::observe({
      export_settings <- current_settings()$export

      shiny::updateNumericInput(session, "export_width", value = export_settings$width)
      shiny::updateNumericInput(session, "export_height", value = export_settings$height)
      shiny::updateNumericInput(session, "export_dpi", value = export_settings$dpi)
      shiny::updateSelectInput(session, "export_format", selected = export_settings$format)
    })

    shiny::observeEvent(input$custom_palette_select, {
      selected <- input$custom_palette_select

      if (is.null(selected) || identical(selected, "__new__")) {
        shiny::updateTextInput(session, "custom_palette_name", value = "")
        shiny::updateTextAreaInput(session, "custom_palette_colors", value = "")
        return()
      }

      custom_palettes <- current_custom_palettes()
      colors <- custom_palettes[[selected]]

      shiny::updateTextInput(session, "custom_palette_name", value = selected)
      shiny::updateTextAreaInput(session, "custom_palette_colors", value = paste(colors, collapse = "\n"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$palette_id, {
      state$app_settings <- sanitize_app_settings(
        utils::modifyList(current_settings(), list(palette_id = input$palette_id)),
        current_custom_palettes()
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(
      list(input$export_width, input$export_height, input$export_dpi, input$export_format),
      {
        state$app_settings <- sanitize_app_settings(
          utils::modifyList(
            current_settings(),
            list(
              export = list(
                width = input$export_width,
                height = input$export_height,
                dpi = input$export_dpi,
                format = input$export_format
              )
            )
          ),
          current_custom_palettes()
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$reset_export_defaults, {
      state$app_settings <- sanitize_app_settings(
        utils::modifyList(current_settings(), list(export = get_default_export_settings())),
        current_custom_palettes()
      )
    })

    shiny::observeEvent(input$save_custom_palette, {
      palette_name <- sanitize_palette_name(input$custom_palette_name)
      selected_palette <- input$custom_palette_select
      colors <- parse_palette_color_input(input$custom_palette_colors)
      validation <- validate_hex_colors(colors)
      updated_settings <- isolate(current_settings())

      if (!nzchar(palette_name)) {
        shiny::showNotification("Bitte einen Namen für die Palette vergeben.", type = "error")
        return()
      }

      if (!isTRUE(validation$valid)) {
        shiny::showNotification(validation$message, type = "error")
        return()
      }

      custom_palettes <- current_custom_palettes()
      existing_names <- names(custom_palettes)
      renamed_palette <- if (!is.null(selected_palette) && !identical(selected_palette, "__new__")) selected_palette else NULL

      if (tolower(palette_name) %in% tolower(setdiff(existing_names, renamed_palette))) {
        shiny::showNotification("Der Palettenname ist bereits vergeben.", type = "error")
        return()
      }

      if (!is.null(renamed_palette) && renamed_palette %in% names(custom_palettes) && !identical(renamed_palette, palette_name)) {
        custom_palettes[[renamed_palette]] <- NULL
      }

      custom_palettes[[palette_name]] <- colors
      state$custom_palettes <- custom_palettes

      if (!is.null(renamed_palette) && identical(updated_settings$palette_id, custom_palette_id(renamed_palette))) {
        updated_settings$palette_id <- custom_palette_id(palette_name)
      }

      state$app_settings <- sanitize_app_settings(updated_settings, custom_palettes)

      shiny::updateSelectInput(session, "custom_palette_select", selected = palette_name)
      shiny::showNotification(sprintf("Palette '%s' gespeichert.", palette_name), type = "message")
    })

    shiny::observeEvent(input$delete_custom_palette, {
      selected_palette <- input$custom_palette_select
      updated_settings <- isolate(current_settings())

      if (is.null(selected_palette) || identical(selected_palette, "__new__")) {
        shiny::showNotification("Bitte zuerst eine eigene Palette auswählen.", type = "warning")
        return()
      }

      custom_palettes <- current_custom_palettes()
      custom_palettes[[selected_palette]] <- NULL
      state$custom_palettes <- custom_palettes

      if (identical(updated_settings$palette_id, custom_palette_id(selected_palette))) {
        updated_settings$palette_id <- APP_DEFAULT_PALETTE_ID
      }

      state$app_settings <- sanitize_app_settings(updated_settings, custom_palettes)
      shiny::updateSelectInput(session, "custom_palette_select", selected = "__new__")
      shiny::updateTextInput(session, "custom_palette_name", value = "")
      shiny::updateTextAreaInput(session, "custom_palette_colors", value = "")
      shiny::showNotification(sprintf("Palette '%s' gelöscht.", selected_palette), type = "message")
    })

    output$active_palette_preview <- shiny::renderUI({
      colors <- get_palette_preview_colors(current_settings(), current_custom_palettes(), n = 8)

      shiny::tagList(
        shiny::tags$p(class = "text-muted", sprintf("Aktiv: %s", palette_choice_labels(current_custom_palettes())[[current_settings()$palette_id]])),
        build_palette_preview(colors)
      )
    })

    output$custom_palette_validation <- shiny::renderUI({
      has_input <- nzchar(trimws(paste(input$custom_palette_name, collapse = ""))) ||
        nzchar(trimws(paste(input$custom_palette_colors, collapse = "")))

      if (!has_input) {
        return(
          shiny::tags$div(
            class = "alert alert-light",
            "Neue Paletten werden nach Eingabe eines Namens und gültiger Hex-Farben gespeichert."
          )
        )
      }

      validation <- custom_palette_validation()

      if (isTRUE(validation$valid)) {
        return(
          shiny::tags$div(
            class = "alert alert-success",
            "Alle Farbwerte sind gültig."
          )
        )
      }

      shiny::tags$div(
        class = "alert alert-warning",
        validation$message
      )
    })

    output$custom_palette_preview <- shiny::renderUI({
      validation <- custom_palette_validation()

      if (!isTRUE(validation$valid)) {
        return(NULL)
      }

      colors <- parse_palette_color_input(input$custom_palette_colors)
      build_palette_preview(colors)
    })

    output$export_validation <- shiny::renderUI({
      messages <- export_validation_messages()

      if (length(messages) == 0) {
        return(NULL)
      }

      shiny::tags$div(
        class = "alert alert-info",
        shiny::tags$ul(lapply(messages, shiny::tags$li))
      )
    })

    invisible(list(
      current_settings = current_settings,
      current_custom_palettes = current_custom_palettes
    ))
  })
}
