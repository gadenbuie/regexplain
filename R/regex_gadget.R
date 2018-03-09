#' @import miniUI
#' @import shiny
regex_gadget <- function(text = NULL) {
  library("shiny")
  library("miniUI")

  ui <- miniPage(
    shiny::includeCSS(system.file("style.css", package = "regexplain")),
    gadgetTitleBar(
      "regexplain",
      right = miniTitleBarButton("done", "To Console", TRUE)
    ),
    miniTabstripPanel(
      selected = if (is.null(text)) "Text" else "Regex",
      miniTabPanel(
        "Text", icon = icon('file-text-o'),
        miniContentPanel(
          fillCol(
            flex = c(3, 1),
            textAreaInputAlt('text', label = "Text", value = paste(text, collapse = "\n"), resize = "both", width = "100%", height="90%"),
            inputPanel(
              checkboxInput('text_break_newline', "Break Lines", TRUE)
            )
          )
        )
      ),
      miniTabPanel(
        "Regex", icon = icon('terminal'),
        miniContentPanel(
          textInputCode('pattern', 'Regex', width = "100%",
                    placeholder = "Enter regex, double backslash not required"),
          htmlOutput('result')
        )
      ),
      miniTabPanel(
        "Help", icon = icon("support"),
        miniContentPanel(
          tags$p("Help will go here.")
        )
      )
    )
  )

  server <- function(input, output, session) {
    rtext <- reactive({
      x <- if (input$text_break_newline) {
        strsplit(input$text, "\n")[[1]]
      } else input$text
      x
    })

    output$result <- renderUI({
      if (is.null(rtext())) return(NULL)
      if (input$pattern == "") {
        return(toHTML(paste('<p class="results">', rtext(), "</p>", collapse = "")))
      }
      tryCatch({
        toHTML(
          paste(
            view_regex(rtext(), sanitize_text_input(input$pattern), render = FALSE),
            collapse = ""
          )
        )
      },
      error = function(e) {
        toHTML(paste0("<pre class='alert alert-danger' style='padding: 4px; margin-top: 1px; margin-bottom: 4px;'>", paste(e$message, collapse = "<br>"), "</pre>"),
                paste('<p class="results">', rtext(), "</p>", collapse = ""))
      })
    })

    observeEvent(input$done, {
      # browser()
      if (input$pattern != "") {
        pattern <- paste0('regex <- "', escape_backslash(sanitize_text_input(input$pattern)), '"')
        rstudioapi::sendToConsole(pattern, FALSE)
      }
      stopApp()
    })

    observeEvent(input$cancel, {
      stopApp()
    })
  }

  viewer <- shiny::paneViewer(700)
  runGadget(ui, server, viewer = viewer)
}

sanitize_text_input <- function(x) {
  x <- gsub("(“|”)", '"', x)
  x <- gsub("‘|’", "'", x)
  x
}

toHTML <- function(...) {
  x <- paste(..., collapse = "")
  x <- gsub("\n", "\\\\n", x)
  HTML(x)
}


# ---- Modified Shiny Inputs ----

#' Modified Text Area Input
#'
#' @inheritParams shiny textAreaInput
textAreaInputAlt <- function(inputId, label, value = "", width = NULL, height = NULL,
                          cols = NULL, rows = NULL, placeholder = NULL, resize = NULL,
                          is_code = TRUE) {
  `%AND%` <- shiny:::`%AND%`

  value <- shiny::restoreInput(id = inputId, default = value)

  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
  }

  style <- paste(
    if (!is.null(width))  paste0("width: ",  shiny::validateCssUnit(width),  ";"),
    if (!is.null(height)) paste0("height: ", shiny::validateCssUnit(height), ";"),
    if (!is.null(resize)) paste0("resize: ", resize, ";"),
    if (is_code) 'font-family: "Monaco", "Inconsolata", monospace;'
  )

  parent_style <- paste(
    if (!is.null(width) && grepl("%", width)) paste0("width: ", width, ";"),
    if (!is.null(height) && grepl("%", height)) paste0("height: ", height, ";")
  )

  # Workaround for tag attribute=character(0) bug:
  #   https://github.com/rstudio/htmltools/issues/65
  if (length(style) == 0) style <- NULL

  shiny::div(class = "form-group shiny-input-container",
      label %AND% shiny::tags$label(label, `for` = inputId),
      style = if (!parent_style %in% c(" ", "", "  ")) parent_style,
      shiny::tags$textarea(
        id = inputId,
        class = "form-control",
        placeholder = placeholder,
        style = style,
        rows = rows,
        cols = cols,
        autocomplete = "off",
        autocorrect = "off",
        autocapitalize = "off",
        spellcheck = "false",
        value
      )
  )
}

#' Modified Text Input
#'
#' @inheritParams shiny textInput
textInputCode <- function(inputId, label, value = "", width = NULL,
                      placeholder = NULL) {
  `%AND%` <- shiny:::`%AND%`
  value <- shiny::restoreInput(id = inputId, default = value)

  shiny::div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
      label %AND% shiny::tags$label(label, `for` = inputId),
      shiny::tags$input(id = inputId, type="text", class="form-control", value=value,
                        style = 'font-family: "Monaco", "Inconsolata", monospace;',
                        autocomplete = "off", autocorrect = "off",
                        autocapitalize = "off", spellcheck = "false",
                        placeholder = placeholder)
  )
}
