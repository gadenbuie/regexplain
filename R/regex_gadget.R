#' regexplain_addin
#'
#' @keywords internal
regexplain_addin <- function() {
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Get context text
  ctx_text <- context$selection[[1]]$text

  # If it is one line and evaluates to something, use that
  # Otherwise treat as text
  obj <- tryCatch({
    if (grepl("\n", ctx_text)) {
      ctx_text
    } else {
      x <- eval(parse(text = ctx_text))
      x <- as.character(x)
      if (length(x) == 1 && grepl("\n", x))
        x <- strsplit(x, "\n")[[1]]
      if (length(x) > 10) {
        message(ctx_text, " gave ", length(x), " lines, limiting to first 10 unique lines.")
        x <- unique(x)
        x[1:min(length(x), 10)]
      } else x
    }
  },
  error = function(e) {as.character(ctx_text)})

  regex_gadget(if (length(obj) && obj != "") obj)

}

#' regexplain gadget
#'
#' @import miniUI
#' @import shiny
#' @export
regex_gadget <- function(text = NULL) {
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))

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
            textAreaInputAlt('text', label = "Text", value = paste(text, collapse = "\n"), resize = "both", width = "100%", height="90%")
          )
        )
      ),
      miniTabPanel(
        "Regex", icon = icon('terminal'),
        miniContentPanel(
          fillCol(
            flex = c(1, 3),
            fillCol(
              flex = c(1, 1),
              textInputCode('pattern', 'Regex', width = "90%",
                            placeholder = "Enter regex, single \\ okay"),
              checkboxGroupInput(
                'regex_options',
                label = "",
                inline = TRUE,
                width = "90%",
                choices = c("Break Lines" = "text_break_lines",
                            "Ignore Case" = "ignore.case",
                            "Perl Style" = "perl",
                            "Fixed" = "fixed",
                            "Use Bytes" = "useBytes"
                            # , "Invert" = "invert"
                ),
                selected = c('text_break_lines')
              )
            ),
            tags$div(
              class = "gadget-result",
              htmlOutput('result')
            )
          )
        )
      ),
      miniTabPanel(
        "Output", icon = icon("table"),
        miniContentPanel(
          fillCol(
            flex = c(1, 3),
            inputPanel(
              width = "100%;",
              selectInput('regexFn', label = 'Apply Function',
                          choices = regexFn_choices)
            ),
            verbatimTextOutput('output_result',
                               placeholder = TRUE)
          )
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
      x <- if ('text_break_lines' %in% input$regex_options) {
        strsplit(input$text, "\n")[[1]]
      } else input$text
      x
    })

    output$result <- renderUI({
      if (is.null(rtext())) return(NULL)
      if (input$pattern == "") {
        return(toHTML(paste('<p class="results">', escape_html(rtext()), "</p>", collapse = "")))
      }
      tryCatch({
        toHTML(
          paste(
            view_regex(
              rtext(),
              sanitize_text_input(input$pattern),
              ignore.case = 'ignore.case' %in% input$regex_options,
              perl = 'perl' %in% input$regex_options,
              fixed = 'fixed' %in% input$regex_options,
              useBytes = 'useBytes' %in% input$regex_options,
              # invert = 'invert' %in% input$regex_options,
              render = FALSE,
              escape = TRUE),
            collapse = ""
          )
        )
      },
      error = function(e) {
        toHTML(paste0("<pre class='alert alert-danger' style='padding: 4px; margin-top: 1px; margin-bottom: 4px;'>", paste(e$message, collapse = "<br>"), "</pre>"),
                paste('<p class="results">', escape_html(rtext()), "</p>", collapse = ""))
      })
    })

    output$output_result <- renderPrint({
      req(input$regexFn)
      regexPkg <- get_pkg_namespace(input$regexFn)
      regexFn <- getFromNamespace(input$regexFn, regexPkg)
      x <- if (regexPkg == "base") {
        regexFn(input$pattern, rtext())
      } else if (regexPkg == "stringr") {
        regexFn(rtext(), input$pattern)
      } else {
        "Um. Not sure how I got here."
      }

      # if (inherits(x, 'logical') || inherits(x, 'character')) {
      #   if (length(x) < 25) print(x) else print(head(x, 25))
      # } else if (inherits(x, 'matrix') | inherits(x, "data.frame")) {
      #   if (nrow(x) < 15) { print(x)
      #   } else glimpse(x)
      # } else {
      #   str(x, max.level = 3)
      # }
      print(x)
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
  x <- gsub("\t", "\\\\t", x)
  x <- gsub("\r", "\\\\r", x)
  HTML(x)
}

regexFn_choices <- list(
  "Choose a function" = "",
  base = c(
    "grep",
    "grepl",
    "regexpr",
    "gregexpr",
    "regexec"
  ),
  stringr = c(
    "str_detect",
    "str_locate",
    "str_locate_all",
    "str_extract",
    "str_extract_all",
    "str_match",
    "str_match_all",
    "str_split"
  )
)

get_pkg_namespace <- function(fn) {
  x <- names(purrr::keep(regexFn_choices, ~ (fn %in% .)))
  if (length(x) > 1) warning(fn, " matches multiple functions in regexFn_choices, please review.")
  x
}
