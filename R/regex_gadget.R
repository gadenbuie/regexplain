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
      ctx_text[1:min(length(ctx_text), 100)]
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
  error = function(e) {as.character(ctx_text[1:min(length(ctx_text), 100)])})

  regex_gadget(if (length(obj) && obj != "") obj)

}

#' regexplain gadget
#'
#' @import miniUI
#' @import shiny
#' @param text Text to explore in gadget (editable using interface)
#' @param start_page Open gadget to this tab, one of `"Text"`, `"Regex"`,
#'   `"Output"`, or `"Help"`
#' @export
regex_gadget <- function(text = NULL,
                         start_page = if (is.null(text)) "Text" else "Regex") {
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))

  ui <- miniPage(
    shiny::includeCSS(system.file("style.css", package = "regexplain")),
    shiny::includeCSS(system.file("gadget.css", package = "regexplain")),
    gadgetTitleBar(
      "regexplain",
      right = miniTitleBarButton("done", "Send Regex To Console", TRUE)
    ),
    miniTabstripPanel(
      selected = match.arg(start_page, c("Text", "Regex", "Output", "Help")),
      miniTabPanel(
        "Text", icon = icon('file-text-o'),
        miniContentPanel(
          fillCol(
            textAreaInputAlt('text',
                             label = "Text to search or parse",
                             value = paste(text, collapse = "\n"),
                             resize = "both",
                             width = "100%",
                             height="90%",
                             placeholder = "Paste, enter, or edit your sample text here.")
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
              textInputCode('pattern', 'Regex', width = "100%",
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
              style = "overflow-y: scroll; height: 100%;",
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
            # verbatimTextOutput('output_result', placeholder = TRUE)
            tags$pre(
              id = "output_result",
              class = "shiny-text-output",
              style = "overflow-y: scroll; height: 100%;"
            )
          )
        )
      ),
      miniTabPanel(
        "Help", icon = icon("support"),
        miniContentPanel(
          fillRow(
            flex = c(1, 4),
            tagList(
              # selectInput("help_category", "Category", c("", unique(cheatsheet$category))),
              # uiOutput("help_group"),
              tags$ul(
                id = "help-sidebar",
                tags$li("Character Classes", class = "header"),
                tags$ul(
                  class = "subgroup",
                  tags$li(actionLink("help_cat_character_classes_regular", "Regular")),
                  tags$li(actionLink("help_cat_character_classes_prebuilt", "Pre-Built"))
                ),
                tags$li(actionLink("help_cat_anchors", "Anchors")),
                tags$li("Escaped Characters", class = "header"),
                tags$ul(
                  class = "subgroup",
                  tags$li(actionLink("help_cat_escaped_general", "General")),
                  tags$li(actionLink("help_cat_escaped_hex", "Hex")),
                  tags$li(actionLink("help_cat_escaped_control", "Control Characters"))
                ),
                tags$li(actionLink("help_cat_groups", "Groups")),
                tags$li(actionLink("help_cat_quantifiers", "Quantifiers"))
              )
            ),
            tags$div(
              style = "width: 100%; padding-left: 10px;",
              uiOutput('help_text_selected')
            )
          )
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

    # output$help_group <- renderUI({
    #   req(input$help_category)
    #   groups <- unique(cheatsheet[cheatsheet$category == input$help_category, ]$group)
    #   if (is.na(groups[1])) {
    #     NULL
    #   } else {
    #     selectInput("help_group", "Group", groups)
    #   }
    # })

    # ---- Help Section ---- #
    help_text <- reactiveVal("<p>Select a category from the left sidebar.</p>")

    output$help_text_selected <- renderUI({
      HTML(help_text())
    })

    make_html_table <- function(x) {
      select(x, .data$regexp, .data$text) %>%
        knitr::kable(
          col.names = c("Regexp", "Text"),
          escape = FALSE,
          format = "html")
    }

    observeEvent(input$help_cat_character_classes_regular, {
      cheatsheet %>%
        filter(category == "character classes", group == "regular") %>%
        make_html_table %>%
        help_text
    })

    observeEvent(input$help_cat_character_classes_prebuilt, {
      cheatsheet %>%
        filter(category == "character classes", group == "pre-built") %>%
        make_html_table %>%
        help_text
    })

    observeEvent(input$help_cat_anchors, {
      cheatsheet %>%
        filter(category == "anchors") %>%
        make_html_table %>%
        help_text
    })

    observeEvent(input$help_cat_escaped_general, {
      cheatsheet %>%
        filter(category == "escaped characters", group == "general") %>%
        make_html_table %>%
        help_text
    })

    observeEvent(input$help_cat_escaped_hex, {
      cheatsheet %>%
        filter(category == "escaped characters", group == "hex") %>%
        make_html_table %>%
        help_text
    })

    observeEvent(input$help_cat_escaped_control, {
      cheatsheet %>%
        filter(category == "escaped characters", group == "control characters") %>%
        make_html_table %>%
        help_text
    })

    observeEvent(input$help_cat_groups, {
      cheatsheet %>%
        filter(category == "groups") %>%
        make_html_table %>%
        help_text
    })

    observeEvent(input$help_cat_quantifiers, {
      cheatsheet %>%
        filter(category == "quantifiers") %>%
        make_html_table %>%
        help_text
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
