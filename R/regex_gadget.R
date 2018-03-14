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

  update_available <- check_version()

  # ---- UI ----
  ui <- miniPage(
    shiny::includeCSS(system.file("styles", "style.css", package = "regexplain")),
    shiny::includeCSS(system.file("styles", "gadget.css", package = "regexplain")),
    gadgetTitleBar(
      "regexplain",
      right = miniTitleBarButton("done", "Send Regex To Console", TRUE)
    ),
    miniTabstripPanel(
      selected = match.arg(start_page, c("Text", "Regex", "Output", "Help")),
      # --- UI - Tab - Text ----
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
      # ---- UI - Tab - Regex ----
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
                label = HTML(
                  '<div style="font-size: 1.25rem;">',
                  'Option Groups: ',
                  '<span style="color: #337ab7;">regexplain</span>,',
                  '<span style="color: #5cb85c;">all</span>, ',
                  '<span style="color: #f0ad4e;">base only</span>',
                  '</div>'
                ),
                inline = TRUE,
                width = "90%",
                choiceValues = list(
                  "text_break_lines",
                  "ignore.case",
                  "fixed",
                  "perl",
                  "useBytes"),
                choiceNames = list(
                  HTML('<span style="color: #337ab7;">Break Lines</span>'),
                  HTML('<span style="color: #5cb85c;">Ignore Case</span>'),
                  HTML('<span style="color: #5cb85c;">Fixed/Literal</span>'),
                  HTML('<span style="color: #f0ad4e;">Perl Style</span>'),
                  HTML('<span style="color: #f0ad4e;">Use Bytes</span>')),
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
      # ---- UI - Tab - Output ----
      miniTabPanel(
        "Output", icon = icon("table"),
        miniContentPanel(
          fillCol(
            flex = c(1, 3),
            inputPanel(
              tags$div(
                width = "100%;",
                selectInput('regexFn', label = 'Apply Function',
                            choices = regexFn_choices),
                tags$span(class = "help-block",
                          style = "font-size:1.25rem; margin-top:-10px; margin-bottom:0px; margin-left:4px;",
                          "Adjust options in Regex tab")
              ),
              uiOutput("output_sub")
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
      # ---- UI - Tab - Help ----
      miniTabPanel(
        "Help", icon = icon("support"),
        help_ui("help")
      )
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {
    if (!is.null(update_available)) {
      showModal(
        modalDialog(
          title = "Update Available \U1F389",
          easyClose = TRUE,
          footer = modalButton("OK"),
          tagList(
            tags$p(
              "Version", update_available$version, "is",
              tags$a(href = update_available$link,
                     "available on GitHub.")
            ),
            if ("devtools" %in% installed.packages()) tags$p(
              "The fastest way to update is with devtools:",
              tags$pre(
                "devtools::update_packages(\"gadenbuie/regexplain\")"
              )
            ),
            tags$p(
              class = 'help-block',
              "This message won't be shown again during this R session."
            )
          )
        )
      )
    }

    # ---- Server - Global ----
    rtext <- reactive({
      x <- if ('text_break_lines' %in% input$regex_options) {
        strsplit(input$text, "\n")[[1]]
      } else input$text
      x
    })

    pattern <- reactive({
      sanitize_text_input(input$pattern)
    })

    alert_result <- function(msg, type = "danger") {
      msg <- gsub("\n", "<br>", msg)
      msg <- gsub("\t", "&nbsp;&nbsp;", msg)
      paste0("<pre class='alert alert-", type, "' ",
             "style='padding: 4px; margin-top: 1px; margin-bottom: 4px;'>",
             paste(msg, collapse = "<br>"),
             "</pre>")
    }

    # ---- Server - Tab - Regex ----
    output$result <- renderUI({
      if (is.null(rtext())) return(NULL)
      if (pattern() == "") {
        return(toHTML(paste('<p class="results">', escape_html(rtext()), "</p>", collapse = "")))
      }
      res <- NULL
      error_message <- NULL
      warning_message <- NULL
      tryCatch({
        res <- paste(
          view_regex(
            rtext(),
            pattern(),
            ignore.case = 'ignore.case' %in% input$regex_options,
            perl = 'perl' %in% input$regex_options,
            fixed = 'fixed' %in% input$regex_options,
            useBytes = 'useBytes' %in% input$regex_options,
            # invert = 'invert' %in% input$regex_options,
            render = FALSE,
            escape = TRUE,
            exact  = FALSE),
          collapse = ""
        )
      },
      error = function(e) {
        error_message <<- alert_result(e$message, "danger")
      },
      warning = function(w) {
        warning_message <<- alert_result(w$message, "warning")
      })

      if (is.null(res)) res <- toHTML(
        paste('<p class="results">', escape_html(rtext()), "</p>", collapse = "")
      )

      toHTML(paste(error_message, warning_message, res))
    })

    # ---- Server - Tab - Output ----
    regexFn_replacement_val <- NULL

    output$output_sub <- renderUI({
      req(input$regexFn)
      if (!input$regexFn %in% regexFn_substitute) return(NULL)
      textInputCode('regexFn_replacement', 'Subsitution',
                    value = regexFn_replacement_val,
                    placeholder = "Replacement Text")
    })

    replacement <- reactive({
      req(input$regexFn)
      if (!input$regexFn %in% regexFn_substitute) {
        NULL
      } else {
        regexFn_replacement_val <<- input$regexFn_replacement
        sanitize_text_input(input$regexFn_replacement)
      }
    })

    output$output_result <- renderPrint({
      req(input$regexFn)
      regexPkg <- get_pkg_namespace(input$regexFn)
      regexFn <- getFromNamespace(input$regexFn, regexPkg)
      req_sub_arg <- input$regexFn %in% regexFn_substitute
      x <- if (regexPkg == "base") {
        if (req_sub_arg) {
          req(replacement())
          regexFn(pattern(), replacement(), rtext(),
                  ignore.case = 'ignore.case' %in% input$regex_options,
                  perl = 'perl' %in% input$regex_options,
                  fixed = 'fixed' %in% input$regex_options,
                  useBytes = 'useBytes' %in% input$regex_options)
        } else {
          regexFn(pattern(), rtext(),
                  ignore.case = 'ignore.case' %in% input$regex_options,
                  perl = 'perl' %in% input$regex_options,
                  fixed = 'fixed' %in% input$regex_options,
                  useBytes = 'useBytes' %in% input$regex_options)
        }
      } else if (regexPkg == "stringr") {
        if (req_sub_arg) {
          req(replacement())
          regexFn(
            rtext(),
            stringr::regex(
              pattern(),
              ignore_case = 'ignore.case' %in% input$regex_options,
              literal = 'fixed' %in% input$regex_options
            ),
            replacement()
          )
        } else {
          regexFn(
            rtext(),
            stringr::regex(
              pattern(),
              ignore_case = 'ignore.case' %in% input$regex_options,
              literal = 'fixed' %in% input$regex_options
            )
          )
        }
      } else {
        "Um. Not sure how I got here."
      }
      print(x)
    })

    # ---- Server - Tab - Help ----
    help_text <- callModule(help_server, "help")

    # ---- Server - Tab - Exit ----
    observeEvent(input$done, {
      if (pattern() != "") {
        pattern <- paste0('pattern <- "', escape_backslash(pattern()), '"')
        if ("regexFn_replacement" %in% names(input) && replacement() != "") {
          pattern <- paste0(
            pattern, "\n",
            'replacement <- "', escape_backslash(replacement()), '"'
          )
        }
        rstudioapi::sendToConsole(pattern, FALSE)
      }
      stopApp()
    })

    observeEvent(input$cancel, {
      stopApp()
    })
  }

  viewer <- shiny::paneViewer(minHeight = 1000)
  runGadget(ui, server, viewer = viewer)
}

# ---- Gadget Helper Functions and Variables ----

sanitize_text_input <- function(x) {
  if (is.null(x) || !nchar(x)) return(x)
  if (grepl("\\u|\\x|\\N|\\a|\\o", x)) {
    try({
      y <- stringi::stri_unescape_unicode(x)
    }, silent = TRUE)
    if (!is.na(y)) x <- y
  }
  # x <- gsub("\u201C|\u201D", '"', x)
  # x <- gsub("\u2018|\u2019", "'", x)
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
    "sub",  #<<
    "gsub", #<<
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
    "str_replace",     #<<
    "str_replace_all", #<<
    "str_split"
  )
)

regexFn_substitute <- c(
  paste0(c("", "g"), "sub"),
  paste0("str_replace", c("", "_all"))
)

get_pkg_namespace <- function(fn) {
  x <- names(purrr::keep(regexFn_choices, ~ (fn %in% .)))
  if (length(x) > 1) warning(fn, " matches multiple functions in regexFn_choices, please review.")
  x
}

#' Check if an updated version is available
#'
#' I included this because it can be difficult to tell if your RStudio Addins
#' are up to date. I may add new features that you want but you won't hear about
#' the updates. This function checks if an update is available, using GitHub
#' tags. If an update is available, a modal dialog is shown when you start
#' the regexplain gadget. This only happens once per R session, though, so feel
#' free to ignore the message.
#'
#' @param gh_user GitHub user account
#' @param gh_repo GitHub repo name
#' @param this_version The currently installed version of the package
#' @keywords internal
check_version <- function(
  gh_user = "gadenbuie",
  gh_repo = "regexplain",
  this_version = packageVersion('regexplain')
) {
  ok_to_check <- getOption("regexplain.no.check.version", TRUE)
  if (!ok_to_check) return(NULL)
  if (!requireNamespace('jsonlite', quietly = TRUE)) return(NULL)
  gh_tags <- jsonlite::fromJSON(
    paste0("https://api.github.com/repos/", gh_user, "/", gh_repo, "/git/refs/tags"),
    simplifyDataFrame = TRUE
  )
  gh_tags$tag <- sub("refs/tags/", "", gh_tags$ref, fixed = TRUE)
  gh_tags$version <- sub("^v\\.?", "", gh_tags$tag)
  if (any(gh_tags$version > this_version)) {
    max_version <- max(gh_tags$version)
    max_tag <- gh_tags$tag[gh_tags$version == max_version]
    options(regexplain.no.check.version = FALSE)
    return(
      list(
        version = max_version,
        link = paste("https://github.com", gh_user, gh_repo, "releases/tag", max_tag, sep = "/")
      )
    )
  } else return(NULL)
}
