#' regexplain gadget
#'
#' @import miniUI
#' @import shiny
#' @param text Text to explore in gadget (editable using interface)
#' @param start_page Open gadget to this tab, one of `"Text"`, `"RegEx"`,
#'   `"Output"`, or `"Help"`
#' @export
regex_gadget <- function(text = NULL,
                         start_page = if (is.null(text)) "Text" else "RegEx") {
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))

  update_available <- check_version()

  # ---- UI ----
  ui <- miniPage(
    shiny::includeCSS(system.file("styles", "style.css", package = "regexplain")),
    shiny::includeCSS(system.file("styles", "gadget.css", package = "regexplain")),
    gadgetTitleBar(
      "regexplain",
      right = miniTitleBarButton("done", "Send RegEx To Console", TRUE)
    ),
    miniTabstripPanel(
      selected = match.arg(start_page, c("Text", "RegEx", "Output", "Help")),
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
        "RegEx", icon = icon('terminal'),
        miniContentPanel(
          fillCol(
            flex = c(1, 3),
            fillCol(
              flex = c(1, 1),
              textInputCode('pattern', 'RegEx', width = "100%",
                            placeholder = "Standard RegEx, e.g. \\w+_\\d{2,4}\\s+"),
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
                          "Adjust options in RegEx tab")
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
        generate_help_ui(cheatsheet_only = FALSE)
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
      if (!requireNamespace(regexPkg, quietly = TRUE)) {
        return(cat(
          paste0(
            "The package `", regexPkg, "` is not installed.\n",
            "To preview results from this package, please run\n\n",
            "    install.packages(\"", regexPkg, "\")"
          )
        ))
      }
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
      } else if (regexPkg == "rematch2") {
        regexFn(rtext(), pattern(),
                ignore.case = 'ignore.case' %in% input$regex_options,
                perl = 'perl' %in% input$regex_options,
                fixed = 'fixed' %in% input$regex_options,
                useBytes = 'useBytes' %in% input$regex_options)
      } else {
        "Um. Not sure how I got here."
      }
      print(x)
    })

    # ---- Server - Tab - Help ----
    source(system.file("shiny", "help_server.R", package = "regexplain"), local = TRUE)

    load_buttons <- function(..., extra_btns = NULL) {
      prefix <- paste(..., sep = "_")
      btns <- c(
        list(c("text", "Load Text", "btn-success"),
             c("pattern", "Load Pattern", "btn-primary")),
        extra_btns
      )
      tags$span(
        style = "display: inline-block;",
        purrr::map(
          btns,
          ~ actionButton(paste0(prefix, "_", .[1]), .[2], class = paste("btn-xs", if (!is.na(.[3])) .[3]))
        )
      )
    }

    observeEvent(input$help_try_this, {
      tagList(
        tags$h3("Try These Examples"),
        tags$p("Here are a couple interesting text extraction challenges you can try",
               "with this gadget."),
        tags$h4("Harvard Sentences"),
        tags$p("These examples come from the",
               tags$a(href = "http://r4ds.had.co.nz/strings.html", "R for Data Science"),
               "book and are based on a collection of short sentences called the Harvard Sentences."),
        tags$ol(
          tags$li(tags$p(
            "Find sentences that contain a color (i.e. red, orange, yellow, green, blue, purple).",
            load_buttons("help_try_this", "hs", "colors"))),
          tags$li(tags$p(
            "Use the text from Exercise 1 and make sure that only full words that are colors are found.",
            HTML("E.g. <code>red</code> and not <code>flickered</code>."),
            tags$span(style = "display: inline-block;",
                      actionButton("help_try_this_hs_colors_word", "Load Pattern", class = "btn-xs btn-primary"),
                      actionButton("help_try_this_hs_colors_hint", "Show Hint", class = "btn-xs"))
            )),
          tags$li(tags$p(
            "Extract nouns from sentences by finding any word that comes after \"a\" or \"the\".",
            "Use", actionLink("help_try_this_hs_words_go2_groups", "Groups"),
            "to extract the article and possible noun separately and check your results with",
            HTML("<code>stringr::str_match()</code>:"),
            load_buttons("help_try_this", "hs", "words",
                         extra_btns = list(c("output", "Check str_match()")))
            )),
          tags$li(tags$p(
            "Switch the order of the two words following the articles", '"a" or "the"',
            "using", actionLink("help_try_this_hs_refs_go2_groups", "backreferences,"),
            "so that", tags$code("the birch canoe"), "would read",
            HTML("<code>the canoe birch</code>. Use <code>sub</code>"),
            "in the", tags$strong("Output"), "tab to replace the matched pattern.",
            load_buttons("help_try_this", "hs", "refs",
                         extra_btns = list(c("output", "Load Replacement")))
          ))
        ),
        tags$h4("Phone Numbers"),
        tags$p("This example is also from the",
               tags$a(href = "http://r4ds.had.co.nz/strings.html#other-types-of-pattern",
                      "R for Data Science"),
               "book. Phone numbers in the United States start with a 3-digit area code,",
               "followed by another 3 digits and a final 4-digit group.",
               "Sometimes the area code is wrapped in parenthesis, or sometimes dots or dashes",
               "are used to separate the digit groups. Try to extract each digit group from these phone numbers:",
               load_buttons("help_try_this", "phone",
                            extra_btns = list(c("output", "Check str_match()")))),
        tags$h4("CSS Unit Validation"),
        tags$p("This example is used in", tags$code("validateCssUnit()"),
               "in the", tags$a(href="https://www.r-pkg.org/pkg/htmltools", "htmltools package."),
               "CSS units can be integer or decimal numbers with units such as",
               "in, cm, mm, em, ex, pt, px, etc. (see the list",
               HTML('<a href="https://www.w3.org/Style/Examples/007/units.en.html">here</a>).'),
               "Try to extract the number and unit from these units:",
               load_buttons("help_try_this", "css")),
        tags$h4("Parse Github Repos"),
        tags$p("This example is from the",
               tags$a(href = "https://www.r-pkg.org/pkg/rematch2", "rematch2 package."),
               "Github repositories are often specified in like",
               HTML("<code>user/repo/subdir@ref*release</code> or <code>user/repo/subdir#PR</code>"),
               "where only", tags$code("user"), "and", tags$code("repo"), "are required elements.",
               "Try to extract each piece of the repo text and use",
               tags$code("rematch2::re_match()"), "to extract a tidy tibble of matches:",
               load_buttons("help_try_this", "github",
                            extra_btns = list(c("output", "Check re_match()"))))
      ) %>%
        as.character() %>%
        help_text()
    })

    observeEvent(input$help_try_this_hs_colors_text, {
      color_match <- "\\b(red|orange|yellow|green|blue|purple|he)\\b|red"
      color_text <- stringr::sentences[grepl(color_match, stringr::sentences)]
      color_text <- sample(color_text, 25)
      updateTextAreaInput(session, "text", value = paste(color_text, collapse = "\n"))
      showNotification("Text loaded! View it in Text tab", type = 'message')
    })

    observeEvent(input$help_try_this_hs_colors_pattern, {
      color_match <- "red|orange|yellow|green|blue|purple"
      updateTextInput(session, 'pattern', value = color_match)
      updateCheckboxGroupInput(session, "regex_options", selected = c('text_break_lines', 'perl'))
      showNotification("Pattern loaded! View it in RegEx tab", type = 'message')
    })

    observeEvent(input$help_try_this_hs_colors_word, {
      color_match <- "\\b(red|orange|yellow|green|blue|purple)\\b"
      updateTextInput(session, 'pattern', value = color_match)
      updateCheckboxGroupInput(session, "regex_options", selected = c('text_break_lines', 'perl'))
      showNotification("Pattern loaded! View it in RegEx tab", type = 'message')
    })

    observeEvent(input$help_try_this_hs_colors_hint, {
      showModal(
        modalDialog(title = "Hint \U0001f575", footer = NULL, easyClose = TRUE,
                    tags$p("Try using the", tags$strong("word boundary"), "anchor."))
      )
    })

    observeEvent(input$help_try_this_hs_words_go2_groups, {
      make_help_tab_text("groups")
    })

    observeEvent(input$help_try_this_hs_words_output, {
      updateSelectInput(session, 'regexFn', selected = 'str_match')
      showNotification("Go to Output tab to see results from str_match()", type = "message")
    })

    observeEvent(input$help_try_this_hs_words_text, {
      hs_text <- sample(stringr::sentences, 25)
      updateTextAreaInput(session, "text", value = paste(hs_text, collapse = "\n"))
      showNotification("Text loaded! View it in Text tab", type = 'message')
    })

    observeEvent(input$help_try_this_hs_words_pattern, {
      noun_pattern <- "(a|the) ([^ ]+)"
      updateTextInput(session, 'pattern', value = noun_pattern)
      updateCheckboxGroupInput(session, "regex_options", selected = c('text_break_lines'))
      updateSelectInput(session, 'regexFn', selected = "str_match")
      showNotification("Pattern loaded! View it in RegEx and Output tabs", type = 'message')
    })

    observeEvent(input$help_try_this_hs_refs_go2_groups, {
      make_help_tab_text("groups")
    })

    observeEvent(input$help_try_this_hs_refs_output, {
      regexFn_replacement_val <<- "\\1 \\3 \\2"
      updateSelectInput(session, 'regexFn', selected = 'sub')
      showNotification("Replacement loaded! Go to Output tab to see results", type = "message")
    })

    observeEvent(input$help_try_this_hs_refs_text, {
      hs_text <- sample(stringr::sentences, 25)
      updateTextAreaInput(session, "text", value = paste(hs_text, collapse = "\n"))
      showNotification("Text loaded! View it in Text tab", type = 'message')
    })

    observeEvent(input$help_try_this_hs_refs_pattern, {
      noun_pattern <- "(a|the) ([^ ]+) ([^ ]+)"
      updateTextInput(session, 'pattern', value = noun_pattern)
      updateCheckboxGroupInput(session, "regex_options", selected = c('text_break_lines'))
      showNotification("Pattern loaded! View it in RegEx tab", type = 'message')
    })

    observeEvent(input$help_try_this_phone_output, {
      updateSelectInput(session, 'regexFn', selected = 'str_match')
      showNotification("Go to Output tab to see results from str_match()", type = "message")
    })

    observeEvent(input$help_try_this_phone_text, {
      phone_number <- function() {
        first <- function() sample(2:9, 1)
        others <- function(n) sample(1:9, n, replace = TRUE)
        wrap_types <- c("parens", "dash", "space", "dot", "nothing")
        wrap <- function(x, type) {
          switch(
            match.arg(type, choices = wrap_types),
            parens = paste0("(", x, ")"),
            dash = paste0(x, "-"),
            space = paste0(x, " "),
            dot = paste0(x, "."),
            x
          )
        }

        area_code <- paste0(c(first(), others(2)), collapse = "")
        group1 <- paste0(c(first(), others(2)), collapse = "")
        group2 <- paste0(c(first(), others(3)), collapse = "")

        area_wrap <- sample(wrap_types, 1)
        other_wrap <- if (area_wrap == "parens") sample(wrap_types[-1], 1) else area_wrap

        paste0(wrap(area_code, area_wrap), wrap(group1, other_wrap), group2)
      }
      phone_numbers <- replicate(25, phone_number())
      updateTextAreaInput(session, "text", value = paste(phone_numbers, collapse = "\n"))
      showNotification("Text loaded! View it in Text tab", type = 'message')
    })

    observeEvent(input$help_try_this_phone_pattern, {
      phone_pattern <- "\\(?(\\d{3})[-). ]?(\\d{3})[- .]?(\\d{4})"
      updateTextInput(session, 'pattern', value = phone_pattern)
      updateCheckboxGroupInput(session, "regex_options", selected = c('text_break_lines'))
      showNotification("Pattern loaded! View it in RegEx tab", type = 'message')
    })

    observeEvent(input$help_try_this_github_text, {
      github_repos <- c(
        "metacran/crandb",
        "jeroenooms/curl@v0.9.3",
        "jimhester/covr#47",
        "hadley/dplyr@*release",
        "r-lib/remotes@550a3c7d3f9e1493a2ba"
      )
      updateTextAreaInput(session, "text", value = paste(github_repos, collapse = "\n"))
      showNotification("Text loaded! Go to RegEx Tab", type = 'message')
    })

    observeEvent(input$help_try_this_github_pattern, {
      owner_rx   <- "(?:(?<owner>[^/]+)/)?"
      repo_rx    <- "(?<repo>[^/@#]+)"
      subdir_rx  <- "(?:/(?<subdir>[^@#]*[^@#/]))?"
      ref_rx     <- "(?:@(?<ref>[^*].*))"
      pull_rx    <- "(?:#(?<pull>[0-9]+))"
      release_rx <- "(?:@(?<release>[*]release))"

      subtype_rx <- sprintf("(?:%s|%s|%s)?", ref_rx, pull_rx, release_rx)
      github_rx  <- sprintf(
        "^(?:%s%s%s%s|(?<catchall>.*))$",
        owner_rx, repo_rx, subdir_rx, subtype_rx
      )

      updateTextInput(session, 'pattern', value = github_rx)
      updateCheckboxGroupInput(session, "regex_options", selected = c('text_break_lines', 'perl'))
      showNotification("Pattern loaded! Go to RegEx Tab", type = 'message')
    })

    observeEvent(input$help_try_this_github_output, {
      updateSelectInput(session, 'regexFn', selected = 're_match')
      showNotification("Go to Output tab to see results from re_match()", type = "message")
    })

    observeEvent(input$help_try_this_css_text, {
      css_units <- c(
        "125%","16pt","2cm","7em","3ex","24pt",
        ".15in","20pc","5.9vw","3.0vh","2vmin"
      )
      showNotification("Example text loaded! Go to RegEx tab", type = "message")
      updateTextAreaInput(session, "text", value = paste(css_units, collapse = "\n"))
    })

    observeEvent(input$help_try_this_css_pattern, {
      pattern <- "^(auto|inherit|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|em|ex|pt|pc|px|vh|vw|vmin|vmax))$"
      updateTextInput(session, "pattern", value = pattern)
      updateCheckboxGroupInput(session, 'regex_options', selected = c('text_break_lines', 'perl'))
      showNotification("Pattern loaded! Go to RegEx tab", type = "message")
    })

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
  rx_unicode <- "\\u[0-9a-f]{4,8}"
  rx_hex <- "\\\\x[0-9a-f]{2}|\\\\x\\{[0-9a-f]{1,6}\\}"
  rx_octal <- "\\\\[0][0-7]{1,3}"
  rx_escape <- paste(rx_unicode, rx_hex, rx_octal, sep = "|")
  if (grepl(rx_escape, x, ignore.case = TRUE)) {
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
  ),
  "rematch2" = c(
    "re_match",
    "re_match_all",
    "re_exec",
    "re_exec_all"
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
  get_json <- purrr::possibly(jsonlite::fromJSON, NULL)
  gh_tags <- get_json(
    paste0("https://api.github.com/repos/", gh_user, "/", gh_repo, "/git/refs/tags"),
    simplifyDataFrame = TRUE
  )
  if (!is.null(gh_tags)) {
    gh_tags$tag <- sub("refs/tags/", "", gh_tags$ref, fixed = TRUE)
    gh_tags$version <- sub("^v\\.?", "", gh_tags$tag)
  }
  if (!is.null(gh_tags) && any(gh_tags$version > this_version)) {
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
