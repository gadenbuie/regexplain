# ---- Server ----

regexplain_gadget_server <- function(update_available = NULL) {
  # update_available = check_version()
  function(input, output, session) {
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
                "devtools::update_packages(\"regexplain\")"
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

    observe({
      if (getOption('regexplain.debug.gadget.text', FALSE)) {
        cat("\ntext   :", rtext())
      }
      if (getOption('regexplain.debug.gadget.pattern', FALSE)) {
        cat("\npattern:", pattern())
      }
      if (getOption('regexplain.debug.gadget.replacement', FALSE)) {
        cat("\nreplace:", replacement())
      }
      cat("\n")
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
      delay <- getOption('regexplain.input_delay_ms', NULL)
      if (!is.null(delay)) invalidateLater(delay, session)
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
            exact  = FALSE,
            global = "global" %in% input$regex_options),
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

    # ---- Server - Tab - RegEx - Library ----
    library_patterns <- get_regex_library()

    this_pattern <- reactive({
      req(input$library_pattern)
      purrr::keep(library_patterns, ~ .$name == input$library_pattern) %>%
        purrr::flatten()
    })

    observeEvent(input$library_show, {
      showModal(
        modalDialog(
          title = "Regex Library",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("library_apply_pattern", "Use Pattern", class = "btn-success")
          ),
          selectInput("library_pattern", "Pattern",
                      choices = c("Choose pattern" = "",
                                  purrr::set_names(purrr::map_chr(library_patterns, 'name')))),
          uiOutput("library_pattern_info")
        )
      )
    })

    output$library_pattern_info <- renderUI({
      req(this_pattern())
      tp <- this_pattern()
      rx_url <- "((https?|ftp|file)://)?([[:alnum:].-]+)\\.([a-zA-Z.]{2,6})([/[[:alpha:].-]*)*/?"
      tagList(
        tags$h5("Description"),
        tags$p(HTML(tp$description)),
        tags$h5("Pattern"),
        tags$pre(tp$regex),
        if (!is.null(tp$source)) tags$p(
          "Source:",
          if (grepl(rx_url, tp$source)) tags$a(href = tp$source, tp$source) else HTML(tp$source)
        )
      )
    })

    observeEvent(input$library_apply_pattern, {
      updateTextInput(session, "pattern", value = this_pattern()$regex, placeholder = "")
      updateSelectInput(session, "template", selected = "")
      removeModal()
    })

    observe({
      is_empty <- input$pattern == ""
      if (is_empty) updateTextInput(
        session, "pattern",
        placeholder = "Standard RegEx, e.g. \\w+_\\d{2,4}\\s+")
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
            stringr_regex(
              pattern(),
              ignore_case = 'ignore.case' %in% input$regex_options,
              literal = 'fixed' %in% input$regex_options
            ),
            replacement()
          )
        } else {
          regexFn(
            rtext(),
            stringr_regex(
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

    stringr_regex <- function(pattern, ignore_case = FALSE, literal = FALSE) {
      if (!requireNamespace("stringr", quietly = TRUE)) return(NULL)
      do.call(
        eval(parse(text = "stringr::regex")),
        list(pattern = pattern, ignore_case, ignore_case, literal = literal)
      )
    }

    # ---- Server - Tab - Help ----
    HELP_DEFAULT_TEXT <- c(
      "<h3>Welcome to RegExplain</h3>",
      "<p>If you're new to regular expressions, one of the best places to start is <a href=\"http://stringr.tidyverse.org/articles/regular-expressions.html\">the regular expressions vignette</a> from <code>stringr</code>. The chapter on strings in <a href=\"http://r4ds.had.co.nz/strings.html\">R for Data Science</a> is also an excellent first resource.</p>",
      "<p><strong>Exploring or looking for a challenge?</strong> Click on <i>Try These Examples</i> to see what you can do with this addin.</p>",
      "<h4>Getting Started</h4>",
      "<ul>",
      "<li><p><i class=\"fa fa-file-text-o\"></i> Enter or edit the <strong>Text</strong> you want to search.</p></li>",
      "<li><p><i class=\"fa fa-terminal\"></i> Edit your <strong>RegEx</strong> and view matches in real time.</p></li>",
      "<li><p><i class=\"fa fa-table\"></i> Test the <strong>Output</strong> of your regular expression with common functions, including <i>search and replace</i> functions.</p></li>",
      "<li><p><i class=\"fa fa-support\"></i> Get <strong>Help</strong> and look up the regular expression syntax.</p></li>",
      "</ul>",
      "<h4>Escaping characters</h4>",
      "<p>In order to store a backslash (<code>\\</code>) as a character in R, backslashes need to be escaped...with another backslash! To write a literal <code>\\</code> in an R character string, you need to actually store <code>&quot;\\\\&quot;</code>.</p>",
      "<p>In regular expressions, <code>\\w</code> stands for any alphabetical character, but to store it in a string in R you need <code>&quot;\\\\w&quot;</code>.</p>",
      "<p>Inside <strong>RegExplain</strong>, however, standard regular expressions can be used so that you can easily copy patterns from other places. When you click on the <span class=\"btn btn-xs btn-primary\">Send RegEX to Console</span> button, the necessary extra <code>\\</code> will be included.</p>",
      "<p>An extra backslash is still needed to match a literal <code>\\</code> in standard regular expressions. This means that you will need to enter <code>\\\\</code> in the <strong>RegEx</strong> tab, and the output to R will be <code>&quot;\\\\\\\\&quot;</code>.</p>"
    )

    # avoid CRAN check NOTES
    help_text <- NULL          # in help_server.R
    make_help_tab_text <- NULL # in help_server.R
    source(system.file("shiny", "help_server.R", package = "regexplain"), local = TRUE)

    observeEvent(input$help_resources, {
      tagList(
        tags$h3("Resources"),
        tags$p("There are lots of great resources available for learning and working with regular expressions."),
        tags$h4("Regular Expressions in R"),
        tags$ul(
          tags$li(tags$p("The", tags$a(href = "http://stringr.tidyverse.org/articles/regular-expressions.html", "Regular Expressions vignette"),
                         "from", tags$code("stringr"), "is an excellent first introduction to regular expressions in R.")),
          tags$li(tags$p("The", tags$a(href = "http://r4ds.had.co.nz/strings.html", "chapter on strings"),
                         "in", tags$a(href = "http://r4ds.had.co.nz/", "R for Data Science"),
                         "is also a great overall introduction.")),
          tags$li(tags$p("RStudio's",
                         tags$a(href = "https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf", "RegEx CheatSheet"),
                         "is a good pocket reference.")),
          tags$li(tags$p("Or try the", tags$strong("Regexplain Cheatsheet"), "addin installed with this package."))
        ),
        tags$h4("Online Resources"),
        tags$ul(
          tags$li(tags$a(href = "https://github.com/aloisdg/awesome-regex", "Awesome RegEx")),
          tags$li(tags$a(href = "https://www.regular-expressions.info", "Regular-Expressions.info")),
          tags$li(tags$a(href = "https://projects.lukehaas.me/regexhub", "Regex Hub"), "- common regex patterns"),
          tags$li(tags$a(href = "http://regexlib.com/DisplayPatterns.aspx", "RegExLib.com"), "- large collection of searchable patterns")
        ),
        tags$h4("Live Preview and Explanations"),
        tags$ul(
          tags$li(tags$a(href = "https://regexr.com", "https://regexr.com")),
          tags$li(tags$a(href = "https://regexper.com/", "https://regexper.com")),
          tags$li(tags$a(href = "https://debuggex.com", "https://debuggex.com")),
          tags$li(tags$a(href = "https://regex101.com", "https://regex101.com")),
          tags$li(tags$a(href = "http://rick.measham.id.au/paste/explain", "http://rick.measham.id.au/paste/explain")),
          tags$li(tags$a(href = "http://www.mactechnologies.com/index.php?page=downloads#regexrx", "RegexRx"), "(app)"),
          tags$li(tags$a(href = "https://www.regexbuddy.com/", "Regex Buddy"), "(paid app)")
        ),
        tags$h4("Regex and String R Packages"),
        tags$dl(
          tags$dt(tags$a(href = "https://stringr.tidyverse.org/", "stringr")),
          tags$dd("A cohesive set of functions designed to make working with strings as easy as possible"),
          tags$dt(tags$a(href = "http://www.gagolewski.com/software/stringi/", "stringi")),
          tags$dd("THE R package for very fast, correct, consistent, and convenient string/text processing"),
          tags$dt(tags$a(href = "https://github.com/trinker/regexr", "regexr")),
          tags$dd("An R framework for constructing and managing human readable regular expressions"),
          tags$dt(tags$a(href = "https://github.com/kevinushey/rex", "rex")),
          tags$dd("Friendly Regular Expressions: complex regular expressions from human readable expressions"),
          tags$dt(tags$a(href = "https://github.com/richierocks/rebus", "rebus")),
          tags$dd("Build regular expressions in a human readable way"),
          tags$dt(tags$a(href = "https://www.r-pkg.org/pkg/qdapRegex", "qdapRegex")),
          tags$dd("A collection of regular expression tools for extraction/removal/replacement of common patterns in text documents"),
          tags$dt(tags$a(href = "https://github.com/AdamSpannbauer/r_regex_tester_app", "R Regex Tester")),
          tags$dd("A Shiny app for testing regular expressions")
        )
      ) %>%
        as.character() %>%
        help_text()
    })

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
      color_match <- "\\b(red|orange|yellow|green|blue|purple)\\b|red"
      if (requireNamespace("stringr", quietly = TRUE)) {
        color_text <- stringr::sentences[grepl(color_match, stringr::sentences)]
        color_text <- sample(color_text, 25)
        updateTextAreaInput(session, "text", value = paste(color_text, collapse = "\n"))
        showNotification("Text loaded! View it in Text tab", type = 'message')
      } else {
        showNotification("Please install {stringr} for this example", type = 'error')
      }
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
      if (requireNamespace("stringr", quietly = TRUE)) {
        hs_text <- sample(stringr::sentences, 25)
        updateTextAreaInput(session, "text", value = paste(hs_text, collapse = "\n"))
        showNotification("Text loaded! View it in Text tab", type = 'message')
      } else {
        showNotification("Please install {stringr} for this example", type = 'error')
      }
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
      if (requireNamespace("stringr", quietly = TRUE)) {
        hs_text <- sample(stringr::sentences, 25)
        updateTextAreaInput(session, "text", value = paste(hs_text, collapse = "\n"))
        showNotification("Text loaded! View it in Text tab", type = 'message')
      } else {
        showNotification("Please install {stringr} for this example", type = 'error')
      }
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
        if (any(c("perl", "fixed", "ignore.case", "useBytes") %in% input$regex_options)) {
          options <- input$regex_options[!input$regex_options %in% c("text_break_lines", "global")]
          options <- paste0(options, "=TRUE", collapse = ", ")
          pattern <- paste(pattern, "#", options)
        }
        if ("regexFn_replacement" %in% names(input) && isTruthy(replacement())) {
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
}
