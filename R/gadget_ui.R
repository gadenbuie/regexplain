# ---- Regex Gadget UI ----

#' @inheritParams regex_gadget
regexplain_gadget_ui <- function(text = NULL, pattern = NULL, start_page = "Text") {
  miniPage(
    shiny::includeCSS(system.file("styles", "groups.css", package = "regexplain")),
    shiny::includeCSS(system.file("styles", "gadget.css", package = "regexplain")),
    gadgetTitleBar(
      "RegExplain",
      right = miniTitleBarButton("done", "Send RegEx To Console", TRUE)
    ),
    miniTabstripPanel(
      selected = match.arg(start_page, c("Text", "RegEx", "Output", "Help")),
      # --- UI - Tab - Text ----
      miniTabPanel(
        "Text",
        icon = icon("file-text-o"),
        miniContentPanel(
          fillCol(
            textAreaInputAlt(
              "text",
              label = "Text to search or parse",
              value = paste(text, collapse = "\n"),
              resize = "both",
              width = "100%",
              height = "90%",
              placeholder = "Paste, enter, or edit your sample text here."
            )
          )
        )
      ),
      # ---- UI - Tab - Regex ----
      miniTabPanel(
        "RegEx",
        icon = icon("terminal"),
        miniContentPanel(
          tags$div(
            id = "regexPage",
            class = "container-fluid",
            style = "height: 90%",
            fluidRow(
              id = "regexInput",
              tags$div(
                class = "col-12",
                textInputCode(
                  "pattern",
                  HTML(
                    "RegEx",
                    format(
                      # <button type="button" class="btn btn-lg btn-danger" data-toggle="popover" title="Popover title" data-content="content">Click to toggle popover</button>
                      tags$button(
                        id = "regex-format-help",
                        type = "button",
                        class = "btn btn-sm btn-link",
                        style = "line-height: 1;padding: 0;font-size: inherit;",
                        `data-toggle` = "popover",
                        `data-trigger` = "hover",
                        `data-placement` = "bottom",
                        `data-content` = paste(
                          "<p>Enter your regular expressions in",
                          "<em>standard RegExp</em> format.</p>",
                          "<p>In other words, <strong>use single slashes</strong> <code>\\</code>",
                          "rather than double slahes <code>\\\\</code>.</p>"
                        ),
                        title = "Regular Expression Format",
                        shiny::icon("question-circle", "fa-sm")
                      )
                    )
                  ),
                  width = "100%",
                  value = pattern,
                  placeholder = "Standard RegEx, e.g. \\w+_\\d{2,4}\\s+",
                  tags$span(
                    class = "input-group-btn",
                    id = "library_show_container",
                    actionButton("library_show", "Library", class = "btn-primary")
                  )
                ),
                tags$script(
                  "const $regexHelp = $('#regex-format-help');
                  $regexHelp.popover({html: true});"
                )
              )
            ),
            fluidRow(
              id = "regexOptions",
              checkboxGroupInput(
                "regex_options",
                label = HTML(
                  '<div style="font-size: 1.25rem;">',
                  "Option Groups: ",
                  '<span style="color: #337ab7;">RegExplain</span>,',
                  '<span style="color: #5cb85c;">All</span>, ',
                  '<span style="color: #f0ad4e;">Base only</span>',
                  "</div>"
                ),
                inline = TRUE,
                width = "90%",
                choiceValues = list(
                  "global",
                  "text_break_lines",
                  "ignore.case",
                  "fixed",
                  "perl",
                  "useBytes"
                ),
                choiceNames = list(
                  HTML('<span style="color: #337ab7;">Global</span>'),
                  HTML('<span style="color: #337ab7;">Break Lines</span>'),
                  HTML('<span style="color: #5cb85c;">Ignore Case</span>'),
                  HTML('<span style="color: #5cb85c;">Fixed/Literal</span>'),
                  HTML('<span style="color: #f0ad4e;">Perl Style</span>'),
                  HTML('<span style="color: #f0ad4e;">Use Bytes</span>')
                ),
                selected = c("text_break_lines", "perl")
              )
            ),
            tags$div(
              id = "regexResult",
              class = "gadget-result",
              style = "overflow-y: scroll; height: 100%; max-height: calc(100% - 90px);",
              htmlOutput("result")
            )
          )
        )
      ),
      # ---- UI - Tab - Output ----
      miniTabPanel(
        "Output",
        icon = icon("table"),
        miniContentPanel(
          fillCol(
            flex = c(1, 3),
            inputPanel(
              tags$div(
                width = "100%",
                selectInput(
                  "regexFn",
                  label = "Apply Function",
                  choices = available_regex_functions()
                ),
                tags$span(
                  class = "help-block",
                  style = "font-size:1.25rem; margin-top:-10px; margin-bottom:0px; margin-left:4px;",
                  "Adjust options in RegEx tab"
                )
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
        "Help",
        icon = icon("support"),
        generate_help_ui(cheatsheet_only = FALSE)
      )
    )
  )
}
