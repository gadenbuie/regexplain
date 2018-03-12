#' Regex Cheatsheet Quick Reference
#'
#' @import miniUI
#' @import shiny
#' @export
regexplain_cheatsheet <- function() {
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))

  ui <- miniPage(
    shiny::includeCSS(system.file("gadget.css", package = "regexplain")),
    gadgetTitleBar(
      "Regex Cheatsheet Quick Reference",
      right = miniTitleBarButton("done", "OK", TRUE)
    ),
    help_ui("help")
  )

  server <- function(input, output, session) {
    help_text <- callModule(help_server, "help")
    observeEvent(input$done, {
      stopApp()
    })
    observeEvent(input$cancel, {
      stopApp()
    })
  }

  viewer <- shiny::paneViewer(700)
  runGadget(ui, server, viewer = viewer)
}

help_ui <- function(id) {
  ns <- NS(id)

  miniContentPanel(
    fillRow(
      flex = c(1, 4),
      tagList(
        tags$ul(
          id = "help-sidebar",
          tags$li("Character Classes", class = "header"),
          tags$ul(
            class = "subgroup",
            tags$li(actionLink(ns("help_cat_character_classes_regular"), "Regular")),
            tags$li(actionLink(ns("help_cat_character_classes_prebuilt"), "Pre-Built"))
          ),
          tags$li(actionLink(ns("help_cat_anchors"), "Anchors")),
          tags$li("Escaped Characters", class = "header"),
          tags$ul(
            class = "subgroup",
            tags$li(actionLink(ns("help_cat_escaped_general"), "General")),
            tags$li(actionLink(ns("help_cat_escaped_hex"), "Hex")),
            tags$li(actionLink(ns("help_cat_escaped_control"), "Control Characters"))
          ),
          tags$li(actionLink(ns("help_cat_groups"), "Groups")),
          tags$li(actionLink(ns("help_cat_quantifiers"), "Quantifiers"))
        )
      ),
      tags$div(
        style = "width: 100%; padding-left: 10px;",
        uiOutput(ns('help_text_selected'))
      )
    )
  )
}

help_server <- function(input, output, session) {
  help_text <- reactiveVal("<p>Select a category from the left sidebar.</p>")

  make_html_table <- function(x) {
    select(x, .data$regexp, .data$text) %>%
      knitr::kable(
        col.names = c("Regexp", "Text"),
        escape = FALSE,
        format = "html")
  }

  output$help_text_selected <- renderUI({
    HTML(help_text())
  })

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
}
