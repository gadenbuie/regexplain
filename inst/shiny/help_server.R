if (!exists('HELP_DEFAULT_TEXT')) HELP_DEFAULT_TEXT <- "<p>Select a category from the left sidebar.</p>"

help_text <- reactiveVal(HELP_DEFAULT_TEXT)

make_html_table <- function(x) {
  select(x, .data$regexp, .data$text) %>%
    knitr::kable(
      col.names = c("Regexp", "Text"),
      escape = FALSE,
      format = "html")
}

make_help_tab_text <- function(category = NULL, group = NULL) {
  x <- cheatsheet
  if (!is.null(category)) x <- filter(x, .data$category == !!category)
  if (!is.null(group))    x <- filter(x, .data$group == !!group)
  x %>%
    make_html_table() %>%
    help_text()
}

output$help_text_selected <- renderUI({
  help_body <- help_text()
  if (inherits(help_body, "shiny.tag.list")) {
    help_body
  } else HTML(help_body)
})

observeEvent(input$help_default, {
  help_text(HELP_DEFAULT_TEXT)
})

observeEvent(input$help_cat_character_classes_regular, {
  make_help_tab_text("character classes", "regular")
})

observeEvent(input$help_cat_character_classes_prebuilt, {
  make_help_tab_text("character classes", "pre-built")
})

observeEvent(input$help_cat_anchors, {
  make_help_tab_text("anchors")
})

observeEvent(input$help_cat_escaped_general, {
  make_help_tab_text("escaped characters", "general")
})

observeEvent(input$help_cat_escaped_hex, {
  make_help_tab_text("escaped characters", "hex")
})

observeEvent(input$help_cat_escaped_control, {
  make_help_tab_text("escaped characters", "control characters")
})

observeEvent(input$help_cat_groups, {
  make_help_tab_text("groups")
})

observeEvent(input$help_cat_quantifiers, {
  make_help_tab_text("quantifiers")
})

