HELP_DEFAULT_TEXT <- "<p>Select a category from the left sidebar.</p>"

help_text <- reactiveVal(HELP_DEFAULT_TEXT)

make_html_table <- function(x) {
  select(x, .data$regexp, .data$text) %>%
    knitr::kable(
      col.names = c("Regexp", "Text"),
      escape = FALSE,
      format = "html")
}

output$help_text_selected <- renderUI({
  help_body <- help_text()
  if (inherits(help_body, "shiny.tag.list")) {
    help_body
  } else HTML(help_body)
})

observeEvent(input$help_cat_character_classes_regular, {
  cheatsheet %>%
    filter(.data$category == "character classes", .data$group == "regular") %>%
    make_html_table() %>%
    help_text()
})

observeEvent(input$help_cat_character_classes_prebuilt, {
  cheatsheet %>%
    filter(.data$category == "character classes", .data$group == "pre-built") %>%
    make_html_table() %>%
    help_text()
})

observeEvent(input$help_cat_anchors, {
  cheatsheet %>%
    filter(.data$category == "anchors") %>%
    make_html_table() %>%
    help_text()
})

observeEvent(input$help_cat_escaped_general, {
  cheatsheet %>%
    filter(.data$category == "escaped characters", .data$group == "general") %>%
    make_html_table() %>%
    help_text()
})

observeEvent(input$help_cat_escaped_hex, {
  cheatsheet %>%
    filter(.data$category == "escaped characters", .data$group == "hex") %>%
    make_html_table() %>%
    help_text()
})

observeEvent(input$help_cat_escaped_control, {
  cheatsheet %>%
    filter(.data$category == "escaped characters", .data$group == "control characters") %>%
    make_html_table() %>%
    help_text()
})

observeEvent(input$help_cat_groups, {
  cheatsheet %>%
    filter(.data$category == "groups") %>%
    make_html_table() %>%
    help_text()
})

observeEvent(input$help_cat_quantifiers, {
  cheatsheet %>%
    filter(.data$category == "quantifiers") %>%
    make_html_table() %>%
    help_text()
})

