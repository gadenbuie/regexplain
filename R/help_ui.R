#' Generates Help Tab UI
#'
#' @param cheatsheet_only If TRUE then returns just basic regex
#'   explainer UI.
#' @keywords internal
generate_help_ui <- function(cheatsheet_only = TRUE) {
  miniUI::miniContentPanel(
    shiny::fillRow(
      flex = c(1, 4),
      shiny::tagList(
        shiny::tags$ul(
          id = "help-sidebar",
          if (!cheatsheet_only) shiny::tagList(
            shiny::tags$li(shiny::tags$strong("RegExplain"), class = "header"),
            shiny::tags$li(shiny::actionLink("help_default", "Introduction")),
            shiny::tags$li(shiny::actionLink("help_try_this", "Try These Examples")),
            shiny::tags$li(shiny::actionLink("help_resources", "Resources")),
            shiny::tags$li(class = "header", style = "margin-top: 5px;", shiny::tags$strong("Reference"))),
          shiny::tags$li("Character Classes", class = "header"),
          shiny::tags$ul(
            class = "subgroup",
            shiny::tags$li(shiny::actionLink("help_cat_character_classes_regular", "Regular")),
            shiny::tags$li(shiny::actionLink("help_cat_character_classes_prebuilt", "Pre-Built"))
          ),
          shiny::tags$li(shiny::actionLink("help_cat_anchors", "Anchors")),
          shiny::tags$li("Escaped Characters", class = "header"),
          shiny::tags$ul(
            class = "subgroup",
            shiny::tags$li(shiny::actionLink("help_cat_escaped_general", "General")),
            shiny::tags$li(shiny::actionLink("help_cat_escaped_hex", "Hex")),
            shiny::tags$li(shiny::actionLink("help_cat_escaped_control", "Control Characters"))
          ),
          shiny::tags$li(shiny::actionLink("help_cat_groups", "Groups")),
          shiny::tags$li(shiny::actionLink("help_cat_quantifiers", "Quantifiers"))
        )
      ),
      shiny::tags$div(
        style = "width: 100%; padding-left: 10px;",
        shiny::uiOutput('help_text_selected')
      )
    )
  )
}
