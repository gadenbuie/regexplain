#' Regex Cheatsheet Quick Reference
#'
#' The function behind the RegExplain Cheatsheet addin. Opens a summary of
#' regular expression syntax -- the RegExplain cheatsheet -- in an RStudio
#' viewer pane.
#'
#' @import miniUI
#' @import shiny
#' @export
regexplain_cheatsheet <- function() {
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))

  ui <- miniPage(
    shiny::includeCSS(system.file("styles", "gadget.css", package = "regexplain")),
    gadgetTitleBar(
      "Regex Cheatsheet Quick Reference",
      right = miniTitleBarButton("done", "OK", TRUE)
    ),
    generate_help_ui(cheatsheet_only = TRUE)
  )

  server <- function(input, output, session) {
    source(system.file("shiny/help_server.R", package = "regexplain"), local = TRUE)
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
