# ---- Modified Shiny Inputs ----
# The shiny package as a whole is distributed under GPL-3
# (GNU GENERAL PUBLIC LICENSE version 3).
# See https://github.com/rstudio/shiny/blob/master/LICENSE

#' Modified Text Area Input
#'
#' Standard [shiny::textAreaInput()] with additional `is_code` parameter, added
#' code font style for the input text and with `autocomplete`, `autocorrect`,
#' `autocapitalize` and `spellcheck` set to `off` or `false`.
#'
#' @inheritParams shiny::textAreaInput
#' @param is_code Should the text input be considered verbatim code input?
#' @family modified shiny inputs
textAreaInputAlt <- function(inputId, label, value = "", width = NULL, height = NULL,
                             cols = NULL, rows = NULL, placeholder = NULL, resize = NULL,
                             is_code = TRUE) {
  `%AND%` <- getFromNamespace("%AND%", "shiny")

  value <- shiny::restoreInput(id = inputId, default = value)

  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
  }

  style <- paste(
    if (!is.null(width))  paste0("width: ",  shiny::validateCssUnit(width),  ";"),
    if (!is.null(height)) paste0("height: ", shiny::validateCssUnit(height), ";"),
    if (!is.null(resize)) paste0("resize: ", resize, ";"),
    if (is_code) 'font-family: "Monaco", "Inconsolata", monospace;'
  )

  parent_style <- paste(
    if (!is.null(width) && grepl("%", width)) paste0("width: ", width, ";"),
    if (!is.null(height) && grepl("%", height)) paste0("height: ", height, ";")
  )

  # Workaround for tag attribute=character(0) bug:
  #   https://github.com/rstudio/htmltools/issues/65
  if (length(style) == 0) style <- NULL

  shiny::div(class = "form-group shiny-input-container",
             label %AND% shiny::tags$label(label, `for` = inputId),
             style = if (!parent_style %in% c(" ", "", "  ")) parent_style,
             shiny::tags$textarea(
               id = inputId,
               class = "form-control",
               placeholder = placeholder,
               style = style,
               rows = rows,
               cols = cols,
               autocomplete = "off",
               autocorrect = "off",
               autocapitalize = "off",
               spellcheck = "false",
               value
             )
  )
}

#' Modified Text Input
#'
#' Standard [shiny::textInput()] with additional `width` parameter, added code
#' font style for the input text and with `autocomplete`, `autocorrect`,
#' `autocapitalize` and `spellcheck` set to `off` or `false`.
#'
#' @inheritParams shiny::textInput
#' @param width Width of `shiny-input-container` div.
#' @family modified shiny inputs
textInputCode <- function(inputId, label, value = "", width = NULL,
                          placeholder = NULL, ...) {
  `%AND%` <- getFromNamespace("%AND%", "shiny")
  value <- shiny::restoreInput(id = inputId, default = value)

  shiny::div(class = "input-group shiny-input-container",
             style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
             label %AND% shiny::tags$label(label, `for` = inputId),
             shiny::tags$input(id = inputId, type="text", class="form-control", value=value,
                               style = 'font-family: "Monaco", "Inconsolata", monospace;',
                               autocomplete = "off", autocorrect = "off",
                               autocapitalize = "off", spellcheck = "false",
                               placeholder = placeholder),
             ...
  )
}
