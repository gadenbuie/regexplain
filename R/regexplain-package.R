#' @section Options:
#' - `regexplain.no.check.version` if `TRUE` or unset, the app will check for
#'   updates on the first run of the session.
#' - `regexplain.addin.max_lines` controls the number of lines of text imported
#'   into the gadget, defaults to 100. Higher values will degrade performance.
#' @importFrom dplyr "%>%" mutate filter group_by summarize select
#' @importFrom utils getFromNamespace installed.packages packageVersion globalVariables
#' @importFrom rlang .data
#' @keywords internal
"_PACKAGE"
