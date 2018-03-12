#' @importFrom dplyr "%>%" mutate filter group_by summarize select
#' @importFrom utils getFromNamespace
NULL

escape_html <- function(x) {
  x = gsub("&", "&amp;", x)
  x = gsub("<", "&lt;", x)
  x = gsub(">", "&gt;", x)
  x = gsub("\"", "&quot;", x)
  x = gsub(" ", "&nbsp;", x)
  x
}

escape_backslash <- function(x) {
  gsub("\\\\", "\\\\\\\\", x)
}
