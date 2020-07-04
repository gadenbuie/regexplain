#' Deprecated or renamed functions
#'
#' These functions in regexplain have been deprecated or renamed.
#'
#' @param ... Arguments passed to the new or renamed functions.
#' @return The result of the new or renamed function.
#' @name regexplain-deprecated
NULL

#' @description
#' Use [regexplain_gadget()] instead of `regex_gadget()`.
#'
#' @export
#' @keywords internal
#' @rdname regexplain-deprecated
regex_gadget <- function(...) {
  .Deprecated(msg = "regex_gadget() was renamed. Please use `regexplain_gadget()` instead.")
  regexplain_gadget(...)
}
