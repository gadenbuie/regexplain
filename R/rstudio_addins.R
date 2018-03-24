#' RegExplain Addin
#'
#' Addin for regexplain. If the selection does not contain multiple lines, the
#' addin tries to evaluate the selection and coerce the result to a character
#' string. Only unique lines are passed to the gadget to maximize screen space.
#' The max number of lines returned is set by the option
#' `regexplain.addin.max_lines`, with a default value of 100 lines.
#' If evaluating the selection does not produce a character vector without
#' errors, the original selection is returned.
#'
#' @keywords internal
regexplain_addin <- function() {
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Get context text
  ctx_text <- context$selection[[1]]$text

  max_lines <- getOption("rexeplain.addin.max_lines", 100)

  # If it is one line and evaluates to something, use that
  # Otherwise treat as text
  obj <- tryCatch({
    if (grepl("\n", ctx_text)) {
      ctx_text[1:min(length(ctx_text), max_lines)]
    } else {
      x <- eval(parse(text = ctx_text))
      if (inherits(x, "list")) x <- unlist(x)
      x <- as.character(x)
      if (length(x) == 1 && grepl("\n", x))
        x <- strsplit(x, "\n")[[1]]
      x <- unique(x)
      if (length(x) > max_lines) {
        message(ctx_text, " gave ", length(x), " lines, limiting to first ",
                max_lines, " unique lines. Set ",
                "options('regexplain.addin.max_lines') to a higher value to ",
                "increase the number of lines.")
        x <- unique(x)
        x[1:min(length(x), max_lines)]
      } else x
    }
  },
  error = function(e) {as.character(ctx_text[1:min(length(ctx_text), max_lines)])})

  regex_gadget(if (length(obj) && obj != "") obj)

}

#' Regexplain File Addin
#'
#' See [regexplain_addin]. Opens file chooser to pick file, reads lines, returns
#' first `regexplain.addin.max_lines` (default 100).
#'
#' @keywords internal
regexplain_file <- function() {
  fname <- file.choose()
  x <- readLines(fname)
  max_lines <- getOption("rexeplain.addin.max_lines", 100)
  if (length(x) > max_lines) {
    message("There were ", format(length(x), big.mark = ","), " lines in ",
            fname, "\nUsing only first ", max_lines, ".\n",
            "Set options('regexplain.addin.max_lines') to a higher value to ",
            "increase the number of lines.")
    x <- x[1:max_lines]
  }
  regex_gadget(x, "RegEx")
}
