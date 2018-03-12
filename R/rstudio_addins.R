#' regexplain_addin
#'
#' @keywords internal
regexplain_addin <- function() {
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Get context text
  ctx_text <- context$selection[[1]]$text

  # If it is one line and evaluates to something, use that
  # Otherwise treat as text
  obj <- tryCatch({
    if (grepl("\n", ctx_text)) {
      ctx_text[1:min(length(ctx_text), 100)]
    } else {
      x <- eval(parse(text = ctx_text))
      x <- as.character(x)
      if (length(x) == 1 && grepl("\n", x))
        x <- strsplit(x, "\n")[[1]]
      if (length(x) > 10) {
        message(ctx_text, " gave ", length(x), " lines, limiting to first 10 unique lines.")
        x <- unique(x)
        x[1:min(length(x), 10)]
      } else x
    }
  },
  error = function(e) {as.character(ctx_text[1:min(length(ctx_text), 100)])})

  regex_gadget(if (length(obj) && obj != "") obj)

}
