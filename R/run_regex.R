#' @export
run_regex <- function(
  text,
  pattern,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  invert = FALSE
) {
  m <- regexec(pattern, text, ignore.case, perl, fixed, useBytes)
  x <- purrr::map(m, function(mi) list('idx' = purrr::map2(mi, attr(mi, "match.length"), ~ if(.x[1] != -1) c(.x, .x + .y - 1L))))
  y <- purrr::map(text, ~ list(text = .))
  z <- purrr::map(regmatches(text, m), ~ list(m = .))
  purrr::map(seq_along(x), ~ list(text = y[[.]][[1]], idx = x[[.]][[1]], m = z[[.]][[1]]))
}

wrap_result <- function(x, escape = FALSE) {
  if (is.null(x$idx[[1]])) return(if (escape) escape_html(x$text) else x$text)
  text <- x$text
  idx <- x$idx
  len_idx <- length(idx)
  inserts <- data.frame(
    i = 1:len_idx - 1,
    start = purrr::map_int(idx, ~ .[1]),
    end = purrr::map_int(idx, ~ .[2]) + 1
  ) %>%
    mutate(
      class = sprintf("group g%02d", i),
      pad = 0
    )
  for (j in seq_len(nrow(inserts))) {
    if (inserts$i[j] == 0) next
    overlap <- filter(
      inserts[1:(j-1), ],
      i != 0,
      start <= !!inserts$start[j] & end >= !!inserts$end[j])
    inserts[j, 'pad'] <- inserts$pad[j] + nrow(overlap)
  }
  inserts <- inserts %>%
    tidyr::gather(type, loc, start:end) %>%
    mutate(
      class = ifelse(pad > 0, sprintf("%s pad%02d", class, pad), class),
      insert = ifelse(type == 'start', sprintf('<span class="%s">', class), "</span>")
    ) %>%
    group_by(loc, type) %>%
    summarize(insert = paste(insert, collapse = ''))

  # inserts now gives html (span open and close) to insert and loc
  # first split text at inserts$loc locations,
  # then recombine by zipping with inserts$insert text
  # start at 0, unless there's a hit on first character
  # end at nchar(text) + 1 because window is idx[k] to idx[k+1]-1
  idx_split <- c(0 - (inserts$loc[1] == 0), inserts$loc)
  if (!(nchar(text) + 1) %in% idx_split)
    idx_split <- c(idx_split, nchar(text) + 1)
  text_split <- c()
  for (k in seq_along(idx_split[-1])) {
    text_split <- c(text_split, substr(text, idx_split[k], idx_split[k+1] - 1))
  }
  out <- c()
  for (j in seq_along(text_split)) {
    out <- c(
      out,
      ifelse(escape, escape_html(text_split[j]), text_split[j]),
      if (!is.na(inserts$insert[j])) inserts$insert[j]
    )
  }
  paste(out, collapse = '')
}

wrap_regex <- function(pattern, escape = TRUE, exact = TRUE) {
  stopifnot(length(pattern) == 1)
  if(escape) pattern <- escape_html(pattern)
  r_open_parens <- "(?<![\\\\])\\("
  x <- strsplit(pattern, r_open_parens, perl = TRUE)[[1]]
  first <- x[1]
  x <- x[-1]
  if (length(x)) {
    x <- paste0(
      '<span class="g', sprintf("%02d", seq_along(x)), '">(',
      x,
      collapse = ""
    )
    x <- gsub("(?<![\\\\])\\)", ")</span>", x, perl = TRUE)
  }
  if (exact) x <- escape_backslash(x)
  paste0(first, x)
}

#' View grouped regex results
#'
#' @param text Text to search
#' @param pattern Regex pattern to look for
#' @param render Render results to an HTML doc and open in RStudio viewer?
#' @param escape Escape HTML-related characters in `text`?
#' @param knitr Print into knitr doc? If `TRUE`, marks text as `asis_output` and
#'   sets `render = FALSE` and `escape = TRUE`.
#' @param exact Should regex be displayed as entered by the user into R console
#'   or source (default)? When `TRUE`, regex is displayed with the double `\\`
#'   required for escaping backslashes in R. When `FALSE`, regex is displayed
#'   as interpreted by the regex engine (i.e. double `\\` as a single `\`).
#' @param ... Passed to [run_regex]
#' @export
view_regex <- function(
  text,
  pattern,
  ...,
  render = TRUE,
  escape = render,
  knitr = FALSE,
  exact = escape
) {
  if (knitr) {
    render <- FALSE
    escape <- TRUE
  }
  res <- run_regex(text, pattern, ...)
  res <- purrr::map_chr(res, wrap_result, escape = escape)
  res <- paste("<p class='results'>", res, "</p>")
  if (!nchar(pattern)) res <- paste("<p class='results'>", text, "</p>")
  if (knitr) return(knitr::asis_output(res))
  if (!render) return(res)
  head <- c(
    "---", "pagetitle: View Regex", "---",
    "<h5>Regex</h5>",
    "<p><pre style = 'font-size: 1em;'>", wrap_regex(pattern, escape, exact), "</pre></p>",
    "<h5>Results</h5>"
  )
  res <- c(head, res)
  tmp <- tempfile(fileext = ".Rmd")
  cat(res, file = tmp, sep = "\n")
  tmp_html <- suppressWarnings(
    rmarkdown::render(
      tmp,
      output_format = rmarkdown::html_document(css = system.file('style.css', package='regexhelp')),
      quiet = TRUE
  ))
  rstudioapi::viewer(tmp_html)
}
