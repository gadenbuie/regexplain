#' Extract matched groups from regexp
#'
#' @param text Text to search
#' @param pattern regexp
#' @inheritParams base::regexec
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
  # Use regex to get matches by group, gives start index and length
  m <- regexec(pattern, text, ignore.case, perl, fixed, useBytes)
  # Convert to start/end index
  x <- purrr::map(m, function(mi) {
    list(
      'idx' = purrr::map2(mi, attr(mi, "match.length"),
                          ~ if(.x[1] != -1) c(.x, .x + .y)))
  })
  # Store text and original regexc result with same hierarchy
  y <- purrr::map(text, ~ list(text = .))
  z <- purrr::map(regmatches(text, m), ~ list(m = .))
  # Zip text, indexes and regexc match object lists
  purrr::map(seq_along(x), ~ list(text = y[[.]][[1]], idx = x[[.]][[1]], m = z[[.]][[1]]))
}

#' Wrap matches in HTML span tags to colorize via CSS
#'
#' @param x Individual list item in list returned by [run_regex()]
#' @inheritParams view_regex
#' @keywords internal
wrap_result <- function(x, escape = FALSE, exact = FALSE) {
  if (is.null(x$idx[[1]])) return(if (escape) escape_html(x$text) else x$text)
  text <- x$text
  idx <- x$idx
  len_idx <- length(idx)
  inserts <- data.frame(
    i = 1:len_idx - 1,
    start = purrr::map_int(idx, ~ .[1]),
    end = purrr::map_int(idx, ~ .[2])
  ) %>%
    mutate(
      # unmatched groups have start/end of zero
      start = ifelse(.data$start == 0, NA, .data$start),
      end = ifelse(.data$end == 0, NA, .data$end),
      class = sprintf("group g%02d", .data$i),
      pad = 0
    )
  for (j in seq_len(nrow(inserts))) {
    if (inserts$i[j] == 0) next
    if (is.na(inserts$start[j]) || is.na(inserts$end[j])) next
    overlap <- filter(
      inserts[1:(j-1), ],
      .data$i != 0,
      .data$start <= !!inserts$start[j] & .data$end >= !!inserts$end[j])
    inserts[j, 'pad'] <- inserts$pad[j] + nrow(overlap)
  }
  inserts <- inserts %>%
    tidyr::gather(type, loc, start:end) %>%
    filter(!is.na(.data$loc)) %>%
    mutate(
      class = ifelse(.data$pad > 0, sprintf("%s pad%02d", .data$class, .data$pad), .data$class),
      insert = ifelse(.data$type == 'start', sprintf('<span class="%s">', .data$class), "</span>")
    ) %>%
    group_by(.data$loc, .data$type) %>%
    summarize(insert = paste(.data$insert, collapse = ''))

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
  if (exact) out <- escape_backslash(out)
  paste(out, collapse = '')
}

#' Wraps capture groups in regex pattern in span tags to colorize with CSS
#'
#' @inheritParams view_regex
#' @keywords internal
wrap_regex <- function(pattern, escape = TRUE, exact = TRUE) {
  stopifnot(length(pattern) == 1)
  if (escape) pattern <- escape_html(pattern)
  # 1. walk characters in pattern
  # 2. if current is open parens
  #    1. walk backwards, counting number of "\\" until first non-"\\" char
  #    2. If odd, then not an opening group
  #    3. Look forward, if followed by "?" then not a capturing group
  #    4. If capturing group then add opening "<span...>(" to out and
  #       add TRUE for valid capture group to parens stack
  #    5. If non-capturing group, add "(" to out and FALSE for non-valid to paren stack
  # 3. if close parens, add closing "</span>" to out
  out <- c()
  paren_stack <- c()
  group <- 0
  pattern_chars <- strsplit(pattern, "")[[1]]
  for (i in seq_along(pattern_chars)) {
    is_capture_group <- FALSE
    if (pattern_chars[i] == "(") {
      backslash_count <- 0
      if (i != 1) {
        j <- i-1
        while (pattern_chars[j] == "\\" && j > 0) {
          backslash_count <- backslash_count + 1
          j <- j - 1
        }
      }
      if (backslash_count %% 2 == 0) {
        if (i != length(pattern_chars) && pattern_chars[i + 1] != "?") {
          is_capture_group <- TRUE
        }
      }
      if (is_capture_group) {
        group <- group + 1
        paren_stack <- c(TRUE, paren_stack) #push
        out <- c(out, paste0('<span class="g', sprintf("%02d", group), '">('))
      } else {
        paren_stack <- c(FALSE, paren_stack) #push
        out <- c(out, "(")
      }
    } else if (pattern_chars[i] == ")") {
      closes_capture_group <- paren_stack[1]
      paren_stack <- paren_stack[-1] #pop
      if (closes_capture_group) {
        out <- c(out, ")</span>")
      } else {
        out <- c(out, ")")
      }
    } else {
      out <- c(out, pattern_chars[i])
    }
  }
  if (exact) out <- escape_backslash(out)
  paste(out, collapse = "")
}

#' View grouped regex results
#'
#' @param text Text to search
#' @param pattern Regex pattern to look for
#' @param render Render results to an HTML doc and open in RStudio viewer?
#' @param escape Escape HTML-related characters in `text`?
#' @param knitr Print into knitr doc? If `TRUE`, marks text as `asis_output` and
#'   sets `render = FALSE` and `escape = TRUE`.
#' @param exact Should the regex pattern be displayed as entered by the user
#'   into R console or source (default)? When `TRUE`, regex is displayed with
#'   the double `\\\\` required for escaping backslashes in R. When `FALSE`,
#'   regex is displayed as interpreted by the regex engine (i.e. double `\\\\`
#'   as a single `\\`).
#' @param ... Passed to [run_regex()]
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
  res <- purrr::map_chr(res, wrap_result, escape = escape, exact = exact)
  res <- purrr::map_chr(res, function(resi) {
    result_pad <- ""
    if (grepl("pad\\d{2}", resi)) {
      max_pad <- max(stringr::str_extract_all(resi, "pad\\d{2}")[[1]])
      max_pad_level <- as.integer(stringr::str_extract(max_pad, "\\d{2}"))
      if (max_pad_level - 3 > 0) {
        result_pad <- sprintf("pad%02d", max_pad_level - 3)
      }
    }
    paste("<p class='results", result_pad, "'>", resi, "</p>")
  })
  res <- paste(res, collapse = "")
  if (!nchar(pattern)) res <- paste("<p class='results'>", text, "</p>")
  if (knitr) return(knitr::asis_output(res))
  if (!render) return(res)
  head <- c(
    "---", "pagetitle: View Regex", "---",
    "<h5>Pattern</h5>",
    "<p><pre>", wrap_regex(pattern, escape, exact), "</pre></p>",
    "<h5>Matches</h5>"
  )
  res <- c(head, res)
  tmp <- tempfile(fileext = ".Rmd")
  cat(res, file = tmp, sep = "\n")
  tmp_html <- suppressWarnings(
    rmarkdown::render(
      tmp,
      output_format = rmarkdown::html_document(css = c(system.file("styles", 'skeleton.css', package='regexplain'),
                                                       system.file("styles", 'style.css', package='regexplain')),
                                               theme = NULL),
      quiet = TRUE
  ))
  rstudioapi::viewer(tmp_html)
}
