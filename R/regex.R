#' Extract matched groups from regexp
#'
#' @param text Text to search
#' @param pattern regexp
#' @param global If `TRUE`, enables global pattern matching
#' @inheritParams base::regexec
regex <- function(
  text,
  pattern,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  global = TRUE
) {
  # Use regex to get matches by group, gives start index and length
  m <- regexec(pattern, text, ignore.case, perl, fixed, useBytes)
  m <- purrr::map2(text, m, ~ list(text = .x, idx = expand_matches(.y)))

  attr(m, "global") <- global
  if (!global) return(m)

  mmi <- max_match_index(m)
  if (any(!is.na(mmi))) {
    subtext <-  purrr::map_chr(m, "text") %>% purrr::map2_chr(mmi, substring)
    subtext[is.na(subtext)] <- ""
    m2 <- regex(subtext, pattern, ignore.case, perl, fixed, useBytes)
    for (i in seq_along(m2)) {
      if (is.null(m2[[i]]$idx[[1]])) next
      m2[[i]]$idx[, c(1, 2)] <- m2[[i]]$idx[, c(1, 2)] + mmi[i] - 1L
      m2[[i]]$idx$pass <- m2[[i]]$idx$pass + 1L
      m[[i]]$idx <- rbind(m[[i]]$idx, m2[[i]]$idx)
    }
  }
  m
}

expand_matches <- function(m) {
  if (m[1] == -1) return(list(NULL))
  m_length <- attr(m, "match.length")
  x <- purrr::map2(m, m_length, ~ c(.x, .x + .y))
  x <- as.data.frame(do.call(rbind, x))
  names(x) <- c("start", "end")
  x$start <- ifelse(x$start == 0L, NA_integer_, x$start)
  x$end   <- ifelse(x$end == 0L, NA_integer_, x$end)
  x$group <- 1:nrow(x) - 1L
  x$pass  <- 1L
  x
}

max_match_index <- function(m) {
  max_na <- function(x) if (is.null(x)) NA else max(x, na.rm = TRUE)
  max_int <- function(x) as.integer(max(x))

  purrr::map(m, "idx") %>%
    purrr::modify_depth(1, ~c(start = max_na(.x$start), end = max_na(.x$end))) %>%
    purrr::map_int(max_int)
}


#' Wrap matches in HTML span tags to colorize via CSS
#'
#' @param x Individual list item in list returned by [regex()]
#' @inheritParams view_regex
#' @keywords internal
wrap_result <- function(x, escape = FALSE, exact = FALSE) {
  if (is.null(x$idx[[1]])) return(if (escape) escape_html(x$text) else x$text)
  text <- x$text

  inserts <- x$idx
  inserts$class <- sprintf("group g%02d", inserts$group)
  inserts$pad <- 0L
  names(inserts)[which(names(inserts) == "group")] <- "i"

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
    dplyr::arrange(loc, class, dplyr::desc(type)) %>%
    mutate(
      class = ifelse(.data$pad > 0, sprintf("%s pad%02d", .data$class, .data$pad), .data$class),
      insert = ifelse(.data$type == 'start', sprintf('<span class="%s">', .data$class), "</span>")
    )

  inserts <- if (max(inserts$pass) == 1) {
    collapse_span_inserts(inserts)
  } else {
    inserts %>%
      tidyr::nest(spans = -.data$pass) %>%
      mutate(spans = purrr::map(.data$spans, collapse_span_inserts)) %>%
      tidyr::unnest(.data$spans) %>%
      group_by(.data$loc, .data$type) %>%
      summarize(insert = paste(.data$insert, collapse = "")) %>%
      dplyr::ungroup()
  }

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

collapse_span_inserts <- function(inserts) {
  inserts_g0 <- filter(inserts, class == "group g00")
  inserts_other <- filter(inserts, class != "group g00")
  dplyr::bind_rows(
    filter(inserts_g0, type == "start"),
    inserts_other,
    filter(inserts_g0, type == "end")
  ) %>%
    mutate(type = sprintf("%05d%s", dplyr::row_number(), type)) %>%
    group_by(.data$loc, .data$type) %>%
    summarize(insert = paste(.data$insert, collapse = '')) %>%
    dplyr::ungroup() %>%
    mutate(type = sub("^\\d{5}", "", type))
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
#' View the result of the regular expression when applied to the given text.
#' The default behavior renders the result as HTML and opens the file in
#' the RStudio viewer pane. If `render` is `FALSE`, the HTML itself is returned.
#' If the output is destined for a [knitr] document, set `knitr` to `TRUE`.
#'
#' @examples
#' view_regex("example", "amp", render=FALSE)
#'
#' @param text Text to search
#' @param pattern Regex pattern to look for
#' @param render Render results to an HTML doc and open in RStudio viewer?
#' @param escape Escape HTML-related characters in `text`?
#' @param exact Should the regex pattern be displayed as entered by the user
#'   into R console or source (default)? When `TRUE`, regex is displayed with
#'   the double `\\\\` required for escaping backslashes in R. When `FALSE`,
#'   regex is displayed as interpreted by the regex engine (i.e. double `\\\\`
#'   as a single `\\`).
#' @param result_only Should only the result be displayed? If `FALSE`, then
#'   the colorized regular expression is also displayed in the output.
#' @inheritDotParams base::regexec ignore.case perl fixed useBytes
#' @export
view_regex <- function(
  text,
  pattern,
  ...,
  render = TRUE,
  escape = render,
  exact = escape,
  result_only = FALSE
) {
  knitr <- isTRUE(getOption('knitr.in.progress'))
  if (knitr) {
    render <- FALSE
    escape <- TRUE
  }
  regex_opts <- deprecate_knitr_option(...)
  regex_opts$text <- text
  regex_opts$pattern <- pattern
  res <- do.call(regex, regex_opts)
  res <- purrr::map_chr(res, wrap_result, escape = escape, exact = exact)
  res <- purrr::map_chr(res, function(resi) {
    result_pad <- ""
    if (grepl("pad\\d{2}", resi)) {
      max_pad <- max(stringi::stri_extract_all_regex(resi, "pad\\d{2}")[[1]])
      max_pad_level <- as.integer(stringi::stri_extract_all_regex(max_pad, "\\d{2}"))
      if (max_pad_level - 3 > 0) {
        result_pad <- sprintf("pad%02d", max_pad_level - 3)
      }
    }
    paste('<p class="results', result_pad, '">', resi, "</p>")
  })
  res <- paste(res, collapse = "")
  if (!nchar(pattern)) res <- paste("<p class='results'>", text, "</p>")
  if (knitr) {
    # embed css
    group_css <- htmltools::htmlDependency(
      name = "regexplain-groups", version = packageVersion("regexplain"),
      src = system.file("styles", package = "regexplain"),
      stylesheet = "groups.css")
    res <- htmltools::attachDependencies(htmltools::HTML(res), group_css)
    return(res)
  }
  if (!render) return(res)
  head <- if (!result_only) c(
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
      output_format = rmarkdown::html_document(
        css = c(system.file("styles", 'skeleton.css', package='regexplain'),
                system.file("styles", 'view_regex.css', package='regexplain'),
                system.file("styles", 'groups.css', package='regexplain')),
        theme = NULL,
        md_extensions = "-autolink_bare_uris"),
      quiet = TRUE
  ))
  rstudioapi::viewer(tmp_html)
}

deprecate_knitr_option <- function(...) {
  regex_opts <- list(...)
  if ("knitr" %in% names(regex_opts)) {
    warning("The `knitr` parameter of `view_regex()` has been removed. Running `view_regex()` in R Markdown is automatically detected.")
  }
  regex_opts[setdiff(names(regex_opts), "knitr")]
}
