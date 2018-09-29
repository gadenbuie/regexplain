#' RegExplain gadget
#'
#' The function behind the RegExplain Selection and RegExplain File
#' addins. Opens the RegExplain gadget interface in an RStudio viewer
#' pane.
#'
#' @examples
#' \dontrun{
#' regexplain_gadget(text = month.name, pattern = "(Ma|Ju)|(er)")
#' regexplain_web(text = month.name, pattern = "(Ma|Ju)|(er)")
#' regexplain_file()
#' }
#'
#' @import miniUI
#' @import shiny
#' @param text Text to explore in gadget (editable using interface)
#' @param pattern Regular Expression to edit or visualize using RegExplain
#' @param start_page Open gadget to this tab, one of `"Text"`, `"RegEx"`,
#'   `"Output"`, or `"Help"`
#' @export
regexplain_gadget <- function(
  text = NULL,
  pattern = NULL,
  start_page = if (is.null(text)) "Text" else "RegEx"
) {
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))

  viewer <- shiny::paneViewer(minHeight = 800)
  runGadget(
    regexplain_gadget_ui(text, pattern, start_page),
    regexplain_gadget_server(check_version()),
    viewer = viewer)
}

#' @describeIn regexplain_gadget Launches the RegExplain gadget in a browser or an
#'   RStduio viewer pane.
#' @inheritDotParams shiny::shinyApp
#' @export
regexplain_web <- function(text = NULL, pattern = NULL, start_page = "Text", ...) {
  stopifnot(requireNamespace("miniUI"), requireNamespace("shiny"))

  shinyApp(
    regexplain_gadget_ui(text, pattern, start_page),
    regexplain_gadget_server(check_version()), ...)
}

# ---- Gadget Helper Functions and Variables ----

sanitize_text_input <- function(x) {
  if (is.null(x) || !nchar(x)) return(x)
  rx_unicode <- "\\\\u[0-9a-f]{4,8}"
  rx_hex <- "\\\\x[0-9a-f]{2}|\\\\x\\{[0-9a-f]{1,6}\\}"
  rx_octal <- "\\\\[0][0-7]{1,3}"
  rx_escape <- paste(rx_unicode, rx_hex, rx_octal, sep = "|")
  if (grepl(rx_escape, x, ignore.case = TRUE)) {
    try({
      y <- stringi::stri_unescape_unicode(x)
    }, silent = TRUE)
    if (!is.na(y)) x <- y
  }
  # x <- gsub("\u201C|\u201D", '"', x)
  # x <- gsub("\u2018|\u2019", "'", x)
  x
}

toHTML <- function(...) {
  x <- paste(..., collapse = "")
  x <- gsub("\n", "\\\\n", x)
  x <- gsub("\t", "\\\\t", x)
  x <- gsub("\r", "\\\\r", x)
  HTML(x)
}

regexFn_choices <- list(
  "Choose a function" = "",
  base = c(
    "grep",
    "grepl",
    "sub",  #<<
    "gsub", #<<
    "regexpr",
    "gregexpr",
    "regexec"
  ),
  stringr = c(
    "str_detect",
    "str_locate",
    "str_locate_all",
    "str_extract",
    "str_extract_all",
    "str_match",
    "str_match_all",
    "str_replace",     #<<
    "str_replace_all", #<<
    "str_split"
  ),
  "rematch2" = c(
    "re_match",
    "re_match_all",
    "re_exec",
    "re_exec_all"
  )
)

regexFn_substitute <- c(
  paste0(c("", "g"), "sub"),
  paste0("str_replace", c("", "_all"))
)

get_pkg_namespace <- function(fn) {
  x <- names(purrr::keep(regexFn_choices, ~ (fn %in% .)))
  if (length(x) > 1) warning(fn, " matches multiple functions in regexFn_choices, please review.")
  x
}

#' Check if an updated version is available
#'
#' I included this because it can be difficult to tell if your RStudio Addins
#' are up to date. I may add new features that you want but you won't hear about
#' the updates. This function checks if an update is available, using GitHub
#' tags. If an update is available, a modal dialog is shown when you start
#' the regexplain gadget. This only happens once per R session, though, so feel
#' free to ignore the message.
#'
#' @param gh_user GitHub user account
#' @param gh_repo GitHub repo name
#' @param this_version The currently installed version of the package
#' @keywords internal
check_version <- function(
  gh_user = "gadenbuie",
  gh_repo = "regexplain",
  this_version = packageVersion('regexplain')
) {
  ok_to_check <- getOption("regexplain.no.check.version", TRUE)
  if (!isTRUE(ok_to_check)) return(NULL)
  if (!requireNamespace('jsonlite', quietly = TRUE)) return(NULL)
  get_json <- purrr::possibly(jsonlite::fromJSON, NULL)
  gh_tags <- get_json(
    paste0("https://api.github.com/repos/", gh_user, "/", gh_repo, "/git/refs/tags"),
    simplifyDataFrame = TRUE
  )
  if (!is.null(gh_tags)) {
    gh_tags$tag <- sub("refs/tags/", "", gh_tags$ref, fixed = TRUE)
    gh_tags$version <- sub("^v\\.?", "", gh_tags$tag)
  }
  if (!is.null(gh_tags) && any(gh_tags$version > this_version)) {
    max_version <- max(gh_tags$version)
    max_tag <- gh_tags$tag[gh_tags$version == max_version]
    options(regexplain.no.check.version = FALSE)
    return(
      list(
        version = max_version,
        link = paste("https://github.com", gh_user, gh_repo, "releases/tag", max_tag, sep = "/")
      )
    )
  } else return(NULL)
}

#' Loads Regex Pattern Library
#'
#' Patterns sourced from [Regex Hub](https://projects.lukehaas.me/regexhub)
#' are available at <https://github.com/lukehaas/RegexHub> and are copyright
#' Luke Haas licensed under the MIT license available at
#' <https://github.com/lukehaas/RegexHub/commit/3ab87b5a4fd2817b42e2e45dcf040d4f0164ea37>.
#' Patterns source from [qdapRegex](https://github.com/trinker/qdapRegex) are
#' copyright Tyler Rinker and Jason Gray, licensed under the GPL-2 license.
#'
#' @keywords internal
get_regex_library <- function() {
  if (!requireNamespace("jsonlite")) {
    warning("Please install the `jsonlite` package to use template features")
    return(NULL)
  }
  f_patterns <- system.file("extdata", "patterns.json", package = "regexplain")
  if (!file.exists(f_patterns)) return(NULL)
  patterns <- jsonlite::fromJSON(
    f_patterns,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
  patterns <- purrr::keep(patterns, ~ .$name != "")
  patterns[order(purrr::map_chr(patterns, 'name'))]
}
