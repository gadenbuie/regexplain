# highr:::escape_html
# by Yihui Xie, GPL license
# https://github.com/yihui/highr/blob/4f54a5b8960d6246daadacea1020ebcdc458ce50/R/utils.R#L54-L61
escape_html = function(x) {
  x = gsub('&', '&amp;', x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = gsub('"', '&quot;', x)
  x
}

escape_backslash <- function(x) {
  gsub("\\\\", "\\\\\\\\", x)
}

# avoid CRAN note for tidyr::gather in wrap_results
utils::globalVariables(c("end", "loc", "start", "type"))
