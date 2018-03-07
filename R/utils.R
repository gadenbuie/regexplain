escape_html <- function(x) {
  x = gsub("&", "&amp;", x)
  x = gsub("<", "&lt;", x)
  x = gsub(">", "&gt;", x)
  x = gsub("\"", "&quot;", x)
  x
}
