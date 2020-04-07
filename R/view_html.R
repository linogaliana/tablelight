view_html <- function(x){
  tab <- paste(x, collapse = "")
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
}
