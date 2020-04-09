#' Report HTML table in Rstudio viewer
#'
#' @param x HTML code
#' @return Returns nothing, HTML reported in
#'   Rstudio viewer
#' @importFrom rstudioapi viewer
#' @export
view_html <- function(x){
  tab <- paste(x, collapse = "")
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
}
