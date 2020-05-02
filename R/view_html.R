#' Report HTML table in Rstudio viewer or in a Rmarkdown document
#'
#' @param x HTML code
#' @return Returns nothing, HTML reported in
#'   Rstudio viewer (`view_html`) or in
#'   the Rmd document (`print_html`)
#' @importFrom rstudioapi viewer
#' @export
view_html <- function(x){
  tab <- paste(x, collapse = "")
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
}

#' @rdname view_html
#' @export
#' @importFrom htmltools includeHTML
print_html <- function(x){
  tab <- paste(x, collapse = "")
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  htmltools::includeHTML(tf)
}

