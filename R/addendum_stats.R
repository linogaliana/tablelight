#' An internal method to add some text to summary statistics part
#'
#' @param text Text in statistics part of the table
#' @param addendum Text that should be added
#' @inheritParams light_table
#' @export
addendum_stats <- function(text, addendum, type = "latex"){
  UseMethod("addendum_stats")
}

#' @rdname addendum_stats
#' @export
addendum_stats.default <- function(text, addendum, type = "latex"){

  if (type == "html"){
    text <- c(text, paste0("<tr><td style=\"text-align:left\">", addendum, "</td></tr>"))
  } else{
    text <- c(text, addendum)
  }

  return(text)
}


