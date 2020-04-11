#' Espace special characters in regex
#'
#' @param str A character vector
#' @return Initial vector with special
#'   characters escaped

str_to_regex <- function(str){

  special_characters <- c("^", "$", ".",
  "|", "?", "*",
  "+", "(", ")",
  "[", "]" ,"{", "}")

  return(
    mgsub(special_characters, paste0("\\", special_characters),
      str)
  )
}
