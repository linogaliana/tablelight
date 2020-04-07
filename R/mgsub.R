#' Multiple gsub
#'
#' \code{multigsub} - Borrowed from \code{qdap} library.
#' A wrapper for \code{\link[base]{gsub}} that takes a vector
#' of search terms and a vector or single value of replacements.
#'
#' @param pattern Character string to be matched in the given character vector.
#' @param replacement Character string equal in length to pattern or of length
#' one which are  a replacement for matched pattern.
#' @param text.var The text variable.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param \dots Additional arguments passed to \code{\link[base]{gsub}}.
#' @rdname multigsub
#' @return \code{multigsub} - Returns a vector with the pattern replaced.
#' @seealso \code{\link[base]{gsub}}
#' @export
#' @examples
#' \dontrun{
#' ## ======================
#' ##    `mgsub` Function
#' ## ======================
#'
#' mgsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub("[[:punct:]]", "PUNC", DATA$state, fixed = FALSE)
#'
#' }


mgsub <- function (pattern, replacement, text.var, fixed = TRUE, ...){

  if (fixed) {
    ord <- rev(order(nchar(pattern)))
    pattern <- pattern[ord]
    if (length(replacement) != 1)
      replacement <- replacement[ord]
  }
  if (length(replacement) == 1)
    replacement <- rep(replacement, length(pattern))
  for (i in seq_along(pattern)) {
    text.var <- gsub(pattern[i], replacement[i], text.var,
                     fixed = fixed, ...)
  }
  text.var
}
