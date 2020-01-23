#' Methods to extract dispersion parameters
#'   in negative binomial settings
#'
#' @param object An object
#' @param ... Additional arguments
#' @return A value (chracter formatted)
#'   that can be passed to latex. Default
#'   returns an empty string
#'
#' @export
extract_alpha <- function(object, ...){
  UseMethod("extract_alpha")
}

#' @rdname extract_alpha
#' @export
extract_alpha.default <- function(object, ...){
  return("")
}

#' @rdname extract_alpha
#' @export
extract_alpha.zeroinfl <- function(object, ...){

  if (!object$dist == "negbin") return("")

  return(
    as.character(
      format(
        1/object$theta, digits = 3L, nsmall = 3L)
    )
  )
}


#' @rdname extract_alpha
#' @export
extract_alpha.glm <- function(object, ...){

  if (!inherits(object, "negbin")) return("")

  return(
    as.character(
      format(
        1/object$theta, digits = 3L, nsmall = 3L)
    )
  )
}
