#' @export
secoeff <- function(object, ...){
  UseMethod("secoeff")
}

#' @rdname secoeff
#' @export
secoeff.default  <- function(object, tol = 1e-20, ...){

  return(
    summary(object)$coefficients
  )

}

#' @rdname secoeff
#' @export
secoeff.oglmx <- function(object, tol = 1e-20, ...){

  return(summary(object)$estimate)
  summary(oglm)$estimate

}
