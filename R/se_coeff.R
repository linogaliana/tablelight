
#' Extract summary table for coefficients
#'  estimates
#'
#' @param object A regression object
#' @param ... Additional parameters
#'
#' @return Returns coefficient values,
#'  standard errors, t-stat (or z-stat)
#'  and pvalues


#' @export
secoeff <- function(object, ...){
  UseMethod("secoeff")
}

#' @rdname secoeff
#' @export
secoeff.default  <- function(object, ...){

  return(
    summary(object, ...)$coefficients
  )

}

#' @rdname secoeff
#' @export
secoeff.oglmx <- function(object, ...){

  return(summary(object, ...)$estimate)

}
