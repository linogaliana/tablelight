
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


#' @rdname secoeff
#' @export
secoeff.light.zeroinfl <- function(object, ...){

  args <- list(...)

  if ('modeltype' %in% names(args)){

    if (args[['modeltype']] %in% c("count","outcome")){
      return(
        object$coefficients$count
      )
    } else{
      return(
        object$coefficients$zero
      )
    }

  } else{
    return(
      object$coefficients
    )
  }

}


#' @rdname secoeff
#' @export
secoeff.negbin <- function(object, ...){

  args <- list(...)


  coeffs <- data.frame(
    "Estimate" = object$coefficients,
    "Std. Error" = object$se,
    "z value" =  object$coefficients/object$se,
    "Pr(>|z|)" = pnorm(
      - abs(
        object$coefficients/object$se
      )
    )
  )

  colnames(coeffs) <- c("`Estimate`",
                        "`Std. Error`",
                        "`z value`",
                        "`Pr(>|z|)`")

  return(coeffs)

}

