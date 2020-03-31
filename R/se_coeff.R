
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

  # FOR SUMMARY.LM OBJECTS
  if (inherits(object, "summary.lm")) return(secoeff.summary.lm(object))

  # ENSURE NEGBIN IS WELL HANDLED
  if (inherits(object, "negbin")) return(secoeff.negbin(object))

  # ENSURE lm IS WELL HANDED
  if (inherits(object, "light.lm") &&
      (as.character(object$call[1]) == "lm")) return(object$coefficients)


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

  if ('se' %in% names(object)){
    object2 <- object
    se <- object$se
  } else{
    object2 <- summary(object)
    se <- object2$coefficients[,"Std. Error"]
  }


  coeffs <- data.frame(
    "Estimate" = object2$coefficients,
    "Std. Error" = se,
    "z value" =  abs(object2$coefficients/se),
    "Pr(>|z|)" = pnorm(
      - abs(
        object2$coefficients/se
      )
    )
  )

  colnames(coeffs) <- c("Estimate",
                        "Std. Error",
                        "z value",
                        "Pr(>|z|)")

  return(coeffs)

}


#' @rdname secoeff
#' @export
secoeff.summary.lm  <- function(object, ...){
  return(object$coefficients)
}

#' @rdname secoeff
#' @export
secoeff.summary.glm  <- function(object, ...){
  return(object$coefficients)
}


#' @rdname secoeff
#' @export
secoeff.summary.oglmx  <- function(object, ...){
  return(object$coefficients)
}


#' @rdname secoeff
#' @export
secoeff.summary.zeroinfl  <- function(object, ...){
  return(object$coefficients)
}
