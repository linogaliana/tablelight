#' Bayesian information criterion implementation for stripped objects
#'
#' @param object A fitted model object for which there exists a logLik
#'  method to extract the corresponding log-likelihood, or
#'  an object inheriting from class logLik.
#' @param ... Optionally more fitted model objects
#'
#'
#' Formula \eqn{-2l + k\log(npar)} where *npar*
#'  represents the number of parameters in the fitted model
#'  and \eqn{l} represents model log-likelihood
#'
#' @import stats
#' @export
#'
BIC.light.zeroinfl <- function(object, ...){
  k <- nrow(object$coefficients$count) + nrow(object$coefficients$zero)
  return(
    -2*object$loglik + k*log(object$n)
  )
}


#' @export
BIC.light.glm <- function(object, ...){

  llk <- as.numeric(logLik(object))
  k <- nrow(object$coefficients)

  if (!is.null(object$theta)) k <- k + 1

  return(
    -2*llk + k*log(object$n)
  )

}


#' @export
BIC.logLik.oglmx <- function(object, ...){

  return(
    -2 * as.numeric(object) + attr(object, "df") * log(attr(object, "nobs"))
  )

}


