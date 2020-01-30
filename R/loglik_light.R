#' Extract loglikelihood from a stripped object
#'
#' Extends \link[stats]{logLik} to objects of
#'   class \code{light}* (i.e. regression objects
#'   after applying \link{strip} method)
#'
#' @details Implementation, at this stage,
#'  only returns the loglikelihood value.
#'  Additional returns might be included
#'  in future releases
#'
#' @inheritParams stats::logLik
#'
#' @return Returs loglikelihood
#'
#' @seealso \link[stats]{logLik}
#' @import stats

#' @export
logLik.light.zeroinfl <- function(object, ...){
  return(
    object$loglik
  )
}

#' @rdname logLik.light.zeroinfl
#' @export
logLik.light.glm <- function(object, ...){
  return(
    object$loglik
  )
}
