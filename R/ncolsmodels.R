#' A method to infer the number of columns for the regression table
#' @param object Regression object or list of regression objects
#' @return Number of columns necessary that will be used
#'  for the regression table
#' @export
ncolsmodels <- function(object){
  UseMethod("ncolsmodels")
}

#' @rdname ncolsmodels
#' @export
ncolsmodels.default <- function(object){
  return(
    1L
  )
}

#' @rdname ncolsmodels
#' @export
ncolsmodels.nnet <- function(object){
  return(
    length(object$lab[-1])
  )
}

#' @rdname ncolsmodels
#' @export
ncolsmodels.list <- function(object){

  if (inherits(object[[1]], "nnet")){
    return(
      sum(
        sapply(object, function(m) length(m$lab[-1]))
      )
    )
  } else{
    return(length(object))
  }
}
