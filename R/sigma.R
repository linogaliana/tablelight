#' Recover residual standard deviation from ordered discrete model
#'
#' Extract the estimated standard deviation of the errors,
#'   the “residual standard deviation” (misnomed also “residual standard error”).
#'
#' This function transforms the linear model for
#'   the standard deviation into the standard deviation
#'
#' @param object A \code{oglmx} model
#' @param newdata Dataframe that must be used
#' @return Residual estimated standard deviation in vector form. With an
#'  homoskedastic model, all values are equal
#' @importFrom stats sigma
#' @export

sigma.oglmx <- function(object, newdata = NULL, ...){

  if (!inherits(object, "oglmx")) stop("'object' is not a 'oglmx' object")

  # TERMS THAT ARE USED FOR VARIANCE COMPUTATION
  # ---------------------------------------------------

  if (is.null(newdata)){
    Z <- object$modelframes$Z
  } else{
    Z <- variance_model(object, newdata)
  }

  # delta
  # ----------------------------------------------------

  delta <- object$allparams$delta


  # Expression for variance computation
  # ---------------------------------------------------

  if (delta != 0){
    z <- Z %*% delta
  } else{
    z <- 0
  }

  # Return sigma = g(delta*z)
  # --------------------------------

  sigma <- eval(object$sdmodel)

  return(sigma)
}


#' Construct the dataframe used for
#'  variance estimation
#'
#' @param object A \link[oglmx]{oglmx} object
#' @param newdata Dataframe that must be used. An
#'   error will be thrown away if the model matrix
#'   for variance cannot be constructed
#' @param ... Additional arguments
#' @importFrom stats model.matrix

variance_model <- function(object, newdata = NULL,
                           ...){

  if (!inherits(object, "oglmx")) stop("'object' should be a 'oglmx' object")

  # WHEN newdata is NULL, we use the initial object
  if (is.null(newdata)) newdata <- object$modelframes$Z


  if (!is.null(object$formula$sdeq)) {
    if (object$constantSD) {
      Z <- newdata
      Zint <- match("(Intercept)", colnames(Z), nomatch = 0L)
      if (Zint > 0L) {
        Z <- Z[, -Zint, drop = FALSE]
      }
    } else{
      Z <- newdata
    }
    Z <- stats::model.matrix(object$formula$sdeq, data.frame(Z))
  }
  else {
    Z <- matrix(rep(1, nrow(newdata)), ncol = 1)
  }


  return(Z)
}

