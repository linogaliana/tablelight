
#' Strip regression objects to reduce memory needs
#'
#' Remove elements that are more related
#'  to prediction than model to reduce
#'  memory needs to store a glm output
#'
#' @title strip: strip regressions
#' @param object A regression object or summary.
#'  Accepted classes are \link[stats]{glm} or
#'  \link[pscl]{zeroinfl}
#' @param ... Additional arguments that should be
#'  considered
#'
#' @return Initial object stripped from
#'  heavy elements. Initial class is returned
#'  with a new one indicating the object has
#'  been stripped.
#'
#' @rdname strip
#' @export
strip <- function(object, ...){
  UseMethod("strip")
}

#' @rdname strip
#' @export
strip.glm <- function(object, ...) {

  if (!inherits(object, "glm")) stop("object' should be a glm object")
  if (inherits(object, "negbin")) return(
    strip.negbin(object, ...)
  )


  summary_object <- summary(object)

  object$coefficients <- secoeff(summary_object)
  object$n <- stats::nobs(object)
  llk <- stats::logLik(object)
  object$loglikelihood <- as.numeric(llk)
  object$bic <- as.numeric(BIC(llk))
  object$link_count <- Hmisc::capitalize(as.character(summary_object$family$family))
  object$link_selection <- ""

  object$y = c()
  object$model = c()

  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  object$offset = c()
  object$qr$qr = c()
  object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()


  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()


  class(object) <- c(class(object), "light.glm")

  return(object)
}


#' @rdname strip
#' @export
strip.oglmx <- function(object, ...) {

  if (!inherits(object, "oglmx")) stop("object' should be a glm object")

  summary_object <- summary(object)

  object$coefficients <- secoeff(summary_object)
  object$n <- stats::nobs(object)
  llk <- stats::logLik(object)
  attr(llk, "nobs") <- object$n
  object$loglikelihood <- as.numeric(llk)
  object$bic <- as.numeric(BIC.logLik.oglmx(llk))
  object$link_count <- ""
  object$link_selection <- ""

  object$y = c()
  object$model = c()

  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  object$offset = c()
  object$qr$qr = c()
  object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()


  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()


  class(object) <- c(class(object), "light.oglmx")

  return(object)
}


#' @rdname strip
#' @export
strip.negbin <- function(object, ...) {

  summary_object <- summary(object)

  object$coefficients <- secoeff(summary_object)
  object$n <- stats::nobs(object)
  llk <- stats::logLik(object)
  object$loglikelihood <- as.numeric(llk)
  object$bic <- as.numeric(BIC(llk))
  object$link_count <- gsub("\\s*\\([^\\)]+\\)","",
                            as.character(summary_object$family$family))
  object$link_selection <-  ""


  object$y = c()
  object$model = c()

  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  object$offset = c()
  object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()


  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()


  class(object) <- c(class(object), paste0("light.", class(object)))

  return(object)
}



#' @rdname strip
#' @export
strip.lm <- function(object, ...) {

  if (!inherits(object, "lm")) stop("object' should be a lm object")

  summary_object <- summary(object)

  object$coefficients <- secoeff(summary_object)
  object$n <- stats::nobs(object)
  llk <- stats::logLik(object)
  object$loglikelihood <- as.numeric(llk)
  object$bic <- as.numeric(BIC(llk))
  object$rsq  <- summary_object$r.squared
  object$adjrsq  <- summary_object$adj.r.squared
  object$link_count <- "Gaussian"
  object$link_selection <- ""

  object$y = c()
  object$model = c()

  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  object$qr$qr = c()
  object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()


  object$family$variance = c()
  object$family$dev.resids = c()
  object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()


  class(object) <- c(class(object), "light.lm")

  return(object)
}


#' @rdname strip
#' @export
strip.zeroinfl <- function(object, ...) {

  if (!inherits(object, "zeroinfl")) stop("object' should be a zeroinfl object")


  summary_object <- summary(object)

  object$coefficients <- secoeff(summary_object)

  object$n <- stats::nobs(object)
  llk <- stats::logLik(object)
  object$loglikelihood <- as.numeric(llk)
  object$bic <- as.numeric(BIC(llk))
  object$r.squared  <- summary_object$r.squared
  object$link_count <- Hmisc::capitalize(object$dist)
  object$link_selection <- Hmisc::capitalize(object$link)



  object$y = c()
  object$model = c()
  object$offset = c()

  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  object$qr = c()
  object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()


  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()



  class(object) <- c(class(object), "light.zeroinfl")

  return(object)
}


#' @rdname strip
#' @export
strip.summary.glm <- function(object, ...){

  if (!inherits(object, "summary.glm")) stop("object' should be the summary of a glm object")

  object$deviance.resid <- NULL

  class(object) <- c(class(object), "light.summary.glm")

  return(object)
}


#' @rdname strip
#' @export
strip.summary.lm <- function(object, ...){

  if (!inherits(object, "summary.lm")) stop("object' should be the summary of a lm object")

  object$deviance.resid <- NULL

  class(object) <- c(class(object), "light.summary.lm")

  return(object)
}



#' @rdname strip
#' @export
strip.summary.zeroinfl <- function(object, ...){

  if (!inherits(object, "summary.zeroinfl")) stop("object' should be the summary of a zeroinfl object")

  object$weights <- NULL
  object$residuals <- NULL
  object$fitted.values <- NULL
  #object$model <- NULL
  class(object) <- c(class(object), "light.summary.zeroinfl")

  return(object)
}

