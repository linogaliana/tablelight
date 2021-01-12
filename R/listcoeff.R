
#' A small method to extract coefficient names from
#'  econometric models
#'
#' @param object An object
#' @param ... Additional arguments
#' @return A character vector with names
#'  that can be passed to latex
#' @export
listcoeff <- function(object, ...){
  UseMethod("listcoeff")
}

#' @rdname listcoeff
#' @export
listcoeff.light.zeroinfl <- function(object, ...){

  if (!inherits(object, "light.zeroinfl")) stop("Object is not light.zeroinfl")

  allcoeffs <- object$coefficients

  allcoeffs <- c(rownames(allcoeffs$count),
                 rownames(allcoeffs$zero))
  allcoeffs <- unique(allcoeffs)

  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}


#' @rdname listcoeff
#' @export
listcoeff.mindist <- function(object, ...){
  
  if (!inherits(object, "mindist")) stop("Object is not mindist")
  
  allcoeffs <- object$estimates$theta_hat
  
  allcoeffs <- names(allcoeffs)
  allcoeffs <- unique(allcoeffs)
  
  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)
  
  return(allcoeffs)
}

#' @rdname listcoeff
#' @export
listcoeff.zeroinfl <- function(object, ...){

  if (inherits(object, "light.zeroinfl")) return(
    listcoeff.light.zeroinfl(object, ...)
  )


  allcoeffs <- object$coefficients

  allcoeffs <- c(names(allcoeffs$count),
                 names(allcoeffs$zero))
  allcoeffs <- unique(allcoeffs)

  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}


#' @rdname listcoeff
#' @export
listcoeff.light.glm <- function(object, ...){

  if (!inherits(object, "light.glm")) stop("Object is not light.glm")

  allcoeffs <- object$coefficients

  allcoeffs <- rownames(allcoeffs)
  allcoeffs <- unique(allcoeffs)

  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}


#' @rdname listcoeff
#' @export
listcoeff.default <- function(object, ...){

  allcoeffs <- object$coefficients

  allcoeffs <- names(allcoeffs)

  allcoeffs <- unique(allcoeffs)

  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}

#' @rdname listcoeff
#' @export
listcoeff.nnet <- function(object, ...){

  allcoeffs <- object$vcoefnames

  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}



#' @rdname listcoeff
#' @export
listcoeff.light.lm <- function(object, ...){

  allcoeffs <- object$coefficients

  allcoeffs <- rownames(allcoeffs)

  allcoeffs <- unique(allcoeffs)

  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}
