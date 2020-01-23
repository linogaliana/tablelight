
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
listcoeff.light.glm <- function(object, ...){

  if (!inherits(object, "light.glm")) stop("Object is not light.glm")

  allcoeffs <- object$coefficients

  allcoeffs <- rownames(allcoeffs)
  allcoeffs <- unique(allcoeffs)

  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}
