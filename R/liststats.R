#' A small method to extract statistics names relevent
#'  to econometric models
#'
#' @param object An object
#' @param ... Additional arguments
#' @return A character vector with names
#'  that can be passed to latex
#' @export
liststats <- function(object, ...){
  UseMethod("liststats")
}



#' @rdname liststats
liststats.light.zeroinfl <- function(object, ...){

  if (!inherits(object, "light.zeroinfl")) stop("Object is not light.zeroinfl")

  llk <- object$loglik
  bic <- BIC(object)
  link_count <- if (object$dist == "negbin") "Negative Binomial" else "Poisson"
  link_selection <- Hmisc::capitalize(object$link)


  df <- data.frame(
    stat = c(
      "Count distribution",
      "Selection distribution",
      "Observations",
      "Log likelihood",
      "Log likelihood (by obs.)",
      "Bayesian information criterion"),
    order = seq_len(6L),
    val = as.character(
      c(link_count,
        link_selection,
        format(object$n, digits = 0,  big.mark=",", scientific = FALSE),
        format(llk, digits = 0, big.mark=",", scientific = FALSE),
        format(llk/object$n, digits = 3L, nsmall = 3L, big.mark=",", scientific = FALSE),
        format(bic, digits = 0L, big.mark=",", scientific = FALSE)
      )
    )
  )

  df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                         order = 0,
                         val = as.character(
                           format(1/object$theta, digits = 3L, nsmall = 3L))
  ), df)

  return(df)
}


#' @rdname liststats
liststats.light.glm <- function(object, ...){

  if (!inherits(object, "light.glm")) stop("Object is not light.glm")

  llk <- as.numeric(logLik(object))
  bic <- BIC(object)
  link_count <- if (object$call[1] == "glm.nb()") "Negative Binomial" else "Poisson"
  link_selection <- ""

  df <- data.frame(
    stat = c(
      "Count distribution",
      "Selection distribution",
      "Observations",
      "Log likelihood",
      "Log likelihood (by obs.)",
      "Bayesian information criterion"),
    order = seq_len(6L),
    val = as.character(
      c(link_count,
        link_selection,
        format(object$n, digits = 0,  big.mark=",", scientific = FALSE),
        format(llk, digits = 0, big.mark=",", scientific = FALSE),
        format(llk/object$n, digits = 3L, nsmall = 3L, big.mark=",", scientific = FALSE),
        format(bic, digits = 0L, big.mark=",", scientific = FALSE)
      )
    )
  )

  if (!is.null(object$theta)){
    alpha <- as.character(
      format(1/object$theta, digits = 3L, nsmall = 3L)
    )
  } else{
    alpha <- ""
  }

  df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                         order = 0,
                         val = alpha), df)

  return(df)
}
