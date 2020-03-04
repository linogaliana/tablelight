#' A small method to extract statistics names relevent
#'  to econometric models
#'
#' @param object An object
#' @param ... Additional arguments. See `details`
#' @return A character vector with names
#'  that can be passed to latex
#'
#' @details The following arguments add statistics
#' \itemize{
#'   \item `add_link = TRUE`: Add two rows informing
#'     on the count and selection distributions
#'     used in `object` (possibly empty)
#'   \item `add_alpha = TRUE`: Add a row on the
#'     dispersion parameter for negative binomial
#'     models (possibly empty)
#'   \item `add_sigma = TRUE`: Add a row on the
#'     estimated standard deviation (see \link[stats]{sigma})
#' }
#'
#' @importFrom stats logLik
#' @importFrom stats BIC
#' @export
liststats <- function(object, ...){
  UseMethod("liststats")
}



#' @rdname liststats
#' @importFrom Hmisc capitalize
#' @export
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

  alpha_value <- ""
  if (object$dist == "negbin") alpha_value <- as.character(
    format(1/object$theta, digits = 3L, nsmall = 3L))


  df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                         order = 0,
                         val = alpha_value), df
  )

  return(df)
}


#' @rdname liststats
#' @export
liststats.light.glm <- function(object, ...){

  if (!inherits(object, "light.glm")) stop("Object is not light.glm")

  llk <- as.numeric(logLik(object))
  bic <- BIC(object)
  if (as.character(object$call[1]) %in% c("glm.nb",
                            "MASS::glm.nb",
                            "MASS:::glm.nb",
                            "fastglm.nb",
                            "gravity::fastglm.nb",
                            "gravity:::fastglm.nb")){
    link_count <- "Negative Binomial"
  } else{
    link_count <- "Poisson"
  }

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


#' @rdname liststats
#' @export
liststats.default <- function(object, ...){

  args <- list(...)

  if (as.character(object$call[1]) == "lm" && inherits(object, "light.lm"))
    return(liststats.light.lm(object, ...))


  llk <- as.numeric(logLik(object))
  bic <- BIC(object)

  if ('add_link' %in% names(args)){


    if (!(inherits(object, "zeroinfl"))){

      if (inherits(object, "glm")){
        # Sometimes, link is given with parenthesis
        link_count <- gsub("\\s*\\([^\\)]+\\)", "",
                           Hmisc::capitalize(object$family$family))
      } else if (as.character(object$call[1]) == "lm"){
        link_count <- "Gaussian"
      } else {
        link_count <- ""
      }
      link_selection <- ""

    } else{
      link_count <- Hmisc::capitalize(object$dist)
      link_selection <- Hmisc::capitalize(object$link)
      if (!inherits(object, "glm")){
        if (inherits(object, "zeroinfl")){
          if (object$dist == "negbin") link_count <- "Negative Binomial"
        } else{
          link_count <- "Gaussian"
        }
      } else{
        if (object$dist == "negbin") link_count <- "Negative Binomial"
      }
    }

    link_labels <- c(
      "Count distribution",
      "Selection distribution"
    )


  }


  stat_labels <- c(
    "Observations",
    "Log likelihood",
    "Log likelihood (by obs.)",
    "Bayesian information criterion"
  )

  stat_val <- c(
    format(nobs(object), digits = 0,  big.mark=",", scientific = FALSE),
    format(llk, digits = 0, big.mark=",", scientific = FALSE),
    format(llk/nobs(object), digits = 3L, nsmall = 3L, big.mark=",", scientific = FALSE),
    format(bic, digits = 0L, big.mark=",", scientific = FALSE)
  )


  if ('add_link' %in% names(args)){
    stat_labels <- c(link_labels,
                     stat_labels)
    stat_val <- c(link_count, link_selection, stat_val)
  }


  df <- data.frame(
    stat = stat_labels,
    order = seq_len(length(stat_labels)),
    val = stat_val
  )


  if ('add_alpha' %in% names(args)){
    df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                           order = 0,
                           val = extract_alpha(object)), df
    )
  }

  if ('add_sigma' %in% names(args)){
    est_sigma <- mean(sigma(object))
    df <- rbind(data.frame(stat = "$\\widehat{\\sigma}$",
                           order = -1,
                           val = est_sigma), df
    )
  }


  return(df)
}


#' @rdname liststats
#' @export
liststats.light.lm <- function(object, ...){

  args <- list(...)

  # IF glm OBJECT USE DIFFERENT FUNCTION
  if (inherits(object, "light.glm")) return(liststats.light.glm(object, ...))


  llk <- object$llk
  bic <- object$bic

  if ('add_link' %in% names(args)){


    if (!(inherits(object, "zeroinfl"))){

      if (inherits(object, "glm")){
        # Sometimes, link is given with parenthesis
        link_count <- gsub("\\s*\\([^\\)]+\\)", "",
                           Hmisc::capitalize(object$family$family))
      } else if (as.character(object$call[1]) == "lm"){
        link_count <- "Gaussian"
      } else {
        link_count <- ""
      }
      link_selection <- ""

    } else{
      link_count <- Hmisc::capitalize(object$dist)
      link_selection <- Hmisc::capitalize(object$link)
      if (inherits(object, "glm")){
        if (object$dist == "negbin") link_count <- "Negative Binomial"
      } else{
        link_count <- "Gaussian"
      }
    }

    link_labels <- c(
      "Count distribution",
      "Selection distribution"
    )


  }


  stat_labels <- c(
    "Observations",
    "Log likelihood",
    "Log likelihood (by obs.)",
    "Bayesian information criterion"
  )

  stat_val <- c(
    format(object$n, digits = 0,  big.mark=",", scientific = FALSE),
    format(llk, digits = 0, big.mark=",", scientific = FALSE),
    format(llk/object$n, digits = 3L, nsmall = 3L, big.mark=",", scientific = FALSE),
    format(bic, digits = 0L, big.mark=",", scientific = FALSE)
  )


  if ('add_link' %in% names(args)){
    stat_labels <- c(link_labels,
                     stat_labels)
    stat_val <- c(link_count, link_selection, stat_val)
  }


  df <- data.frame(
    stat = stat_labels,
    order = seq_len(length(stat_labels)),
    val = stat_val
  )


  if ('add_alpha' %in% names(args)){
    df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                           order = 0,
                           val = extract_alpha(object)), df
    )
  }

  if ('add_sigma' %in% names(args)){
    est_sigma <- mean(sigma(object))
    df <- rbind(data.frame(stat = "$\\widehat{\\sigma}$",
                           order = -1,
                           val = est_sigma), df
    )
  }


  return(df)
}

#' @rdname nobs
#' @importFrom stats nobs
#' @export
nobs.zeroinfl <- function(object, ...) return(object$n)

#' @rdname nobs
#' @importFrom stats nobs
#' @export
nobs.negbin <- function(object, ...) return(length(object$residuals))



