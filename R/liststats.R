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

  args <- list(...)

  if (isFALSE("stats.list" %in% names(args))){
    stats.list <- c("n","lln","bic")
  } else{
    stats.list <- args[['stats.list']]
  }

  if (isTRUE('add_link' %in% names(args)) && (isFALSE("link" %in% stats.list))){
    stats.list <- c(stats.list, "link")
  }
  if (isTRUE('add_sigma' %in% names(args))  && (isFALSE("sigma" %in% stats.list))){
    stats.list <- c(stats.list, "sigma")
  }
  if (isTRUE('add_alpha' %in% names(args)) && (isFALSE("alpha" %in% stats.list))){
    stats.list <- c(stats.list, "alpha")
  }

  if (isFALSE("stats.digits" %in% names(args))){
    stats.digits <- 3L
  } else{
    stats.digits <- args[['stats.digits']]
  }


  llk <- object$loglik
  bic <- object$bic
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
        format(round(llk/object$n, stats.digits),
               digits = stats.digits, nsmall = stats.digits,
               big.mark=",", scientific = FALSE),
        format(bic, digits = 0L, big.mark=",", scientific = FALSE)
      )
    )
  )

  alpha_value <- ""
  if (object$dist == "negbin") alpha_value <- as.character(
    format(1/object$theta,
           digits = stats.digits,
           nsmall = stats.digits))


  df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                         order = 0,
                         val = alpha_value), df
  )

  df$shortlist <- c("alpha","link","link","n","ll","lln","bic")

  df <- df[df$shortlist %in% stats.list, ]
  df$shortlist <- NULL

  return(df)
}

#' @rdname liststats
#' @importFrom Hmisc capitalize
#' @export
liststats.zeroinfl <- function(object, ...){

  if (!inherits(object, "zeroinfl")) stop("Object is not a zeroinfl object")

  if (inherits(object, "light.zeroinfl")) return(liststats.light.zeroinfl(object, ...))


  args <- list(...)

  if (isFALSE("stats.list" %in% names(args))){
    stats.list <- c("n","lln","bic")
  } else{
    stats.list <- args[['stats.list']]
  }

  if (isTRUE('add_link' %in% names(args)) && (isFALSE("link" %in% stats.list))){
    stats.list <- c(stats.list, "link")
  }
  if (isTRUE('add_sigma' %in% names(args))  && (isFALSE("sigma" %in% stats.list))){
    stats.list <- c(stats.list, "sigma")
  }
  if (isTRUE('add_alpha' %in% names(args)) && (isFALSE("alpha" %in% stats.list))){
    stats.list <- c(stats.list, "alpha")
  }
  if (isFALSE("stats.digits" %in% names(args))){
    stats.digits <- 3L
  } else{
    stats.digits <- args[['stats.digits']]
  }


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
        format(round(llk/object$n, stats.digits),
               digits = stats.digits,
               nsmall = stats.digits,
               big.mark=",", scientific = FALSE),
        format(bic, digits = 0L, big.mark=",", scientific = FALSE)
      )
    )
  )

  alpha_value <- ""
  if (object$dist == "negbin") alpha_value <- as.character(
    format(round(1/object$theta, stats.digits),
           digits = stats.digits, nsmall = stats.digits))


  df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                         order = 0,
                         val = alpha_value), df
  )

  df$shortlist <- c("alpha","link","link","n","ll","lln","bic")

  df <- df[df$shortlist %in% stats.list, ]
  df$shortlist <- NULL

  return(df)
}


#' @rdname liststats
#' @export
liststats.light.glm <- function(object, ...){

  if (!inherits(object, "light.glm")) stop("Object is not light.glm")

  args <- list(...)

  if (isFALSE("stats.list" %in% names(args))){
    stats.list <- c("n","lln","bic")
  } else{
    stats.list <- args[['stats.list']]
  }

  if (isTRUE('add_link' %in% names(args)) && (isFALSE("link" %in% stats.list))){
    stats.list <- c(stats.list, "link")
  }
  if (isTRUE('add_sigma' %in% names(args))  && (isFALSE("sigma" %in% stats.list))){
    stats.list <- c(stats.list, "sigma")
  }
  if (isTRUE('add_alpha' %in% names(args)) && (isFALSE("alpha" %in% stats.list))){
    stats.list <- c(stats.list, "alpha")
  }

  if (isFALSE("stats.digits" %in% names(args))){
    stats.digits <- 3L
  } else{
    stats.digits <- args[['stats.digits']]
  }

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
        format(round(llk/object$n, stats.digits),
               digits = stats.digits,
               nsmall = stats.digits,
               big.mark=",", scientific = FALSE),
        format(bic, digits = 0L, big.mark=",", scientific = FALSE)
      )
    )
  )

  if (!is.null(object$theta)){
    alpha <- as.character(
      format(1/object$theta, digits = stats.digits,
             nsmall = stats.digits)
    )
  } else{
    alpha <- ""
  }

  df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                         order = 0,
                         val = alpha), df)

  df$shortlist <- c("alpha","link","link","n","ll","lln","bic")

  df <- df[df$shortlist %in% stats.list, ]
  df$shortlist <- NULL

  return(df)
}

#' @export
liststats.mindist <- function(object, ...){
  
  if (!inherits(object, "mindist")) stop("Object is not mindist")
  
  message("mindist models: only number of moments currently available")
  
  args <- list(...)
  
  df <- data.frame(
    stat = c(
      "Number of moments"),
    order = seq_len(1L),
    val = nrow(object$estimates$jacobian)
  )
  
}


#' @rdname liststats
#' @export
liststats.default <- function(object, ...){

  args <- list(...)

  if (isFALSE("stats.list" %in% names(args))){
    stats.list <- c("n","lln","bic")
  } else{
    stats.list <- args[['stats.list']]
  }
  if (isFALSE("stats.digits" %in% names(args))){
    stats.digits <- 3L
  } else{
    stats.digits <- args[['stats.digits']]
  }

  if (isTRUE('add_link' %in% names(args)) && (isFALSE("link" %in% stats.list))){
    stats.list <- c(stats.list, "link")
  }
  if (isTRUE('add_sigma' %in% names(args))  && (isFALSE("sigma" %in% stats.list))){
    stats.list <- c(stats.list, "sigma")
  }
  if (isTRUE('add_alpha' %in% names(args)) && (isFALSE("alpha" %in% stats.list))){
    stats.list <- c(stats.list, "alpha")
  }


  if (as.character(object$call[1]) == "lm" && inherits(object, "light.lm"))
    return(liststats.light.lm(object, ...))


  llk <- as.numeric(logLik(object))
  bic <- BIC(object)

  if (isTRUE(as.character(object$call[1]) == "lm")){
    rsq <- format(round(summary(object)$r.squared, stats.digits),
                  digits = stats.digits,
                  nsmall = stats.digits)
    adjrsq <- format(round(summary(object)$adj.r.squared, stats.digits),
                     digits = stats.digits,
                     nsmall = stats.digits)
  } else{
    rsq <- ""
    adjrsq <- ""
  }

  if (isTRUE('link' %in% stats.list)){


    if (inherits(object, "glm")){
      # Sometimes, link is given with parenthesis
      link_count <- gsub("\\s*\\([^\\)]+\\)", "",
                         Hmisc::capitalize(object$family$family))
    } else if (as.character(object$call[1]) %in% c("lm","fastLm","fastLm.formula")){
      link_count <- "Gaussian"
    } else {
      link_count <- ""
    }
    link_selection <- ""

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
    format(round(llk/nobs(object), stats.digits),
           digits = stats.digits,
           nsmall = stats.digits,
           big.mark=",", scientific = FALSE),
    format(bic, digits = 0L, big.mark=",", scientific = FALSE)
  )

  stat_shortcode <- c(
    "n", "ll", "lln", "bic"
  )


  if (isTRUE('link' %in% stats.list)){
    stat_labels <- c(link_labels,
                     stat_labels)
    stat_val <- c(link_count, link_selection, stat_val)
    stat_shortcode <- c("link", "link", stat_shortcode)
  }


  df <- data.frame(
    stat = stat_labels,
    order = seq_len(length(stat_labels)),
    val = stat_val
  )


  if (isTRUE('alpha' %in% stats.list)){

    df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                           order = 0,
                           val = extract_alpha(object)), df
    )
    stat_shortcode <- c("alpha", stat_shortcode)

  }

  if (isTRUE('sigma' %in% stats.list)){
    est_sigma <- mean(sigma(object))
    df <- rbind(data.frame(stat = "$\\widehat{\\sigma}$",
                           order = -1,
                           val = format(est_sigma,
                                        digits = stats.digits,
                                        nsmall = stats.digits,
                           )), df
    )
    stat_shortcode <- c("sigma", stat_shortcode)
  }


  if (isTRUE('adj.rsq' %in% stats.list)){
    df <- rbind(data.frame(stat = "Adjusted $R^2$", order = -5, val = adjrsq),
                df)
    stat_shortcode <- c("adj.rsq", stat_shortcode)
  }

  if (isTRUE('rsq' %in% stats.list)){
    df <- rbind(data.frame(stat = "$R^2$", order = -10, val = rsq),
                df)
    stat_shortcode <- c("rsq", stat_shortcode)
  }


  df <- cbind(df, 'shortcode' = stat_shortcode)
  df <- df[as.character(df$shortcode) %in% stats.list, ]

  df$shortcode <- NULL

  return(df)
}


#' @rdname liststats
#' @export
liststats.light.lm <- function(object, ...){

  args <- list(...)

  if (isFALSE("stats.list" %in% names(args))){
    stats.list <- c("n","lln","bic")
  } else{
    stats.list <- args[['stats.list']]
  }

  if (isFALSE("stats.digits" %in% names(args))){
    stats.digits <- 3L
  } else{
    stats.digits <- args[['stats.digits']]
  }

  if (isTRUE('add_link' %in% names(args)) && (isFALSE("link" %in% stats.list))){
    stats.list <- c(stats.list, "link")
  }
  if (isTRUE('add_sigma' %in% names(args))  && (isFALSE("sigma" %in% stats.list))){
    stats.list <- c(stats.list, "sigma")
  }
  if (isTRUE('add_alpha' %in% names(args)) && (isFALSE("alpha" %in% stats.list))){
    stats.list <- c(stats.list, "alpha")
  }

  # IF glm OBJECT USE DIFFERENT FUNCTION
  if (inherits(object, "light.glm")) return(liststats.light.glm(object, ...))


  llk <- object$loglikelihood
  bic <- object$bic

  if (isTRUE('link' %in% stats.list)){



    if (inherits(object, "glm")){
      # Sometimes, link is given with parenthesis
      link_count <- gsub("\\s*\\([^\\)]+\\)", "",
                         Hmisc::capitalize(object$family$family))
    } else if (as.character(object$call[1]) %in% c("lm",'fastLm','fastLm.formula')){
      link_count <- "Gaussian"
    } else {
      link_count <- ""
    }
    link_selection <- ""


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
    format(round(llk/object$n, stats.digits),
           digits = stats.digits,
           nsmall = stats.digits,
           big.mark=",", scientific = FALSE),
    format(bic, digits = 0L, big.mark=",", scientific = FALSE)
  )

  stat_shortcode <- c(
    "n", "ll", "lln", "bic"
  )

  if (isTRUE('link' %in% stats.list)){
    stat_labels <- c(link_labels,
                     stat_labels)
    stat_val <- c(link_count, link_selection, stat_val)
    stat_shortcode <- c("link", "link", stat_shortcode)
  }


  df <- data.frame(
    stat = stat_labels,
    order = seq_len(length(stat_labels)),
    val = stat_val
  )


  if (isTRUE('alpha' %in% stats.list)){
    df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                           order = 0,
                           val = extract_alpha(object)), df
    )
    stat_shortcode <- c("alpha", stat_shortcode)
  }

  if ('sigma' %in% stats.list){
    est_sigma <- mean(sigma(object))
    df <- rbind(data.frame(stat = "$\\widehat{\\sigma}$",
                           order = -1,
                           val = est_sigma), df
    )
    stat_shortcode <- c("sigma", stat_shortcode)
  }

  if ('adj.rsq' %in% stats.list){
    df <- rbind(data.frame(stat = "Adjusted $R^2$",
                           order = -10,
                           val = format(object$adjrsq,
                                        digits = stats.digits,
                                        nsmall = stats.digits)), df
    )
    stat_shortcode <- c("adj.rsq", stat_shortcode)
  }


  if ('rsq' %in% stats.list){
    df <- rbind(data.frame(stat = "$R^2$",
                           order = -10,
                           val = format(round(object$rsq, stats.digits),
                                        digits = stats.digits,
                                        nsmall = stats.digits)), df
    )
    stat_shortcode <- c("rsq", stat_shortcode)
  }

  df <- cbind(df, 'shortcode' = stat_shortcode)
  df <- df[as.character(df$shortcode) %in% stats.list, ]

  df$shortcode <- NULL

  return(df)
}


#' @rdname liststats
#' @export
liststats.fastLm <- function(object, ...){

  args <- list(...)

  if (isFALSE("stats.list" %in% names(args))){
    stats.list <- c("n","lln","bic")
  } else{
    stats.list <- args[['stats.list']]
  }
  if (isFALSE("stats.digits" %in% names(args))){
    stats.digits <- 3L
  } else{
    stats.digits <- args[['stats.digits']]
  }

  if (isTRUE('add_link' %in% names(args)) && (isFALSE("link" %in% stats.list))){
    stats.list <- c(stats.list, "link")
  }
  if (isTRUE('add_sigma' %in% names(args))  && (isFALSE("sigma" %in% stats.list))){
    stats.list <- c(stats.list, "sigma")
  }
  if (isTRUE('add_alpha' %in% names(args)) && (isFALSE("alpha" %in% stats.list))){
    stats.list <- c(stats.list, "alpha")
  }


  llk <- as.numeric(logLik(object))

  if (inherits(llk, "logLik")){
    # RcppEigen
    bic <- BIC(object)
  } else{
    # RcppArmadillo
    bic <- BIC_fastLm(object)
  }


  rsq <- format(round(summary(object)$r.squared, stats.digits),
                digits = stats.digits,
                nsmall = stats.digits)
  adjrsq <- format(round(summary(object)$adj.r.squared, stats.digits),
                   digits = stats.digits,
                   nsmall = stats.digits)

  if (isTRUE('link' %in% stats.list)){

    link_count <- "Gaussian"
    link_selection <- ""

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
    format(round(llk/nobs(object), stats.digits),
           digits = stats.digits,
           nsmall = stats.digits,
           big.mark=",", scientific = FALSE),
    format(bic, digits = 0L, big.mark=",", scientific = FALSE)
  )

  stat_shortcode <- c(
    "n", "ll", "lln", "bic"
  )


  if (isTRUE('link' %in% stats.list)){
    stat_labels <- c(link_labels,
                     stat_labels)
    stat_val <- c(link_count, link_selection, stat_val)
    stat_shortcode <- c("link", "link", stat_shortcode)
  }


  df <- data.frame(
    stat = stat_labels,
    order = seq_len(length(stat_labels)),
    val = stat_val
  )


  if (isTRUE('alpha' %in% stats.list)){

    df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
                           order = 0,
                           val = extract_alpha(object)), df
    )
    stat_shortcode <- c("alpha", stat_shortcode)

  }

  if (isTRUE('sigma' %in% stats.list)){
    est_sigma <- mean(sigma(object))
    df <- rbind(data.frame(stat = "$\\widehat{\\sigma}$",
                           order = -1,
                           val = format(est_sigma,
                                        digits = stats.digits,
                                        nsmall = stats.digits,
                           )), df
    )
    stat_shortcode <- c("sigma", stat_shortcode)
  }


  if (isTRUE('adj.rsq' %in% stats.list)){
    df <- rbind(data.frame(stat = "Adjusted $R^2$", order = -5, val = adjrsq),
                df)
    stat_shortcode <- c("adj.rsq", stat_shortcode)
  }

  if (isTRUE('rsq' %in% stats.list)){
    df <- rbind(data.frame(stat = "$R^2$", order = -10, val = rsq),
                df)
    stat_shortcode <- c("rsq", stat_shortcode)
  }


  df <- cbind(df, 'shortcode' = stat_shortcode)
  df <- df[as.character(df$shortcode) %in% stats.list, ]

  df$shortcode <- NULL

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


#' @rdname nobs
#' @importFrom stats nobs
#' @export
nobs.nnet <- function(object, ...) return(nrow(object$residuals))


