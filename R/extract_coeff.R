#' Methods to extract coefficients and
#'  related information from statistical
#'  models
#'
#' @param object Regression objects
#' @param ... Additional parameters
#'
#' @return Coefficients ready to be merged
#'  to produce a latex table


#' @export
extract_coeff <- function(object, ...){
  UseMethod("extract_coeff")
}

#' @rdname extract_coeff
#' @export
extract_coeff.default <- function(object, ...){

  args <- list(...)

  coeff_list <- secoeff(object)

  if (as.character(object$call[1]) == "lm"){
    se_var <- 'Std. Error'
  } else{
    if (as.character(object$call[1]) == "glm"){
      se_var <- 'Std. Error'
    } else{
      se_var <- 'Std. error'
    }
  }

  if (inherits(object, "glm")){
    tstat_var <- "Pr(>|z|)"
  }  else{
    tstat_var <- "Pr(>|t|)"
  }

  text_coeff <- paste0(format(round(coeff_list[,'Estimate'],3L), digits = 3L,
                              nsmall = 3L, big.mark=",", scientific = FALSE),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- paste0("(",format(round(coeff_list[,se_var], 3L),
                               digits = 3L,
                               nsmall = 3L, big.mark=",", scientific = FALSE),
                    ")")
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}

#' @rdname extract_coeff
#' @export
extract_coeff.light.glm <- function(object, ...){

  args <- list(...)
  if (isFALSE('type' %in% names(args))) args[['type']] <- "latex"


  coeff_list <- object$coefficients

  tstat_var <- colnames(coeff_list)[4]
  se_var <- 'Std. Error'


  text_coeff <- paste0(format(round(coeff_list[,'Estimate'],3L), digits = 3L,
                              nsmall = 3L, big.mark=",", scientific = FALSE),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- paste0("(",format(round(coeff_list[,se_var],3L),
                               digits = 3L,
                               nsmall = 3L, big.mark=",", scientific = FALSE),
                    ")")
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}

#' @rdname extract_coeff
#' @export
extract_coeff.oglmx <- function(object, ...){

  args <- list(...)

  coeff_list <- secoeff(object)

  tstat_var <- "Pr(>|t|)"


  text_coeff <- paste0(format(round(coeff_list[,'Estimate'],3),
                              digits = 3L,
                              nsmall = 3L, big.mark=",", scientific = FALSE),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- paste0("(",format(round(coeff_list[,'Std. error'], 3L),
                               digits = 3L,
                               nsmall = 3L, big.mark=",", scientific = FALSE),
                    ")")
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}

#' @rdname extract_coeff
#' @export
extract_coeff.light.zeroinfl <- function(object, ...){

  args <- list(...)

  if (!("modeltype" %in% names(args))){
    message("'modeltype' argument missing, assuming 'outcome'")
    args[['modeltype']] <- 'outcome'
  }

  coeff_list <- secoeff.light.zeroinfl(object, modeltype = args[['modeltype']],...)

  namescol <- colnames(coeff_list)
  coeff_list <- data.frame(coeff_list)
  colnames(coeff_list) <- namescol

  tstat_var <- "Pr(>|z|)"
  sd_var <- "Std. Error"

  colnames(coeff_list) <- colnames(object$coefficients$count)

  text_coeff <- paste0(format(round(coeff_list[,'Estimate'],3L), digits = 3L,
                              nsmall = 3L, big.mark=",", scientific = FALSE),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- paste0("(",format(round(coeff_list[,sd_var], 3L),
                               digits = 3L,
                               nsmall = 3L, big.mark=",", scientific = FALSE),
                    ")")
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)

}

