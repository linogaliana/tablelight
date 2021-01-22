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
extract_coeff <- function(object, digits = max(3L, getOption("digits") - 3L), ...){
  UseMethod("extract_coeff")
}

#' @rdname extract_coeff
#' @export
extract_coeff.default <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)

  coeff_list <- secoeff(object)

  if (as.character(object$call[1]) %in% c("lm","glm")){
    se_var <- 'Std. Error'
  } else{
    se_var <- 'Std. error'
  }

  if (inherits(object, "glm")){
    tstat_var <- "Pr(>|z|)"
  }  else{
    tstat_var <- "Pr(>|t|)"
  }

  text_coeff <- paste0(format_number(coeff_list[,'Estimate'], digits = digits),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list[,se_var], digits = digits))

  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}


#' @rdname extract_coeff
#' @export
extract_coeff.fastLm <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)


  coeff_list <- secoeff(object)

  # For RcppEigen models
  se_var <- 'Std. Error'
  # Correction for RcppArmadillo models
  if (isFALSE(se_var %in% colnames(coeff_list))){
    se_var <- 'StdErr'
  }

  # For RcppEigen models
  tstat_var <- "Pr(>|t|)"
  if (isFALSE(tstat_var %in% colnames(coeff_list))){
    tstat_var <- 'p.value'
  }

  text_coeff <- paste0(format_number(coeff_list[,'Estimate'], digits = digits),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )

  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list[,se_var], digits = digits))

  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}


#' @rdname extract_coeff
#' @export
extract_coeff.light.glm <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)


  if (isFALSE('type' %in% names(args))) args[['type']] <- "latex"


  coeff_list <- object$coefficients

  tstat_var <- colnames(coeff_list)[4]
  se_var <- 'Std. Error'


  text_coeff <- paste0(format_number(coeff_list[,'Estimate'], digits = digits),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list[,se_var], digits = digits))
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}

#' @rdname extract_coeff
#' @export
extract_coeff.oglmx <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)


  coeff_list <- secoeff(object)

  se_var <- 'Std. error'
  tstat_var <- "Pr(>|t|)"


  text_coeff <- paste0(format_number(coeff_list[,'Estimate'], digits = digits),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list[,se_var], digits = digits))
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  # remove sigma from coefficients
  text_coeff <- text_coeff[!is.na(text_coeff[,'variable']),]

  return(text_coeff)
}

#' @rdname extract_coeff
#' @export
extract_coeff.light.zeroinfl <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)


  if (!("modeltype" %in% names(args))){
    message("'modeltype' argument missing, assuming 'outcome'")
    args[['modeltype']] <- 'outcome'
  }

  coeff_list <- secoeff.light.zeroinfl(object, modeltype = args[['modeltype']],...)

  # selection: coeffs = - coeffs zeros
  if (isTRUE(args[['modeltype']] == 'selection')) coeff_list[,'Estimate'] <- -coeff_list[,'Estimate']

  namescol <- colnames(coeff_list)
  coeff_list <- data.frame(coeff_list)
  colnames(coeff_list) <- namescol

  tstat_var <- "Pr(>|z|)"
  se_var <- "Std. Error"

  colnames(coeff_list) <- colnames(object$coefficients$count)

  text_coeff <- paste0(format_number(coeff_list[,'Estimate'], digits = digits),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list[,se_var], digits = digits))
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)

}

#' @rdname extract_coeff
#' @export
extract_coeff.fastzeroinfl <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)


  if (!("modeltype" %in% names(args))){
    message("'modeltype' argument missing, assuming 'outcome'")
    args[['modeltype']] <- 'outcome'
  }

  coeff_list <- secoeff(object, modeltype = args[['modeltype']],...)

  # selection: coeffs = - coeffs zeros
  if (isTRUE(args[['modeltype']] == 'selection')) coeff_list[,'Estimate'] <- -coeff_list[,'Estimate']

  namescol <- colnames(coeff_list)
  coeff_list <- data.frame(coeff_list)
  colnames(coeff_list) <- namescol

  tstat_var <- "Pr(>|z|)"
  se_var <- "Std. Error"


  text_coeff <- paste0(format_number(coeff_list[,'Estimate'], digits = digits),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list[,se_var], digits = digits))
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}

#' @rdname extract_coeff
#' @export
extract_coeff.mindist <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)

  coeff_list <- secoeff(object, ...)

  tstat_var <- "Pr(>|z|)"
  se_var <- "Std. Error"

  text_coeff <- paste0(format_number(coeff_list[,'Estimate'], digits = digits),
                       sapply(coeff_list[,tstat_var], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list[,se_var], digits = digits))

  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = names(object$estimates$theta_hat),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}


#' @rdname extract_coeff
#' @export
extract_coeff.nnet <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)

  # EXTRACT SINGLE MODALITY (DEFAULT TO FIRST ONE NOT BEING REFERENCE)
  if (isFALSE("modality" %in% names(args))){
    modality <- object$lab[-1]
  } else{
    modality <- args[['modality']]
  }

  if (length(modality)>1){
    lapply(modality, function(m) extract_coeff_nnet(object, modality = m, digits = digits, type = args[['type']]))
  } else{
    return(extract_coeff_nnet(object, modality = modality, digits = digits, type = args[['type']]))
  }

}



extract_coeff_nnet <- function(object, modality, digits = max(3L, getOption("digits") - 3L), ...){

  args <- list(...)

  coeff_list <- secoeff(object)

  tstat_var <- "Pr(>|z|)"
  se_var <- "Std. Error"

  coeff_list_red <- lapply(names(coeff_list), function(elem){
    coeff_list[[elem]][,as.character(modality)]
  })
  names(coeff_list_red) <- names(coeff_list)

  text_coeff <- paste0(format_number(coeff_list_red[['Estimate']], digits = digits),
                       sapply(coeff_list_red[[tstat_var]], signif_stars, type = args[['type']])
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- sprintf("(%s)", format_number(coeff_list_red[[se_var]], digits = digits))
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- data.frame("variable" = names(coeff_list_red[[1]]),
                           "text_coeff" = text_coeff, "text_sd" = text_sd,
                           stringsAsFactors = FALSE)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)
}



apply_extract_coeff <- function(object, ncols_models,
                                type, modeltype, stats.digits){

  if (inherits(object, "nnet")) return(
    extract_coeff(object, type = type, digits = stats.digits)
  )

  if (identical(ncols_models, 1L)) return(
    return(
      extract_coeff(object, type = type, digits = stats.digits)
    )
  )

  coeff_data <- lapply(1:length(object),
                       function(k){
                         return(
                           extract_coeff(
                             object = object[[k]],
                             modeltype = modeltype[k],
                             type = type,
                             digits = stats.digits
                           )
                         )
                       })

  return(coeff_data)
}



