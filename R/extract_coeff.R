#' Methods to extract coefficients and
#'  related information from statistical
#'  models
#'
#' @param object Regression objects
#' @param ... Additional parameters
#'
#' @return Coefficients ready to be merged
#'  to produce a latex table

# extract_coeff <- function(idx, model_list, modeltype){
#
#   type <- modeltype[idx]
#
#   if (!(inherits(model_list[[idx]], "zeroinfl") |
#         (inherits(model_list[[idx]], "negbin")))){
#
#     tstat_var <- 'Pr(>|t|)'
#     if (inherits(mod, "glm")) tstat_var <- 'Pr(>|z|)'
#
#     mod <- model_list[[idx]]
#     text_coeff <- paste0(round(mod$coefficients[,'Estimate'],3),
#                          sapply(mod$coefficients[,tstat_var], signif_stars))
#     text_sd <- paste0("(",round(mod$coefficients[,'Std. Error'],3),")")
#     text_coeff <- cbind("variable" = rownames(mod$coefficients),text_coeff, text_sd)
#     text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
#                                     fixed = TRUE)
#     return(text_coeff)
#   }
#
#   mod <- model_list[[idx]]
#   if (inherits(model_list[[idx]], "zeroinfl")){
#     if ((is.na(type)) || (type == "selection")){
#       clist <- mod$coefficients$zero
#     } else{
#       clist <- mod$coefficients$count
#     }
#   } else{
#     clist <- mod$coefficients
#   }
#
#   text_coeff <- paste0(round(clist[,'Estimate'],3),
#                        sapply(clist[,'Pr(>|z|)'], signif_stars))
#   text_sd <- paste0("(",round(clist[,'Std. Error'],3),")")
#   text_coeff <- cbind("variable" = rownames(clist),text_coeff, text_sd)
#   text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
#                                   fixed = TRUE)
#
#   return(text_coeff)
#
# }

#' @export
extract_coeff <- function(object, ...){
  UseMethod("extract_coeff")
}

#' @rdname extract_coeff
#' @export
extract_coeff.default <- function(object, ...){

  # if (inherits(object, "light.zeroinfl")){
  #   return(
  #     extract_coeff.light(object, ...)
  #   )
  # }

  coeff_list <- secoeff(object)

  tstat_var <- "Pr(>|t|)"


  text_coeff <- paste0(format(coeff_list[,'Estimate'],digits = 3L,
                              nsmall = 3L, big.mark=",", scientific = FALSE),
                       sapply(coeff_list[,tstat_var], signif_stars)
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- paste0("(",format(coeff_list[,'Std. error'],
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

  coeff_list <- secoeff(object)

  tstat_var <- "Pr(>|t|)"


  text_coeff <- paste0(format(coeff_list[,'Estimate'],digits = 3L,
                              nsmall = 3L, big.mark=",", scientific = FALSE),
                       sapply(coeff_list[,tstat_var], signif_stars)
                       )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- paste0("(",format(coeff_list[,'Std. error'],
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

  coeff_list <- secoeff.light.zeroinfl(object, ...)

  namescol <- colnames(coeff_list)
  coeff_list <- data.frame(coeff_list)
  colnames(coeff_list) <- namescol

  tstat_var <- "Pr(>|z|)"
  sd_var <- "Std. Error"

  text_coeff <- paste0(format(coeff_list[,'Estimate'],digits = 3L,
                              nsmall = 3L, big.mark=",", scientific = FALSE),
                       sapply(coeff_list[,tstat_var], signif_stars)
  )
  text_coeff <- gsub(x = text_coeff, pattern = " ", replacement = "")

  text_sd <- paste0("(",format(coeff_list[,sd_var],
                               digits = 3L,
                               nsmall = 3L, big.mark=",", scientific = FALSE),
                    ")")
  text_sd <- gsub(x = text_sd, pattern = " ", replacement = "")

  text_coeff <- cbind("variable" = rownames(coeff_list),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)

}

