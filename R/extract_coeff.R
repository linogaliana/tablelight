#' Wrapper to extract coefficients from a list of models
#'
#' @param idx Index
#' @inheritParams light_table
#' @return List with coefficients ready to be merged
#'  to produce a latex table

extract_coeff <- function(idx, model_list, modeltype){

  type <- modeltype[idx]

  if (!(inherits(model_list[[idx]], "zeroinfl") |
        (inherits(model_list[[idx]], "negbin")))){

    tstat_var <- 'Pr(>|t|)'
    if (inherits(mod, "glm")) tstat_var <- 'Pr(>|z|)'

    mod <- model_list[[idx]]
    text_coeff <- paste0(round(mod$coefficients[,'Estimate'],3),
                         sapply(mod$coefficients[,tstat_var], signif_stars))
    text_sd <- paste0("(",round(mod$coefficients[,'Std. Error'],3),")")
    text_coeff <- cbind("variable" = rownames(mod$coefficients),text_coeff, text_sd)
    text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                    fixed = TRUE)
    return(text_coeff)
  }

  mod <- model_list[[idx]]
  if (inherits(model_list[[idx]], "zeroinfl")){
    if ((is.na(type)) || (type == "selection")){
      clist <- mod$coefficients$zero
    } else{
      clist <- mod$coefficients$count
    }
  } else{
    clist <- mod$coefficients
  }

  text_coeff <- paste0(round(clist[,'Estimate'],3),
                       sapply(clist[,'Pr(>|z|)'], signif_stars))
  text_sd <- paste0("(",round(clist[,'Std. Error'],3),")")
  text_coeff <- cbind("variable" = rownames(clist),text_coeff, text_sd)
  text_coeff[,'variable'] <- gsub("_","\\_",text_coeff[,'variable'],
                                  fixed = TRUE)

  return(text_coeff)

}
