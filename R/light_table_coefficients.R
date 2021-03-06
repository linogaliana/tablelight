#' Prepare coefficients part of the latex table
#'
#' @inheritParams light_table
#' @param ncols_models Number of columns
#' @param coeff_data Output from \link{secoeff}
#' @inheritParams light_table
#' @importFrom data.table setnames
light_table_coefficients <- function(object,
                                     ncols_models,
                                     type,
                                     coeff_data, order_variable,
                                     omit, covariate.labels, reference_level_position,
                                     rules_between_covariates
){

  # ARRANGE COEFFICIENTS ORDER -------------------------

  if (ncols_models==1){
    coeff_body <- arrange_coeff(coeff_data, order_variable, type = type)
  } else{
    coeff_body <- lapply(coeff_data, arrange_coeff, order_variable, type = type)
    lapply(seq_along(coeff_body), function(i) data.table::setnames(coeff_body[[i]], old = "value",
                                                                   new = paste0("value",i)))
    coeff_body <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("variable","obj"), all = TRUE),
                         coeff_body)
  }

#  coeff_body <- na.omit(coeff_body)
  if (type == "latex"){
    coeff_body[is.na(coeff_body)] <- ""
  } else{
    coeff_body[is.na(coeff_body)] <- "<td></td>"
  }

  # REMOVE UNECESSARY COEFFICIENTS -----------------------

  if (!is.null(omit)) coeff_body <- remove_coef(coeff_body = coeff_body,
                                  omit = omit)

  coeff_body <- coeff_body[!(coeff_body[,'variable'] == "Log(theta)"),]

  # ARRANGE COEFFICIENTS ORDER -------------------------

  if (inherits(object, "nnet")){
    coeff_body <- reorder_nnet_reference(
      coeff_body = coeff_body,
      reference_level_position = reference_level_position,
      type = type
    )
  }

  coeff_body <- reorder_coef(object = object,
                             ncols_models = ncols_models,
                             coeff_body = coeff_body,
                             order_variable = order_variable)


  list_variables <- unique(coeff_body[,'variable'])


  # ARRANGE STANDARD DEVIATIONS ---------------------------

  if (ncols_models>1){
    rows_sd <- grep("\\(.*?\\)", coeff_body$value.x)
  } else{
    rows_sd <- grep("\\(.*?\\)", coeff_body$value)
  }

  coeff_body$variable[sort(c(rows_sd,rows_sd+1))] <- ""


  # PREPARE CONCATENATION ----------

  coeff_body <- coeff_body[,!(names(coeff_body) %in% c("obj","order"))]

  if (identical(type, "html")){
    coeff_body$variable <- sprintf('<tr><td style="text-align:left">%s</td>', coeff_body$variable)
  }

  if (!is.null(covariate.labels)){
    coeff_body[coeff_body$variable != "(Intercept)", "variable"] <-
      str_to_regex(coeff_body[coeff_body$variable != "(Intercept)", "variable"])
  }

  body_table <- apply(coeff_body, 1, paste, collapse="")

  if (identical(type, "latex")){
    body_table <- gsub(pattern = "-", replacement = "$-$",
                       body_table)
  }

  # PUT CONSTANT IN LAST POSITION ---------------------

  constant_idx <- which(coeff_body[,'variable'] == "(Intercept)")

  coeff_body <- put_constant_end(
    coeff_body = coeff_body,
    constant_idx = constant_idx
  )

  if (identical(type, "latex")) body_table <- paste0(body_table, "\\\\")


  # REPLACE COVARIATES BY LABELS -------------------------
  # (if needed)

  body_table <- label_variables(
    body_table = body_table,
    list_variables = list_variables,
    covariate.labels = covariate.labels,
    type = type)

  body_table <- gsub("\\\\\\(Intercept\\\\)","(Intercept)",
                     body_table)

  # ADD RULES IF NEEDED -------------------------

  body_table <- add_rules(
    body_table = body_table,
    rules_between_covariates = rules_between_covariates,
    type = type,
    ncols_models = ncols_models
  )

  return(body_table)
}
