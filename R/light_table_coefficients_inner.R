remove_coef <- function(coeff_body, omit){
  return(
    coeff_body[!(coeff_body[,'variable'] %in% omit),]
  )
}


reorder_coef <- function(object, ncols_models,
                         coeff_body, order_variable = NULL){

  if (is.null(order_variable)){
    if (isTRUE(ncols_models>1) && !inherits(object, "nnet")){
      order_variable <- unique(do.call(c, lapply(object, listcoeff)))
    } else{
      order_variable <- unique(listcoeff(object))
    }
  }


  order_data <- data.frame(
    "variable" = order_variable,
    order = seq_len(length(order_variable))
  )

  order_data <- na.omit(order_data)
  coeff_body <- merge(coeff_body, order_data, by = "variable",
                      all.x = TRUE)
  coeff_body <- coeff_body[order(coeff_body$order, coeff_body$obj), ]

  coeff_body[coeff_body$obj != "text_coeff", "variable"] <- ""

  return(coeff_body)
}


put_constant_end <- function(coeff_body, constant_idx){

  if (is.null(constant_idx)) return(coeff_body)

  rows <- seq_len(nrow(coeff_body))
  rows_constant <- seq(constant_idx, constant_idx + 2)
  rows <- rows[-rows_constant]
  coeff_body <- coeff_body[c(rows, rows_constant),]
}



label_variables <- function(body_table,
                            list_variables,
                            covariate.labels = NULL,
                            type){

  if (is.null(covariate.labels)) return(body_table)

  n_replace <- min(length(list_variables), length(covariate.labels))
  labels_covariates <- covariate.labels[1:n_replace]
  value_covariates <- list_variables[list_variables != "(Intercept)"]
  value_covariates <- value_covariates[value_covariates != ""]

  if (identical(type, "latex")){
    value_covariates <- paste0("^", str_to_regex(value_covariates))
  } else{
    value_covariates <- str_to_regex(value_covariates)
  }
  value_covariates  <- value_covariates[value_covariates != "^"]
  labels_covariates <- labels_covariates[labels_covariates != "^"]
  # names(labels_covariates) <- value_covariates[1:n_replace]

  body_table <- mgsub(
    pattern = value_covariates,
    replacement = labels_covariates,
    paste0("^", body_table), fixed = TRUE
  )

  body_table <- gsub("^\\^", "", body_table)

  return(body_table)
}


add_rules <- function(body_table, rules_between_covariates,
                      type, ncols_models){

  if (is.null(rules_between_covariates)) return(body_table)

  if (type == "latex"){
    return(
      body_table[rules_between_covariates*3] <- paste0(body_table[rules_between_covariates*3], " \\hline \\\\[-1.8ex] ")
    )
  }

  return(
    body_table[rules_between_covariates*3] <- paste0(body_table[rules_between_covariates*3],
                                                     sprintf("<tr><td colspan=\"%s\"style=\"border-bottom: 1px solid black\"></td></tr>", ncols_models+1))
  )
}



