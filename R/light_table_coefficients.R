#' Prepare coefficients part of the latex table
#'
#' @inheritParams light_table
light_table_coefficients <- function(object,
                                     ncols_models, coeff_data, order_variable,
                                     omit, covariate.labels, rules_between_covariates
                                     ){

  # ARRANGE COEFFICIENTS ORDER -------------------------

  if (ncols_models==1){
    coeff_body <- arrange_coeff(coeff_data, order_variable)
  } else{
    coeff_body <- lapply(coeff_data, arrange_coeff, order_variable)
    coeff_body <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("variable","obj"), all = TRUE),
                         coeff_body)
  }
  coeff_body <- na.omit(coeff_body)

  # REMOVE UNECESSARY COEFFICIENTS -----------------------

  if (omit != ""){
    coeff_body <- coeff_body[!(coeff_body[,'variable'] %in% omit),]
  }

  coeff_body <- coeff_body[!(coeff_body[,'variable'] == "Log(theta)"),]


  # REORDER VARIABLES --------------------------

  if (is.null(order_variable)){
    if (ncols_models>1){
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


  list_variables <- unique(coeff_body[,'variable'])


  # ARRANGE STANDARD DEVIATIONS AND SIGNS ---------------------------

  if (ncols_models>1){
    rows_sd <- grep("\\(.*?\\)", coeff_body$value.x)
  } else{
    rows_sd <- grep("\\(.*?\\)", coeff_body$value)
  }

  coeff_body$variable[sort(c(rows_sd,rows_sd+1))] <- ""


  coeff_body <- coeff_body[,!(names(coeff_body) %in% c("obj","order"))]
  body_table <- apply(coeff_body, 1, paste, collapse="")

  body_table <- gsub(pattern = "-", replacement = "$-$",
                     body_table)


  # PUT CONSTANT IN LAST POSITION ---------------------

  constant_idx <- which(coeff_body[,'variable'] == "(Intercept)")
  if (!is.null(constant_idx)){
    rows <- seq_len(nrow(coeff_body))
    rows <- rows[-constant_idx]
    coeff_body <- coeff_body[c(rows, constant_idx),]
  }

  body_table <- paste0(body_table, "\\\\")


  # REPLACE COVARIATES BY LABELS -------------------------

  if (!is.null(covariate.labels)){
    n_replace <- min(length(list_variables), length(covariate.labels))
    labels_covariates <- covariate.labels[1:n_replace]
    value_covariates <- list_variables[list_variables != "(Intercept)"]
    names(labels_covariates) <- value_covariates[1:n_replace]
    body_table <-  qdap::mgsub(
      pattern = value_covariates,
      replacement = labels_covariates,
      body_table,
    )
  }

  if (!is.null(rules_between_covariates)){
    body_table[rules_between_covariates*3] <- paste0(body_table[rules_between_covariates*3], " \\hline \\\\[-1.8ex] ")
  }

  return(body_table)
}