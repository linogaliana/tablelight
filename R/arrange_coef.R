#' Arrange coefficients for the table
#'
#' Transform output from \link{secoeff} into
#'  a three rows object that can be used
#'  after some transformation in a table
#' @inheritParams light_table
#' @param text_coeff Output from \link{secoeff}
#' @param type Should the coeff be formatted for latex
#'  or html reporting
#'
arrange_coeff <- function(text_coeff, order_variable = NULL, type = c("latex","html")){

  type <- match.arg(type)

  namescol <- colnames(text_coeff)
  text_coeff <- data.frame(text_coeff, stringsAsFactors = FALSE)
  colnames(text_coeff) <- namescol


  if (is.null(nrow(text_coeff))){
    text_coeff <- rbind(text_coeff, "")
  }

  text_coeff <- cbind(text_coeff,
                      data.frame("text_zempty" = "", stringsAsFactors = FALSE)
  )
  # text_coeff <- data.frame(text_coeff)



  if (!('variable' %in% colnames(text_coeff))){
    text_coeff$variable <- rownames(text_coeff)
  }


  body_table <- reshape2::melt(
    text_coeff, id.vars = "variable",
    variable.name = "obj",
    factorsAsStrings=TRUE
  )


  list_variables_model <-  unique(body_table[,'variable'])

  if (!is.null(order_variable)){
    # we add variables not listed in order_variable
    order_variable <- c(order_variable[order_variable %in% list_variables_model],
                        as.character(list_variables_model)[!(list_variables_model %in% order_variable)]
    )
  }

  if (is.null(order_variable)){
    body_table <- body_table[order(body_table$variable,
                                   body_table$obj), ]
  } else{
    order_data <- data.frame(
      "variable" = order_variable,
      order = seq_len(length(order_variable))
    )
    body_table <- merge(body_table, order_data, by = "variable")
    body_table <- body_table[order(body_table$order,
                                   body_table$obj), ]
    body_table$order <- NULL
  }
  body_table$variable <- as.character(body_table$variable)

  if (type == "latex"){
    body_table$value <- paste0(" & ", body_table$value)
  } else{
    body_table$value <- paste0("<td>", body_table$value, "</td>")
  }

  return(body_table)
}


combine_coef <- function(d, order_variable, type){

  coeff_body <- lapply(d, arrange_coeff, order_variable, type = type)
  lapply(seq_along(coeff_body), function(i) data.table::setnames(coeff_body[[i]], old = "value",
                                                                 new = paste0("value",i)))
  coeff_body <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("variable","obj"), all = TRUE),
                       coeff_body)
  return(coeff_body)
}


apply_arrange_coef <- function(object, coeff_data,
                               order_variable, type, reference_level_position = NULL){


  if (inherits(object[[1]], "nnet")){

    coeff_body <- lapply(coeff_data, function(d){
      combine_coef(d, order_variable, type = type)
    })
    coeff_body <- reorder_nnet_reference(object = object, coeff_body = coeff_body,
                           reference_level_position = reference_level_position,
                           type = type)

    lapply(seq_along(coeff_body), function(i){
      data.table::setnames(coeff_body[[i]],
                           old = colnames(coeff_body[[i]])[grepl(
                             "(^value|^v$)",colnames(coeff_body[[i]])
                           )],
                           new = paste0(colnames(coeff_body[[i]])[grepl(
                             "(^value|^v$)",colnames(coeff_body[[i]])
                           )], "_", i)
                           )
    })
    coeff_body <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("variable","obj"), all = TRUE),
                         coeff_body)

    return(
      coeff_body
    )

  } else if (inherits(object, "list")){
    return(combine_coef(d = coeff_data, order_variable, type))
  } else{
    # Other cases (only one model)
    return(
      arrange_coeff(coeff_data, order_variable, type = type)
    )
  }



}
