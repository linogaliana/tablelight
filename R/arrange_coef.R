#' Arrange coefficients for the table
#'
#' Transform output from \link{secoeff} into
#'  a three rows object that can be used
#'  after some transformation in a table
#' @inheritParams light_table
#' @param text_coeff Output from \link{secoeff}
#'
arrange_coeff <- function(text_coeff, order_variable = NULL, type = c("latex","html")){

  type <- match.arg(type)

  namescol <- colnames(text_coeff)
  text_coeff <- data.frame(text_coeff)
  colnames(text_coeff) <- namescol


  if (is.null(nrow(text_coeff))){
    text_coeff <- rbind(text_coeff, "")
  }

  text_coeff <- cbind(text_coeff, "text_zempty" = "")
  # text_coeff <- data.frame(text_coeff)



  if (!('variable' %in% colnames(text_coeff))){
    text_coeff$variable <- rownames(text_coeff)
  }


  body_table <- reshape2::melt(
    text_coeff, id.vars = "variable",
    variable.name = "obj",
    factorsAsStrings=FALSE
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

