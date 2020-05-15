#' Reorder columns for nnet models
#'
#' @param coeff_body Coefficient body part of the regression table
#' @inheritParams light_table
#' @param ... Additional parameters, currently ignored
#'
#' @return Initial object (latex or html code for coefficients
#'   reporting) with the reference level at desired column
#' @export
reorder_nnet_reference <- function(object, coeff_body, reference_level_position = NULL,
                                   type, ...){
  UseMethod("reorder_nnet_reference")
}


reorder_nnet_reference.default <- function(object, coeff_body, reference_level_position = NULL,
                                           type, ...){
  return(coeff_body)
}

#' @rdname reorder_nnet_reference
#' @export
reorder_nnet_reference.nnet <- function(object, coeff_body, reference_level_position = NULL,
                                        type, ...){

  if (is.null(reference_level_position)) return(coeff_body)

  if (reference_level_position > (ncol(coeff_body)-2)){
    coeff_body2 <- cbind(
      coeff_body,
      data.frame("v" = "", stringsAsFactors = FALSE)
    )
  } else{
    coeff_body2 <- cbind(
      coeff_body[,1:(2 + reference_level_position - 1)],
      data.frame("v" = "", stringsAsFactors = FALSE),
      coeff_body[,(2 + reference_level_position):ncol(coeff_body)]
    )
  }

  coeff_body2[coeff_body2$obj == "text_coeff", "v"] <- "(Ref)"

  if (identical(type, "latex")){
    coeff_body2[,"v"] <- paste0("&",coeff_body2[,"v"])
  } else{
    coeff_body2[,"v"] <- paste0("<td>", coeff_body2[,"v"], "</td>")
  }

  return(coeff_body2)
}


#' @rdname reorder_nnet_reference
#' @export
reorder_nnet_reference.list <- function(object, coeff_body, reference_level_position = NULL,
                                        type, ...){


  if (isFALSE(inherits(object[[1]], "nnet"))) return(coeff_body)

  if (length(reference_level_position) == 1L){
    coeff_body2 <- lapply(coeff_body, function(d) {
      reorder_nnet_reference.nnet(
        object = object,
        coeff_body = d,
        type = type,
        reference_level_position = reference_level_position)
    })
  } else{
    coeff_body2 <- lapply(seq_along(coeff_body), function(i){
      reorder_nnet_reference.nnet(
        coeff_body = coeff_body[[i]],
        object = object,
        reference_level_position = reference_level_position[i],
        type = type
      )
    })
  }

  return(coeff_body2)
}
