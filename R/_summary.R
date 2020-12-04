#' @param x Numeric Vector to be summarized
summarize_data_ <- function(x, weights = NULL, digits = 3L, ...){

  return(
    list('Min.' = format(min(x, ...), digits = digits, big.mark = ","),
         '1st Qu.' = format(as.numeric(Hmisc::wtd.quantile(x, weights = weights, probs = 0.25, ...)), digits = digits, big.mark = ","),
         'Median' = format(as.numeric(Hmisc::wtd.quantile(x, weights = weights, probs = 0.5, ...)), digits = digits, big.mark = ","),
         'Mean' = format(as.numeric(Hmisc::wtd.mean(x, weights = weights, ...)), digits = digits, big.mark = ","),
         '3rd Qu.' = format(as.numeric(Hmisc::wtd.quantile(x, weights = weights, probs = 0.75, ...)), digits = digits, big.mark = ","),
         'Max' = format(max(x, ...), digits = digits, big.mark = ",")
    )
  )
}


summary_ <- function(data, xvar,
                     weight_var = NULL,
                     ...){

  data.table::setDT(data)

  if (is.null(weight_var)){
    weight_var <- "tempvar"
    data[, c(weight_var) := 1L]
  }

  summ <- data[,summarize_data_(x = get(xvar), weights = get(weight_var), ...)]

  if (weight_var == "tempvar") data[, c(weight_var) := NULL]

  return(summ)

}


