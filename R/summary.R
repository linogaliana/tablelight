#' @param x Numeric Vector to be summarized
summarize_data_ <- function(x, weights = NULL, digits = 3L,
                            stats = c("min","1Q","median","mean","3Q","max"),
                            ...){

  summarize_vector <- function(x, weights = NULL, type = "mean", ...){
    switch(
      type,
      "min" = min(x, ...),
      "1Q" = as.numeric(Hmisc::wtd.quantile(x, weights = weights, probs = 0.25, ...)),
      "median" = as.numeric(Hmisc::wtd.quantile(x, weights = weights, probs = 0.5, ...)),
      "mean" = as.numeric(Hmisc::wtd.mean(x, weights = weights, ...)),
      "3Q" = as.numeric(Hmisc::wtd.quantile(x, weights = weights, probs = 0.75, ...)),
      "P90" = as.numeric(Hmisc::wtd.quantile(x, weights = weights, probs = 0.9, ...)),
      "max" = max(x, ...),
      "N" = length(x)
    )
  }

  l <- lapply(stats, function(st){
    format(summarize_vector(x = x,
                     weights = weights,
                     type = st,
                     ...
    ), digits = digits, big.mark = ",")}
  )

  names(l) <- data.table::data.table(
    nam = c('Min.', '1st Qu.', 'Median','Mean' ,'3rd Qu.',"P90", 'Max',"Num. obs."),
    val = c("min","1Q","median","mean","3Q","P90","max","N")
  )[get('val') %in% stats][['nam']]

  return(
    l
  )
}




summary_ <- function(data, xvar,
                     weight_var = NULL,
                     by_var = NULL,
                     stats = c("min","1Q","median","mean","3Q", "max", "N"),
                     ...){

  data.table::setDT(data)

  if (is.null(xvar) || is.na(xvar)) stop("A variable name should be provided")

  if (is.null(weight_var) || is.na(weight_var)){
    weight_var <- "tempvar"
    data[, c(weight_var) := 1L]
  }

  if (is.null(by_var) || is.na(by_var)) by_var <- NULL

  summ <- data[,summarize_data_(x = get(xvar),
                                weights = get(weight_var),
                                stats = stats,
                                ...),
               by = by_var]

  if (weight_var == "tempvar") data[, c(weight_var) := NULL]

  if (!is.null(by_var)) summ <- summ[order(get(by_var))]

  return(summ)

}


