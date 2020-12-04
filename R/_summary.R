_summarize_data <- function(x, weights = NULL, ...){
  return(
    list('Min.' = round(min(x, ...)),
         '1st Qu.' = round(Hmisc::wtd.quantile(x, weights = weights, probs = 0.25, ...)),
         'Median' = round(Hmisc::wtd.quantile(x, weights = weights, probs = 0.5, ...)),
         'Mean' = round(Hmisc::wtd.mean(x, weights = weights, ...)),
         '3rd Qu.' = round(Hmisc::wtd.quantile(x, weights = weights, probs = 0.75, ...)),
         'Max' = round(max(x, ...))
    )
  )
}


_summary <- function(data, x,
                     pond = NULL,
                     ...){

  data.table::setDT(data)

  if (is.null(pond)){
    pond <- "tempvar"
    data[, c(pond) := 1L]
  }

  if (pond == "tempvar") data[, c(pond) := NULL]

  return(data)

}
