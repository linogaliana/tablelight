format_number_p <- function(x, digits = max(3L, getOption("digits") - 3L), ...){

  x_form <- formatC(x, digits = digits,
          format = "f", big.mark = ",")

  formatC(x, digits = 8, format = "f")
  if (x_form == paste(c("0.", rep('0', digits)),
                      collapse = "")){
    x_form <- formatC(x, digits = 0, format = "e", big.mark = ",")
  }

  return(x_form)
}


format_number <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  sapply(x, format_number_p, digits, ...)
}
