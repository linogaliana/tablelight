#' Transform pvalues into significance stars
#'
#' @param pvalue pvalue for coefficients

signif_stars <- function(pvalue){
  if (is.na(pvalue)) return("")
  if (pvalue<0.01) return("$^{***}$")
  if (pvalue<0.05) return("$^{**}$")
  if (pvalue<0.1) return("$^{*}$")
  return("")
}
