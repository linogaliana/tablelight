#' Transform pvalues into significance stars
#'
#' @param pvalue pvalue for coefficients

signif_stars <- function(pvalue, type = c("latex", "html")){

  type <- match.arg(type)

  wildcard1 <- ifelse(identical(type, "latex"), "$^{", "<sup>")
  wildcard2 <- ifelse(identical(type, "latex"), "}$", "</sup>")

  if (is.na(pvalue)) return("")
  if (pvalue<0.01) return(sprintf("%s***%s", wildcard1, wildcard2))
  if (pvalue<0.05) return(sprintf("%s**%s", wildcard1, wildcard2))
  if (pvalue<0.1) return(sprintf("%s*%s$", wildcard1, wildcard2))
  return("")
}
