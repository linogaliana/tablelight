#' Transform pvalues into significance stars
#'
#' @param pvalue pvalue for coefficients
#' @param type Should the stars be *latex*
#'   or *html* formatted ?

signif_stars <- function(pvalue, type = c("latex", "html", "none")){

  type <- match.arg(type)

  wildcard1 <- switch(
    type,
    latex = "$^{",
    html = "<sup>",
    none = ""
  )

  wildcard2 <- switch(
    type,
    latex = "}$",
    html = "</sup>",
    none = ""
  )

  if (is.na(pvalue)) return("")
  if (pvalue<0.01) return(sprintf("%s***%s", wildcard1, wildcard2))
  if (pvalue<0.05) return(sprintf("%s**%s", wildcard1, wildcard2))
  if (pvalue<0.1) return(sprintf("%s*%s", wildcard1, wildcard2))
  return("")
}


signif_stars_vectorized <- function(pvalue, type = c("latex", "html", "none")){

  type <- match.arg(type)

  return(
    sapply(pvalue, signif_stars, type = type)
  )

}
