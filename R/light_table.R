#' Produce latex tables from stripped objects to reduce memory needs
#'
#' @param object List of object
#' @param modeltype Character vectors indicating whether
#'  we should use selection or outcome object. Ignored if
#'  object is not zeroinfl
#' @param title Table caption
#' @param label Table label
#' @param dep.var.labels Label for dependent variables
#' @param dep.var.separate Numeric vector that specifies how
#'  `dep.var.labels` should be laid out across regression
#'  table columns. A value of c(2, 1, 3), for instance, will
#'  apply the first label to the two first columns, the second
#'  label to the third column, and the third label will apply
#'  to the following three columns (i.e., columns
#'  number four, five and six).
#' @param stats.var.separate Numeric vector that specifies how
#'  statistics should be laid out across regression
#'  table columns. A value of c(2, 1, 3), for instance, will
#'  apply the first statistics to the two first columns, the second
#'  statistics to the third column, and the statistics
#'  will apply
#'  to the following three columns (i.e., columns
#'  number four, five and six).
#' @param column.labels Label for columns
#' @param covariate.labels A character vector of labels for
#'  columns in regression tables.
#'  Their layout, in terms of the number of columns
#'  associated with each label, is given by the
#'  argument `column.separate`.
#' @param order_variable A vector that indicates the order
#'  in which variables will appear in the output.
#' @param column.separate A numeric vector that specifies how
#'  column.labels should be laid out across regression table
#'  columns. A value of `c(2, 1, 3)`, for instance, will apply
#'  the first label to the two first columns, the second label
#'  to the third column, and the third label will apply to
#'  the following three columns (i.e., columns
#'  number four, five and six). If the argument's value is `NULL`
#'  or the regression table contains more columns than are
#'  referred to in `column.separate`, a value of `1`
#'  is assumed for each *excess* column label.
#' @param add.lines Rows to add in object statistics part
#' @param notes Notes that should be added at the end
#' @param omit List of variables that should be removed from
#'  the table
#' @param rules_between_covariates A numeric vector that specifies how
#'  rules should be laid out across rows of the regression table
#' @param landscape Logical value indicating whether we want to
#'  use a landscape table. Default to `FALSE`
#' @param ... Additional arguments that should be passed. See, for instance,
#'   \link{liststats}
#' @param adjustbox_width If the table needs to be adjusted to page width,
#'  what should be the parameter ?
#'
#' This function is designed to produce `latex` tables with
#'  stripped objects (see \link{strip}). It follows
#'  `stargazer` standards but proposes a
#'  simplified framework. Customization is limiteds
#'
#' @return A character vector.
#'
#' @examples \dontrun{data("bioChemists", package = "pscl")
#'
#' fm_zip    <- pscl::zeroinfl(art ~ . | ., data = bioChemists)
#' fm_zip2   <- pscl::zeroinfl(art ~ 1 | ., data = bioChemists)
#' glm_model <- glm(art ~ . , data = bioChemists)
#' fm_zip3   <- pscl::zeroinfl(art ~ 1 | ., data = bioChemists,
#'                             dist = "negbin")
#' fm_zip5   <- MASS::glm.nb(art ~ 1, data = bioChemists)
#'
#' model_list <- lapply(list(fm_zip, fm_zip2,
#'                           glm_model, fm_zip3,
#'                           fm_zip5), texlight::strip)
#'
#' cat(
#'   texlight::light_table(object = model_list,
#'                         covariate.labels = c("x1","x2")),
#'   sep = "\n"
#' )
#' }
#'
#' @importFrom qdap mgsub
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @export

light_table <- function(object,
                        modeltype = "outcome",
                        title = "Title",
                        label = "label",
                        dep.var.labels = "Label dep.var.labels",
                        dep.var.separate = NULL,
                        column.labels = "blab",
                        column.separate = NULL,
                        covariate.labels = NULL,
                        order_variable = NULL,
                        stats.var.separate = NULL,
                        notes = "notes to add",
                        add.lines = "",
                        rules_between_covariates = NULL,
                        omit = "",
                        landscape = FALSE,
                        adjustbox_width = c(NULL, 1.1),
                        ...){
  UseMethod("light_table")
}

#' @export
light_table.default <- function(
  object,
  modeltype = "outcome",
  title = "Title",
  label = "label",
  dep.var.labels = "Label dep.var.labels",
  dep.var.separate = NULL,
  column.labels = "blab",
  column.separate = NULL,
  covariate.labels = NULL,
  order_variable = NULL,
  stats.var.separate = NULL,
  notes = "notes to add",
  add.lines = "",
  rules_between_covariates = NULL,
  omit = "",
  landscape = FALSE,
  adjustbox_width = c(NULL, 1.1),
  ...){


  if (missing(adjustbox_width)) adjustbox_width <- NULL

  ncols_models <- length(object)

  if (identical(ncols_models, 1L)){
    coeff_data <- extract_coeff(object)
  } else{
    coeff_data <- lapply(1:length(object),
                         function(k){
                           return(
                             extract_coeff(
                               object = object[[k]],
                               modeltype = modeltype[k]
                             )
                           )
                         })
  }





  # PART I : HEAD -------


  table_total <- light_table_header(
    ncols_models,
    title = title,
    label = label,
    dep.var.labels = dep.var.labels,
    dep.var.separate = dep.var.separate,
    column.labels = column.labels,
    adjustbox_width = adjustbox_width)


  # PART II : BODY -------

  body_table <- light_table_coefficients(
    object = object,
    ncols_models = ncols_models,
    coeff_data = coeff_data,
    order_variable = order_variable,
    omit = omit,
    covariate.labels = covariate.labels,
    rules_between_covariates = rules_between_covariates
  )

  table_total <- c(table_total, body_table, "\\hline \\hline \\\\[-1.8ex] ")



  # PART III: STATISTICS -----

  stats_table <- light_table_stats(object = object,
                                   ncols_models = ncols_models,
                                   stats.var.separate = stats.var.separate)

  table_total <- c(table_total, stats_table,
                   "\\hline ",
                   "\\hline \\\\[-1.8ex] ")


  # PART IV: FOOTER -----

  foot_table <- light_table_footer(
    ncols_models = ncols_models,
    add.lines = add.lines,
    adjustbox_width = adjustbox_width)

  table_total <- c(table_total, foot_table)

  if (landscape) table_total <- c("\\begin{landscape}", table_total, "\\end{landscape}")

  return(table_total)
}
