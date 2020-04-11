#' Produce latex tables from stripped objects to reduce memory needs
#'
#' @param object List of object
#' @param type Reporting output used. Accepted values are
#'   *latex* and *html*
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
#' @param stats.list A character vector that specifies which model statistics should
#'  be kept in the regression table output. See `details` section
#' @param column.labels Label for columns
#' @param covariate.labels A character vector of labels for
#'  columns in regression tables.
#'  Their layout, in terms of the number of columns
#'  associated with each label, is given by the
#'  argument `column.separate`.
#' @param order_variable A vector that indicates the order
#'  in which variables will appear in the output.
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
#' @param visualize Logical condition indicating whether we want to print
#'  the table on Rstudio viewer. Ignored if `type` = *latex*
#' @param reference_level_position In multinomial logit, coefficients are
#'  relative to the reference level. A column can be added to represent
#'  an empty coefficient modality that is indicated as being the reference.
#'  If `NULL`, this feature is ignored. Otherwise, the column is placed at
#'  the position indicated by the argument
#'
#' This function is designed to produce `latex` tables with
#'  stripped objects (see \link{strip}). It follows
#'  `stargazer` standards but proposes a
#'  simplified framework. Customization is limited
#'
#' @return A character vector. The table is printed in the viewer
#'  if `type` is *html* and `visualize` is `TRUE`.
#'
#' @details The statistics that are accepted are, for the moment:
#' \itemize{
#'  \item{*"n"*: }{Number of observations}
#'  \item{*"ll"*: }{Log likelihood}
#'  \item{*"lln"*}{Log likelihood by observation}
#'  \item{*"bic"*: }{Bayesian Information Criterion}
#'  \item{*"link"*: }{Distribution used for count and selection models}
#'  \item{*"alpha"*: }{Dispersion parameter for negative binomial models}
#'  \item{*"sigma"*: }{Estimated standard deviation. See \link[stats]{sigma}}
#' }
#'
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
#'                           fm_zip5), tablelight::strip)
#'
#' cat(
#'   tablelight::light_table(object = model_list,
#'                         covariate.labels = c("x1","x2")),
#'   sep = "\n"
#' )
#' }
#'
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @export

light_table <- function(object,
                        type = c("latex","html"),
                        modeltype = "outcome",
                        title = "Title",
                        label = "label",
                        dep.var.labels = "Label dep.var.labels",
                        dep.var.separate = NULL,
                        column.labels = "blab",
                        covariate.labels = NULL,
                        order_variable = NULL,
                        stats.var.separate = NULL,
                        stats.list = c("n", "lln", "bic"),
                        notes = "notes to add",
                        add.lines = "",
                        reference_level_position = NULL,
                        rules_between_covariates = NULL,
                        omit = NULL,
                        landscape = FALSE,
                        adjustbox_width = c(NULL, 1.1),
                        visualize = FALSE,
                        ...){
  UseMethod("light_table")
}

#' @export
light_table.default <- function(
  object,
  type = c("latex","html"),
  modeltype = "outcome",
  title = "Title",
  label = "label",
  dep.var.labels = "Label dep.var.labels",
  dep.var.separate = NULL,
  column.labels = "blab",
  covariate.labels = NULL,
  order_variable = NULL,
  stats.var.separate = NULL,
  stats.list = c("n", "lln", "bic"),
  notes = "notes to add",
  add.lines = "",
  rules_between_covariates = NULL,
  reference_level_position = NULL,
  omit = NULL,
  landscape = FALSE,
  adjustbox_width = c(NULL, 1.1),
  visualize = FALSE,
  ...){

  type <- match.arg(type)

  if (missing(adjustbox_width)) adjustbox_width <- NULL

  if (isFALSE(inherits(object, "list")) && isFALSE(inherits(object, "nnet"))){
    ncols_models <- 1L
  } else if (isTRUE(inherits(object, "nnet"))){
    ncols_models <- length(object$lab[-1])
    if (isFALSE(is.null(reference_level_position))) ncols_models <- ncols_models + 1
  } else{
    ncols_models <- length(object)
  }

  if (identical(ncols_models, 1L)){
    coeff_data <- extract_coeff(object, type = type)
  } else if (isTRUE(inherits(object, "nnet"))){
    coeff_data <- extract_coeff(object, type = type)
  } else{
    coeff_data <- lapply(1:length(object),
                         function(k){
                           return(
                             extract_coeff(
                               object = object[[k]],
                               modeltype = modeltype[k],
                               type = type
                             )
                           )
                         })
  }





  # PART I : HEAD -------


  table_total <- light_table_header(
    ncols_models,
    type = type,
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
    type = type,
    coeff_data = coeff_data,
    order_variable = order_variable,
    omit = omit,
    covariate.labels = covariate.labels,
    reference_level_position = reference_level_position,
    rules_between_covariates = rules_between_covariates
  )

  table_total <- c(table_total, body_table)

  if (identical(type, "latex")){
    table_total <- c(table_total, "\\hline \\hline \\\\[-1.8ex] ")
  } else{
    table_total <- c(table_total,
                     sprintf("<tr><td colspan=\"%s\"style=\"border-bottom: 1px solid black\"></td></tr>", ncols_models + 1)
    )
  }



  # PART III: STATISTICS -----

  stats_table <- light_table_stats(object = object,
                                   type = type,
                                   ncols_models = ncols_models,
                                   stats.var.separate = stats.var.separate,
                                   stats.list = stats.list,
                                   ...)

  table_total <- c(table_total, stats_table)

  if (identical(type, "latex")) table_total <- c(table_total,
                                                 "\\hline ",
                                                 "\\hline \\\\[-1.8ex] ")


  # PART IV: FOOTER -----

  foot_table <- light_table_footer(
    ncols_models = ncols_models,
    type = type,
    add.lines = add.lines,
    adjustbox_width = adjustbox_width)

  table_total <- c(table_total, foot_table)


  # ARRANGE OUTPUT ------

  # Get one line by <tr> ... </tr> elements
  if (identical(type, "html")){
    table_total <- strsplit(paste(table_total, collapse = ""),
                            "</tr>")[[1]]
    table_total[1:(length(table_total)-1)] <- paste0(table_total[1:(length(table_total)-1)],
                                                     "</tr>")
  }


  if (landscape && identical(type, "latex")) table_total <- c("\\begin{landscape}", table_total, "\\end{landscape}")

  if (identical(type, "html") && isTRUE(visualize)) view_html(table_total)

  return(table_total)
}



