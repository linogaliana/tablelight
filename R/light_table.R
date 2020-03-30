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


  ncols_models <- 1L

  coeff_data <- extract_coeff(object)


  # PART I : HEAD -------

  header <- sprintf("\\begin{table}[!htbp] \\centering
  \\caption{%s}
  \\label{%s}", title, label)


  tabular_header <- sprintf(
    "\\begin{tabular}{@{\\extracolsep{5pt}}l%s}
  \\\\[-1.8ex]\\hline
  \\hline \\\\[-1.8ex]",
    paste(rep("c",ncols_models), collapse = "")
  )


  if (!is.null(adjustbox_width)){
    table_total <- c(header,
                     sprintf("\\begin{adjustbox}{width=%s\\linewidth}", adjustbox_width),
                     tabular_header)
  } else{
    table_total <- c(header,tabular_header)
  }


  if (is.null(dep.var.separate) | (length(dep.var.labels)==1)){
    # in that case, we just put dep.var.labels
    depvar_header <- sprintf("
   & \\multicolumn{%s}{c}{\\textit{Dependent variable:}} \\\\
\\cline{2-%s}
\\\\[-1.8ex] & \\multicolumn{%s}{c}{%s} \\\\
  ", ncols_models, ncols_models+1,
                             ncols_models, dep.var.labels)

  } else{

    labels_depvar <- rep("\\multicolumn{%s}{c}{%s}", length(dep.var.separate) + 1)
    length_labels <- c(cumsum(dep.var.separate), ncols_models - sum(dep.var.separate))
    labels_depvar <- sapply(1:length(length_labels), function(i){
      sprintf(
        labels_depvar[i],
        length_labels[i],
        dep.var.labels[i])
    }
    )
    labels_depvar <- paste(labels_depvar, collapse = " & ")

    depvar_header <- sprintf("
   & \\multicolumn{%s}{c}{\\textit{Dependent variable:}} \\\\
\\cline{2-%s}
\\\\[-1.8ex] & %s \\\\
  ", ncols_models, ncols_models+1,  labels_depvar)

  }


  table_total <- c(table_total,depvar_header)

  if (!is.null(column.labels)){
    colvar_header <- paste(c("",column.labels[1:ncols_models]), collapse = " & ")
  } else{
    colvar_header <- ""
  }

  if (length(ncols_models)>1){
    colvar_header <- c(
      colvar_header,
      paste0(" \\\\[-1.8ex] & ",
             paste(paste0("(",seq_len(ncols_models), ")"), collapse = " & ")
      )
    )
  }

  colvar_header <- paste0(colvar_header, " \\\\")


  table_total <- c(table_total, colvar_header, "\\hline \\\\[-1.8ex] ")


  # PART II : BODY -------

  arrange_coeff <- function(text_coeff, order_variable = NULL){

    if (is.null(nrow(text_coeff))){
      text_coeff <- rbind(text_coeff, "")
    }

    text_coeff <- cbind(text_coeff, "text_zempty" = "")
    text_coeff <- data.frame(text_coeff)

    if (!('variable' %in% colnames(text_coeff))){
      text_coeff$variable <- rownames(text_coeff)
    }

    body_table <- reshape2::melt(
      text_coeff, id.vars = "variable",
      variable.name = "obj",
      factorsAsStrings=FALSE
    )

    list_variables_model <-  unique(body_table[,'variable'])

    if (!is.null(order_variable)){
      # we add variables not listed in order_variable
      order_variable <- c(order_variable[order_variable %in% list_variables_model],
                          as.character(list_variables_model)[!(list_variables_model %in% order_variable)]
      )
    }


    if (is.null(order_variable)){
      body_table <- body_table[order(body_table$variable,
                                     body_table$obj), ]
    } else{
      order_data <- data.frame(
        "variable" = order_variable,
        order = seq_len(length(order_variable))
      )
      body_table <- merge(body_table, order_data, by = "variable")
      body_table <- body_table[order(body_table$order,
                                     body_table$obj), ]
      body_table$order <- NULL
    }
    body_table$variable <- as.character(body_table$variable)

    body_table$value <- paste0(" & ", body_table$value)

    return(body_table)
  }

  if (ncols_models==1){
    coeff_body <- arrange_coeff(coeff_data, order_variable)
  } else{
    coeff_body <- lapply(coeff_data, arrange_coeff, order_variable)
    coeff_body <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("variable","obj"), all = TRUE),
                         coeff_body)
  }
  coeff_body <- na.omit(coeff_body)




  if (omit != ""){
    coeff_body <- coeff_body[!(coeff_body[,'variable'] %in% omit),]
  }

  coeff_body <- coeff_body[!(coeff_body[,'variable'] == "Log(theta)"),]


  if (is.null(order_variable)){
    if (ncols_models>1){
      order_variable <- unique(do.call(c, lapply(object, listcoeff)))
    } else{
      order_variable <- unique(listcoeff(object))
    }
  }


  order_data <- data.frame(
    "variable" = order_variable,
    order = seq_len(length(order_variable))
  )

  order_data <- na.omit(order_data)
  coeff_body <- merge(coeff_body, order_data, by = "variable",
                      all.x = TRUE)
  coeff_body <- coeff_body[order(coeff_body$order, coeff_body$obj), ]


  list_variables <- unique(coeff_body[,'variable'])

  if (ncols_models>1){
    rows_sd <- grep("\\(.*?\\)", coeff_body$value.x)
  } else{
    rows_sd <- grep("\\(.*?\\)", coeff_body$value)
  }

  coeff_body$variable[sort(c(rows_sd,rows_sd+1))] <- ""


  coeff_body <- coeff_body[,!(names(coeff_body) %in% c("obj","order"))]
  body_table <- apply(coeff_body, 1, paste, collapse="")

  body_table <- gsub(pattern = "-", replacement = "$-$",
                     body_table)


  # PUT CONSTANT IN LAST POSITION
  constant_idx <- which(coeff_body[,'variable'] == "(Intercept)")
  if (!is.null(constant_idx)){
    rows <- seq_len(nrow(coeff_body))
    rows <- rows[-constant_idx]
    coeff_body <- coeff_body[c(rows, constant_idx),]
  }

  body_table <- paste0(body_table, "\\\\")


  if (!is.null(covariate.labels)){
    n_replace <- min(length(list_variables), length(covariate.labels))
    labels_covariates <- covariate.labels[1:n_replace]
    value_covariates <- list_variables[list_variables != "(Intercept)"]
    names(labels_covariates) <- value_covariates[1:n_replace]
    body_table <-  qdap::mgsub(
      pattern = value_covariates,
      replacement = labels_covariates,
      body_table,
    )
  }

  if (!is.null(rules_between_covariates)){
    body_table[rules_between_covariates*3] <- paste0(body_table[rules_between_covariates*3], " \\hline \\\\[-1.8ex] ")
  }

  table_total <- c(table_total, body_table, "\\hline \\hline \\\\[-1.8ex] ")



  # PART III: STATISTICS -----

  # statsdf <- lapply(model_list, function(mod){
  #
  #   if (inherits(mod,"zeroinfl")){
  #     llk <- mod$loglik
  #     bic <- BIC(mod)
  #     link_count <- if (mod$dist == "negbin") "Negative Binomial" else "Poisson"
  #     link_selection <- Hmisc::capitalize(mod$link)
  #   } else{
  #     llk <- logLik(mod)
  #     k <- attributes(llk)$df
  #     bic <- -2*as.numeric(llk) + k*log(mod$n)
  #     llk <- as.numeric(llk)
  #     link_count <- ""
  #     link_selection <- ""
  #   }
  #
  #   df <- data.frame(
  #     stat = c(
  #       "Count distribution",
  #       "Selection distribution",
  #       "Observations",
  #       "Log likelihood",
  #       "Log likelihood (by obs.)",
  #       "Bayesian information criterion"),
  #     order = seq_len(6L),
  #     val = as.character(
  #       c(link_count,
  #         link_selection,
  #         format(mod$n, digits = 0,  big.mark=",", scientific = FALSE),
  #         format(llk, digits = 0, big.mark=",", scientific = FALSE),
  #         format(llk/mod$n, digits = 3L, nsmall = 3L, big.mark=",", scientific = FALSE),
  #         format(bic, digits = 0L, big.mark=",", scientific = FALSE)
  #       )
  #     )
  #   )
  #
  #   if ((inherits(mod,"zeroinfl") && mod$dist == "negbin") || (inherits(mod,"negbin"))){
  #
  #     df <- rbind(data.frame(stat = "$\\alpha$ (dispersion)",
  #                            order = 0,
  #                            val = as.character(
  #                              format(1/mod$theta, digits = 3L, nsmall = 3L))
  #     ), df)
  #   }
  #
  #   return(df)
  # })

  if (ncols_models>1){
    statsdf <- lapply(object, liststats, ...)
    statsdf <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("stat","order"), all = TRUE),
                      statsdf)
  } else{
    statsdf <- liststats(object, ...)
  }

  statsdf <- statsdf[order(statsdf$order),]
  statsdf <- statsdf[, names(statsdf) != "order"]
  statsdf[, ] <- lapply(statsdf[, ], as.character)
  statsdf[is.na(statsdf)] <- ""


  if (!is.null(stats.var.separate)){

    labels_stats <- rep("\\multicolumn{%s}{c}{%s}", length(stats.var.separate) + 1)
    length_labels <- c(cumsum(stats.var.separate), ncols_models - sum(stats.var.separate))
    # length_labels <- length_labels[length_labels>0]
    statsdf2 <- lapply(1:length(length_labels), function(i){
      sprintf(
        labels_stats[i],
        length_labels[i],
        statsdf[,1 + 2*i])
    }
    )
    statsdf <- cbind(statsdf[,1], do.call(cbind, statsdf2))

  }

  statsdf <- apply(statsdf, 1, paste, collapse = " & ")
  stats_table <- paste0(statsdf, " \\\\")

  stats_table <- gsub(pattern = "-", replacement = "$-$",
                      stats_table)


  table_total <- c(table_total, stats_table,
                   "\\hline ",
                   "\\hline \\\\[-1.8ex] ")


  # PART IV: FOOTER -----

  foot_table <- sprintf(
    "\\textit{Note:}  & \\multicolumn{%s}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\ ",
    ncols_models
  )

  if (add.lines != ""){
    foot_table <- c(foot_table,
                    sprintf(
                      " \\multicolumn{%s}{p{0.9\\linewidth}}{%s} \\\\ ",
                      ncols_models+1,
                      add.lines
                    ))
  }

  if (!is.null(adjustbox_width)){
    foot_table <- c(foot_table, "\\end{tabular} ",
                    "\\end{adjustbox} ",
                    "\\end{table} ")
  } else{
    foot_table <- c(foot_table, "\\end{tabular} ", "\\end{table} ")
  }



  table_total <- c(table_total, foot_table)

  if (landscape) table_total <- c("\\begin{landscape}", table_total, "\\end{landscape}")

  return(table_total)
}




#' @export
light_table.list <- function(
  object,
  modeltype = list("outcome", length(object)),
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

  coeff_data <- lapply(1:length(object),
                       function(k){
                         return(
                           extract_coeff(
                             object = object[[k]],
                             modeltype = modeltype[k]
                           )
                         )
                       })


  # PART I : HEAD -------

  header <- sprintf("\\begin{table}[!htbp] \\centering
  \\caption{%s}
  \\label{%s}", title, label)


  tabular_header <- sprintf(
    "\\begin{tabular}{@{\\extracolsep{5pt}}l%s}
  \\\\[-1.8ex]\\hline
  \\hline \\\\[-1.8ex]",
    paste(rep("c",ncols_models), collapse = "")
  )


  if (!is.null(adjustbox_width)){
    table_total <- c(header,
                     sprintf("\\begin{adjustbox}{width=%s\\linewidth}", adjustbox_width),
                     tabular_header)
  } else{
    table_total <- c(header,tabular_header)
  }



  if (is.null(dep.var.separate) | (length(dep.var.labels)==1)){
    # in that case, we just put dep.var.labels
    depvar_header <- sprintf("
   & \\multicolumn{%s}{c}{\\textit{Dependent variable:}} \\\\
\\cline{2-%s}
\\\\[-1.8ex] & \\multicolumn{%s}{c}{%s} \\\\
  ", ncols_models, ncols_models+1,
                             ncols_models, dep.var.labels)

  } else{

    labels_depvar <- rep("\\multicolumn{%s}{c}{%s}", length(dep.var.separate) + 1)
    length_labels <- c(cumsum(dep.var.separate), ncols_models - sum(dep.var.separate))
    labels_depvar <- sapply(1:length(length_labels), function(i){
      sprintf(
        labels_depvar[i],
        length_labels[i],
        dep.var.labels[i])
    }
    )
    labels_depvar <- paste(labels_depvar, collapse = " & ")

    depvar_header <- sprintf("
   & \\multicolumn{%s}{c}{\\textit{Dependent variable:}} \\\\
\\cline{2-%s}
\\\\[-1.8ex] & %s \\\\
  ", ncols_models, ncols_models+1,  labels_depvar)

  }


  table_total <- c(table_total,depvar_header)

  if (!is.null(column.labels)){
    colvar_header <- paste(c("",column.labels[1:ncols_models]), collapse = " & ")
  } else{
    colvar_header <- ""
  }

  if (length(ncols_models)>1){
    colvar_header <- c(
      colvar_header,
      paste0(" \\\\[-1.8ex] & ",
             paste(paste0("(",seq_len(ncols_models), ")"), collapse = " & ")
      )
    )
  }

  colvar_header <- paste0(colvar_header, " \\\\")


  table_total <- c(table_total, colvar_header, "\\hline \\\\[-1.8ex] ")


  # PART II : BODY -------

  arrange_coeff <- function(text_coeff, order_variable = NULL){

    namescol <- colnames(text_coeff)
    text_coeff <- data.frame(text_coeff)
    colnames(text_coeff) <- namescol


    if (is.null(nrow(text_coeff))){
      text_coeff <- rbind(text_coeff, "")
    }


    text_coeff <- cbind(text_coeff, "text_zempty" = "")


    if (!('variable' %in% colnames(text_coeff))){
      text_coeff$variable <- rownames(text_coeff)
    }


    body_table <- reshape2::melt(
      text_coeff, id.vars = "variable",
      variable.name = "obj",
      factorsAsStrings=FALSE
    )

    list_variables_model <-  unique(body_table[,'variable'])

    if (!is.null(order_variable)){
      # we add variables not listed in order_variable
      order_variable <- c(order_variable[order_variable %in% list_variables_model],
                          as.character(list_variables_model)[!(list_variables_model %in% order_variable)]
      )
    }


    if (is.null(order_variable)){
      body_table <- body_table[order(body_table$variable,
                                     body_table$obj), ]
    } else{
      order_data <- data.frame(
        "variable" = order_variable,
        order = seq_len(length(order_variable))
      )
      body_table <- merge(body_table, order_data, by = "variable")
      body_table <- body_table[order(body_table$order,
                                     body_table$obj), ]
      body_table$order <- NULL
    }
    body_table$variable <- as.character(body_table$variable)

    body_table$value <- paste0(" & ", body_table$value)

    return(body_table)
  }

  coeff_body <- lapply(coeff_data, arrange_coeff, order_variable)
  coeff_body <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("variable","obj"), all = TRUE),
                       coeff_body)
  coeff_body <- na.omit(coeff_body)




  if (omit != ""){
    coeff_body <- coeff_body[!(coeff_body[,'variable'] %in% omit),]
  }

  coeff_body <- coeff_body[!(coeff_body[,'variable'] == "Log(theta)"),]


  if (is.null(order_variable)){
    if (ncols_models>1){
      order_variable <- unique(do.call(c, lapply(object, listcoeff)))
    } else{
      order_variable <- unique(listcoeff(object))
    }
  }


  order_data <- data.frame(
    "variable" = order_variable,
    order = seq_len(length(order_variable))
  )

  order_data <- na.omit(order_data)
  coeff_body <- merge(coeff_body, order_data, by = "variable",
                      all.x = TRUE)
  coeff_body <- coeff_body[order(coeff_body$order, coeff_body$obj), ]


  list_variables <- unique(coeff_body[,'variable'])

  rows_sd <- grep("\\(.*?\\)", coeff_body$value.x)

  coeff_body$variable[sort(c(rows_sd,rows_sd+1))] <- ""


  coeff_body <- coeff_body[,!(names(coeff_body) %in% c("obj","order"))]
  body_table <- apply(coeff_body, 1, paste, collapse="")

  body_table <- gsub(pattern = "-", replacement = "$-$",
                     body_table)


  # PUT CONSTANT IN LAST POSITION
  constant_idx <- which(coeff_body[,'variable'] == "(Intercept)")
  if (!is.null(constant_idx)){
    rows <- seq_len(nrow(coeff_body))
    rows <- rows[-constant_idx]
    coeff_body <- coeff_body[c(rows, constant_idx),]
  }

  body_table <- paste0(body_table, "\\\\")


  if (!is.null(covariate.labels)){
    n_replace <- min(length(list_variables), length(covariate.labels))
    labels_covariates <- covariate.labels[1:n_replace]
    value_covariates <- list_variables[list_variables != "(Intercept)"]
    names(labels_covariates) <- value_covariates[1:n_replace]
    body_table <-  qdap::mgsub(
      pattern = value_covariates,
      replacement = labels_covariates,
      body_table,
    )
  }

  if (!is.null(rules_between_covariates)){
    body_table[rules_between_covariates*3] <- paste0(body_table[rules_between_covariates*3], " \\hline \\\\[-1.8ex] ")
  }

  table_total <- c(table_total, body_table, "\\hline \\hline \\\\[-1.8ex] ")



  # PART III: STATISTICS -----

  statsdf <- lapply(object, liststats, ...)
  statsdf <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("stat","order"), all = TRUE),
                    statsdf)

  statsdf <- statsdf[order(statsdf$order),]
  statsdf <- statsdf[, names(statsdf) != "order"]
  statsdf[, ] <- lapply(statsdf[, ], as.character)
  statsdf[is.na(statsdf)] <- ""


  if (!is.null(stats.var.separate)){

    labels_stats <- rep("\\multicolumn{%s}{c}{%s}", length(stats.var.separate) + 1)
    length_labels <- c(cumsum(stats.var.separate), ncols_models - sum(stats.var.separate))
    # length_labels <- length_labels[length_labels>0]
    statsdf2 <- lapply(1:length(length_labels), function(i){
      sprintf(
        labels_stats[i],
        length_labels[i],
        statsdf[,1 + 2*i])
    }
    )
    statsdf <- cbind(statsdf[,1], do.call(cbind, statsdf2))

  }

  statsdf <- apply(statsdf, 1, paste, collapse = " & ")
  stats_table <- paste0(statsdf, " \\\\")

  stats_table <- gsub(pattern = "-", replacement = "$-$",
                      stats_table)


  table_total <- c(table_total, stats_table,
                   "\\hline ",
                   "\\hline \\\\[-1.8ex] ")


  # PART IV: FOOTER -----

  foot_table <- sprintf(
    "\\textit{Note:}  & \\multicolumn{%s}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\ ",
    ncols_models
  )

  if (add.lines != ""){
    foot_table <- c(foot_table,
                    sprintf(
                      " \\multicolumn{%s}{p{0.9\\linewidth}}{%s} \\\\ ",
                      ncols_models+1,
                      add.lines
                    ))
  }


  if (!is.null(adjustbox_width)){
    foot_table <- c(foot_table, "\\end{tabular} ",
                    "\\end{adjustbox} ",
                    "\\end{table} ")
  } else{
    foot_table <- c(foot_table, "\\end{tabular} ", "\\end{table} ")
  }


  table_total <- c(table_total, foot_table)

  if (landscape) table_total <- c("\\begin{landscape}", table_total, "\\end{landscape}")

  return(table_total)
}
