#' Header for the regression table
#' @inheritParams light_table
#' @param ncols_models Number of columns
#' @inheritParams light_table

light_table_header <- function(ncols_models,
                               type = c("latex","html"),
                               title = "Title",
                               label = "label",
                               dep.var.labels = "Label dep.var.labels",
                               dep.var.separate = NULL,
                               column.labels = "blab",
                               adjustbox_width = c(NULL, 1.1)){

  type <- match.arg(type)

  if (type == "latex"){
    return(
    light_table_header_latex(ncols_models = ncols_models,
                             title = title,
                             label = label,
                             dep.var.labels = dep.var.labels,
                             dep.var.separate = dep.var.separate,
                             column.labels = column.labels,
                             adjustbox_width = adjustbox_width)
  )
  } else{

    return(
      light_table_header_html(ncols_models,
                                        title = title,
                                        label = label,
                                        dep.var.labels = dep.var.labels,
                                        dep.var.separate = dep.var.separate,
                                        column.labels = column.labels)
    )

  }

}

light_table_header_latex <- function(ncols_models,
                               title = "Title",
                               label = "label",
                               dep.var.labels = "Label dep.var.labels",
                               dep.var.separate = NULL,
                               column.labels = "blab",
                               adjustbox_width = c(NULL, 1.1)){

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
                     sprintf("\\begin{adjustbox}{width=%s\\linewidth, center}", adjustbox_width),
                     tabular_header)
  } else{
    table_total <- c(header,tabular_header)
  }


  if ((ncols_models == 1) || is.null(dep.var.separate) || (length(dep.var.labels)==1)){
    # in that case, we just put dep.var.labels
    depvar_header <- sprintf("
   & \\multicolumn{%s}{c}{\\textit{Dependent variable:}} \\\\
\\cline{2-%s}
\\\\[-1.8ex] & \\multicolumn{%s}{c}{%s} \\\\
  ", ncols_models, ncols_models+1,
                             ncols_models, dep.var.labels[1])

  } else{

    labels_depvar <- rep("\\multicolumn{%s}{c}{%s}", length(dep.var.separate) - - sum(dep.var.separate))
    length_labels <- c(dep.var.separate, ncols_models - sum(dep.var.separate))
    length_labels <- length_labels[length_labels>0]
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


  return(table_total)

}



light_table_header_html <- function(ncols_models,
                                    title = "Title",
                                    dep.var.labels = "Label dep.var.labels",
                                    dep.var.separate = NULL,
                                    column.labels = "blab",
                                    adjustbox_width = c(NULL, 1.1),
                                    ...){


  header <- c(
    '<table style="text-align:center"><tr>',
    sprintf('<caption>%s</caption>', title),
    sprintf('<td colspan="%s"', ncols_models+1)
  )

  tabular_header <- c(
    'style="border-bottom: 1px solid black"></td></tr>',
    '<tr><td style="text-align:left"></td><td>',
    '<em>Dependent variable:</em></td></tr>',
    sprintf('<tr><td></td><td colspan="%s"', ncols_models),
    'style="border-bottom: 1px solid black"></td></tr>'
  )


  table_total <- c(header,tabular_header)


  if ((ncols_models == 1) || is.null(dep.var.separate) || (length(dep.var.labels)==1)){

    depvar_header <- c(
      sprintf(
        '<tr><td style="text-align:left"></td><td colspan="%s">%s</td></tr>',
        ncols_models,
        dep.var.labels[1]
      )
    )

  } else{

    labels_depvar <- rep('<td colspan="%s">%s</td>', length(dep.var.separate) + 1)
    length_labels <- c(dep.var.separate, ncols_models - sum(dep.var.separate))
    length_labels <- length_labels[length_labels>0]
    labels_depvar <- sapply(1:length(length_labels), function(i){
      sprintf(
        labels_depvar[i],
        length_labels[i],
        dep.var.labels[i])
    }
    )
    depvar_header <- paste(c("<td></td>",labels_depvar), collapse = "")


  }

  table_total <- c(table_total,depvar_header)



  if (!is.null(column.labels)){
    colvar_header <- paste(
      c('<tr><td style="text-align:left"></td>',
        paste(sprintf("<td>%s</td>", column.labels), collapse = ""),
        '</tr>'), collapse = "")
  } else{
    colvar_header <- ""
  }

  table_total <- c(table_total, colvar_header)

  table_total <- c(
    table_total,
    sprintf('<tr><td colspan="%s" style="border-bottom: 1px solid black"></td></tr>', ncols_models+1)
  )

  return(table_total)

}
