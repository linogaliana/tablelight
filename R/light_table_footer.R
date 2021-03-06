light_table_footer <- function(ncols_models, type = c("latex","html"),
                               add.lines,
                               tabletype = c("regression","summary"),
                               adjustbox_width){

  type <- match.arg(type)
  tabletype <- match.arg(tabletype)

  if (type == "latex"){
    return(
      light_table_footer_latex(ncols_models = ncols_models,
                               add.lines = add.lines,
                               adjustbox_width = adjustbox_width,
                               tabletype = tabletype)
    )
  } else{
    return(
      light_table_footer_html(ncols_models = ncols_models,
                               add.lines = add.lines)
    )
  }


}

light_table_footer_latex <- function(ncols_models, add.lines,
                               adjustbox_width, tabletype = c("regression","summary")){

  tabletype <- match.arg(tabletype)

  footnote <- ifelse(tabletype == "regression",
                     sprintf("\\multicolumn{%s}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}", ncols_models),
                     "")

  foot_table <- sprintf(
    "\\textit{Note:}  & %s \\\\ ",
    footnote
  )

  if (isFALSE(is.null(add.lines))){
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


  return(foot_table)
}



light_table_footer_html <- function(ncols_models, add.lines){

  if (is.null(add.lines)){
    text_add <- ""
  } else{
    text_add <- sprintf(
      paste0('<tr><td colspan="%s">',
      '</td></tr><tr><td style="text-align:left">%s</td><td></td><td></td></tr>'),
      ncols_models + 1, add.lines
    )
  }

  return(
    c(
      sprintf('<tr><td colspan="%s" style="border-bottom: 1px solid black"></td></tr>', ncols_models+1),
      paste0(sprintf('<tr><td style="text-align:left"><em>Note:</em></td><td colspan="%s" style="text-align:right">', ncols_models),
             '<sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>',
             text_add,
             "</table>")
    )
  )

}

