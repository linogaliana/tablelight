stack_summary <- function(object, x_vars, type = c("latex","html", "md", "dataframe"), weight_vars = NULL, by = NULL,
                          caption = "", label = "",
                          multirow_labels = NULL, add_rules = FALSE, add.lines = NULL,
                          stats = c("min","1Q","median","mean","3Q","max"),
                          ...){

  type <- match.arg(type)

  if (type %in% c("html","md")) stop("For the moment, only 'tex' and 'dataframe' options implemented")

  if (isFALSE(is.null(weight_vars)) && isTRUE(length(weight_vars) < length(object))){
    weight_vars <- rep(weight_vars, times = length(object))
  }
  if (isFALSE(is.null(x_vars)) && isTRUE(length(x_vars) < length(object))){
    x_vars <- rep(x_vars, times = length(object))
  }

  if (inherits(object, "data.frame")) object <- list(EP_2015)


  # COMPUTE SUMMARY STATS -------------------

  statsagg <- lapply(seq_along(object), function(i){
    summary_(data = object[[i]], xvar = x_vars[i],
             weight_var = weight_vars[i],
             by_var = by[i],
             stats = stats)
  })

  ncols <- lapply(statsagg, function(x) ncol(x))
  ncols <- max(do.call(rbind, ncols))


  #statsagg <- data.table::rbindlist(statsagg, use.names = TRUE, fill = TRUE)

  if (type == "dataframe") return(statsagg)

  # TABLE HEADER --------------

  header <- c("\\begin{table}[!h]",
              "\\centering",
              sprintf("\\caption{%s\\label{%s}}",
                      caption, label)
  )

  header <- c(header,
              sprintf("\\begin{tabular}{%s}",
                      paste(rep("c", ncols), collapse = "")
              ),
              "\\toprule"
  )


  # TABLE CONTENT ---------------

  colnames_part <- paste0(
    paste(colnames(statsagg[[1]]),
          collapse = " & "),
    "\\\\")

  if (!is.null(by))  colnames_part <- paste0(" & ", colnames_part)


  content <- lapply(seq_along(statsagg), function(i){
    tab <- as.character(lapply(seq_len(nrow(statsagg[[i]])),
                               function(n){
                                 xx <- paste(statsagg[[i]][n,], collapse = " & ")
                                 prefix <- paste(rep(" & ", times = ncols - ncol(statsagg[[i]])), collapse = "")
                                 return(paste0(prefix, xx))
                               })
    )
    tab <- paste0(tab, " \\\\")
    if (isFALSE(is.null(multirow_labels[i])) && isFALSE(is.na(multirow_labels[i]))){
      tab <- c(sprintf("\\multicolumn{%s}{c}{\\textsc{%s}} \\\\", ncols, multirow_labels[i]),
               tab)
    }
    if (isTRUE(add_rules)) tab <- c(tab, "\\midrule")
    return(tab)
  })

  content <- c(
    colnames_part,
    "\\midrule",
    as.character(do.call(c, content))
  )


  # TABLE FOOTER -----------------

  footer <- c("\\bottomrule",
              "\\end{tabular}",
              "\\end{table}")


  # COMBINE EVERYTHING TOGETHER

  if (is.null(add.lines)){
    table <- c(header, content, footer)
  } else{
    foot_table <- light_table_footer(
      ncols_models = ncols - 1L,
      type = type,
      add.lines = add.lines,
      adjustbox_width = NULL,
      tabletype = "summary")
    table <- c(header, content,"\\bottomrule", foot_table)
  }




  return(
    table
  )

}





