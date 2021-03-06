light_table_stats <- function(object, type = c("latex", "html"), ncols_models, stats.var.separate,
                              stats.list, stats.digits = 3L,
                              stats.add = NULL,
                              ...){

  type <- match.arg(type)

  if (inherits(object, "nnet")) stats.var.separate <- 1L #trick for nnet models

  if (type == "latex"){
    return(
      light_table_stats_latex(
        object = object, ncols_models = ncols_models,
        stats.var.separate = stats.var.separate,
        stats.list = stats.list,
        stats.digits = stats.digits,
        stats.add = stats.add,
        ...
      )
    )
  } else{
    return(
      light_table_stats_html(
        object = object, ncols_models = ncols_models,
        stats.var.separate = stats.var.separate,
        stats.list = stats.list,
        stats.digits = stats.digits,
        stats.add = stats.add,
        ...
      )
    )
  }

}

light_table_stats_latex <- function(object, ncols_models, stats.var.separate, stats.list,
                                    stats.digits = 3L,
                                    stats.add = NULL,
                                    ...){

  # COMPUTE STATISTICS -------------------

  if (isTRUE(ncols_models>1) && isFALSE(inherits(object, "nnet"))){
    statsdf <- lapply(object, liststats, stats.list = stats.list,
                      stats.digits = stats.digits,
                      ...)
    lapply(seq_along(statsdf), function(i) data.table::setnames(statsdf[[i]], old = "val",
                                                                new = paste0("val",i)))
    statsdf <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("stat","order"), all = TRUE),
                      statsdf)
  } else {
    statsdf <- liststats(object, stats.list = stats.list,
                         stats.digits = stats.digits,
                         ...)
  }

  statsdf <- statsdf[order(statsdf$order),]
  statsdf <- statsdf[, names(statsdf) != "order"]
  statsdf[, ] <- lapply(statsdf[, ], as.character)
  statsdf[is.na(statsdf)] <- ""


  # ARRANGE STATISTICS FOR MULTICOLUMN RENDERING --------

  if (!is.null(stats.var.separate)){

    if (isTRUE(length(stats.var.separate)==1)){

      statsdf2 <- sprintf(paste0(sprintf("\\multicolumn{%s}{c}", ncols_models),
                                 "{%s}"), statsdf[,2])
      statsdf <- cbind(statsdf[,1], statsdf2)

    } else{

      labels_stats <- rep("\\multicolumn{%s}{c}{%s}", length(stats.var.separate) + ncols_models - sum(stats.var.separate))
      length_labels <- c(stats.var.separate, ncols_models - sum(stats.var.separate))
      length_labels <- length_labels[length_labels>0]
      value_position <- 1 + cumsum(length_labels) - length_labels #Where to start from - number of models
      statsdf2 <- lapply(1:length(length_labels), function(i){
        sprintf(
          labels_stats[i],
          length_labels[i],
          statsdf[,1 + value_position[i]])
      }
      )
      statsdf <- cbind(statsdf[,1], do.call(cbind, statsdf2))

    }
  }


  statsdf <- apply(statsdf, 1, paste, collapse = " & ")

  # IF stats.add, concatenate with other stats
  if (!is.null(stats.add)){
    statsdf <- addendum_stats(statsdf, stats.add, type = "latex")
  }


  stats_table <- paste0(statsdf, " \\\\")

  stats_table <- gsub(pattern = "-", replacement = "$-$",
                      stats_table)

  return(stats_table)
}



light_table_stats_html <- function(object, ncols_models, stats.var.separate,
                                   stats.list,
                                   stats.digits = 3L,
                                   stats.add = NULL,
                                   ...){

  # COMPUTE STATISTICS -------------------

  if (isTRUE(ncols_models>1) && isFALSE(inherits(object, "nnet"))){

    statsdf <- lapply(object, liststats, stats.list = stats.list,
                      stats.digits = stats.digits,
                      ...)

    lapply(seq_along(statsdf), function(i) data.table::setnames(statsdf[[i]], old = "val",
                                                                new = paste0("val",i)))
    statsdf <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("stat","order"), all = TRUE),
                      statsdf)
  } else {
    statsdf <- liststats(object, stats.list = stats.list,
                         stats.digits = stats.digits,
                         ...)
  }

  statsdf <- statsdf[order(statsdf$order),]
  statsdf <- statsdf[, names(statsdf) != "order"]
  statsdf[, ] <- lapply(statsdf[, ], as.character)
  statsdf[is.na(statsdf)] <- ""


  # ARRANGE STATISTICS FOR MULTICOLUMN RENDERING --------

  if (!is.null(stats.var.separate)){

    if (isTRUE(length(stats.var.separate)==1)){

      statsdf2 <- sprintf(paste0(sprintf('<td colspan="%s">', ncols_models),
                                 "%s","</td>"), statsdf[,2])
      statsdf <- cbind(sprintf('<tr><td style="text-align:left">%s</td>', statsdf[,1]),
                       statsdf2, "</tr>")

    } else{

      labels_stats <- rep('<td colspan="%s">%s</td>', length(stats.var.separate) + ncols_models - sum(stats.var.separate))
      length_labels <- c(stats.var.separate, ncols_models - sum(stats.var.separate))
      length_labels <- length_labels[length_labels>0]
      value_position <- 1 + cumsum(length_labels) - length_labels #Where to start from - number of models
      statsdf2 <- lapply(1:length(length_labels), function(i){
        sprintf(
          labels_stats[i],
          length_labels[i],
          statsdf[,1 + value_position[i]])
      }
      )
      statsdf <- cbind(sprintf('<tr><td style="text-align:left">%s</td>', statsdf[,1]),
                       do.call(cbind, statsdf2),
                       "</tr>")
    }
  } else{
    statsdf <- cbind(sprintf('<tr><td style="text-align:left">%s</td>', statsdf[,1]),
                     do.call(cbind, lapply(2:ncol(statsdf), function(d) sprintf("<td>%s</td>", statsdf[,d]))),
                     "</tr>")
  }


  statsdf <- apply(statsdf, 1, paste, collapse = "")

  # IF stats.add, concatenate with other stats
  if (!is.null(stats.add)){
    statsdf <- addendum_stats(statsdf, stats.add, type = "html")
  }


  statsdf <- gsub("\\$\\\\alpha\\$", "&alpha;", statsdf, perl = TRUE)
  statsdf <- gsub("\\$R\\^2\\$", "R<sup>2</sup>", statsdf, perl = TRUE)
  statsdf <- gsub("\\$\\\\widehat\\{\\\\sigma\\}\\$", "&sigma;", statsdf, perl = TRUE)

  return(statsdf)
}



