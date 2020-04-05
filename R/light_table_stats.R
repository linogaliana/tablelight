light_table_stats <- function(object, ncols_models, stats.var.separate, ...){

  # COMPUTE STATISTICS -------------------

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


  # ARRANGE STATISTICS FOR MULTICOLUMN RENDERING --------

  if (!is.null(stats.var.separate)){

    if (isTRUE(length(stats.var.separate)==1)){

      statsdf2 <- sprintf(paste0(sprintf("\\multicolumn{%s}{c}", ncols_models),
                          "{%s}"), statsdf[,2])
      statsdf <- cbind(statsdf[,1], statsdf2)

    } else{

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
  }


  statsdf <- apply(statsdf, 1, paste, collapse = " & ")
  stats_table <- paste0(statsdf, " \\\\")

  stats_table <- gsub(pattern = "-", replacement = "$-$",
                      stats_table)

  return(stats_table)
}
