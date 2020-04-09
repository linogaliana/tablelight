testthat::context("[OLS] light_table produces the expected table")


# SINGLE OLS MODEL ----------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

latex_table <- tablelight::light_table(ols,
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column",
                                       stats.list = c("n","ll","lln","bic"))

html_table <- tablelight::light_table(ols,
                                      type = "html",
                                      title = "My table title",
                                      dep.var.labels = "My depvar",
                                      column.labels = "My label column",
                                      stats.list = c("n","ll","lln","bic"))


# HEADER =========================

testthat::test_that("Table header is correct [latex]",{

  testthat::expect_true(startsWith(prefix = "\\begin{table}[!htbp]", latex_table[1])
  )
  testthat::expect_true(sum(grepl("\\\\caption{My table title}", perl = TRUE,
                                  x = latex_table))>0)
  testthat::expect_true(sum(grepl("\\\\label{My table label}", perl = TRUE,
                                  x = latex_table))>0)

  testthat::expect_true(sum(grepl("My depvar", perl = TRUE,
                                  x = latex_table))>0)

  testthat::expect_true(sum(grepl("& My label column", perl = TRUE,
                                  x = latex_table))>0)

}
)

testthat::test_that("Table header is correct [html]",{

  testthat::expect_true(startsWith(prefix = "<table style=\"text-align:center\"><tr>", html_table[1])
  )
  testthat::expect_true(grepl(sprintf("<td colspan=\"%s\"style=\"border-bottom: 1px solid black\"></td>",2L),
                              html_table[1]))
  testthat::expect_true(grepl(sprintf("<caption>%s</caption>","My table title"),
                              html_table[1]))

}
)

testthat::test_that("Label is ignored in HTML tables",{
  testthat::expect_equal(
    tablelight::light_table(ols,
                            type = "html",
                            title = "My table title",
                            label = "My table label",
                            dep.var.labels = "My depvar",
                            column.labels = "My label column"),
    tablelight::light_table(ols,
                            type = "html",
                            title = "My table title",
                            label = "Label ignored",
                            dep.var.labels = "My depvar",
                            column.labels = "My label column")
  )
}
)


# BODY ===================================

testthat::test_that("Body (coefficients) correct [latex]",{

  rows_coeff <- latex_table[
    which(
      grepl(paste(c("(Intercept)", "Sepal.Width"), collapse = "|"), latex_table)
    )
    ]

  rows_sd <- latex_table[
    which(
      grepl(paste(c("(Intercept)", "Sepal.Width"), collapse = "|"), latex_table)
    )+1
    ]

  # Coefficient names are ok [intercept]
  testthat::expect_equal(
    trimws(
      as.character(
        unlist(stringr::str_split(rows_coeff[1], "&"))
      )[1]
    ),
    "(Intercept)"
  )

  # Coefficient names are ok [x covariate]
  testthat::expect_equal(
    trimws(
      as.character(
        unlist(stringr::str_split(rows_coeff[2], "&"))
      )[1]
    ),
    "Sepal.Width"
  )

  sd <- summary(ols)$coefficients[,"Pr(>|t|)"]

  # Coefficient s.e. are ok [intercept]
  testthat::expect_equal(
    trimws(
      as.character(
        unlist(stringr::str_split(rows_coeff[1], "&"))
      )[2]),
    paste0(
      format(ols$coefficients['(Intercept)'],digits = 3L, nsmall = 3L),
      tablelight:::signif_stars(sd['(Intercept)']),
      "\\\\")
  )

  # Coefficient names are ok [x covariates]
  testthat::expect_equal(
    trimws(
      as.character(
        unlist(stringr::str_split(rows_coeff[2], "&"))
      )[2]),
    gsub(x = paste0(
      format(ols$coefficients['Sepal.Width'],digits = 3L, nsmall = 3L),
      tablelight:::signif_stars(sd['Sepal.Width']),
      "\\\\"), pattern = "-", replacement = "$-$"
    )
  )


})


testthat::test_that("Body (coefficients) correct [latex]",{

  coeff_part <- html_table[
    grepl(paste(c("(Intercept)", "Sepal.Width"), collapse = "|"), html_table)
    ]

  coeff_part <- paste0(as.character(
    stringr::str_split(coeff_part, pattern = "</td>", simplify = TRUE)
  ), "</td>")

  rows_coeff <- coeff_part[
    c(grep(paste(c("(Intercept)", "Sepal.Width"), collapse = "|"), coeff_part),
      grep(paste(c("(Intercept)", "Sepal.Width"), collapse = "|"), coeff_part) + 1)
    ]


  rows_sd <- coeff_part[
    grep(paste(c("(Intercept)", "Sepal.Width"), collapse = "|"), coeff_part) + 3
    ]

  # Coefficient names are ok [intercept]
  testthat::expect_equal(
    grep("(Intercept)", rows_coeff),
    1L
  )

  # Coefficient names are ok [x covariate]
  testthat::expect_equal(
    grep("Sepal.Width", rows_coeff),
    2L
  )

  sd <- summary(ols)$coefficients[,"Std. Error"]
  pvalues <- summary(ols)$coefficients[,"Pr(>|t|)"]

  # Coefficients are ok
  testthat::expect_equal(
    rows_coeff[grep("Sepal.Width", rows_coeff)+2],
    paste0(
      "<td>",
      format(ols$coefficients['Sepal.Width'],digits = 3L, nsmall = 3L),
      tablelight:::signif_stars(pvalues['Sepal.Width'], type = "html"),
      "</td>")
  )
  testthat::expect_equal(
    rows_coeff[grep("(Intercept)", rows_coeff)+2],
    paste0(
      "<td>",
      format(ols$coefficients['(Intercept)'],digits = 3L, nsmall = 3L),
      tablelight:::signif_stars(pvalues['(Intercept)'], type = "html"),
      "</td>")
  )

  # standard errors are ok
  testthat::expect_equal(
    paste0("<td>(", format(sd, digits = 3L, nsmall = 3L), ")</td>"),
    rows_sd
  )

})






## 1.2. SEVERAL MODELS =================



ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

latex_table <- tablelight::light_table(list(ols, ols, ols),
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = c("My depvar1", "My depvar2"),
                                       dep.var.separate = c(2,1),
                                       column.labels = c("Label1","Label2"),
                                       stats.list = c("n","ll","lln","bic"))

html_table <- tablelight::light_table(list(ols, ols, ols),
                                      type = "html",
                                      title = "My table title",
                                      dep.var.labels = c("My depvar1", "My depvar2","toomanyvar"),
                                      dep.var.separate = c(2,1),
                                      column.labels = c("Label1","Label2"),
                                      stats.list = c("n","ll","lln","bic"))


# HEADER

testthat::test_that("[latex] Number of columns: number of models + 1", {
  tabular_row = stringr::str_split(pattern = "\n",
                                   latex_table[grepl("\\\\begin{tabular}", latex_table, perl = TRUE)], simplify = TRUE)[1,1]
  testthat::expect_equal(tabular_row, sprintf("\\begin{tabular}{@{\\extracolsep{5pt}}l%s}",
                                              paste(rep("c",3L), collapse = "")))
})

testthat::test_that("[html] Number of columns: number of models + 1", {

  testthat::expect_true(
    grepl(sprintf("<td colspan=\"%s\"style=\"border-bottom: 1px solid black\"></td></tr>", 3L + 1L),
          html_table[1]
    )
  )

})


testthat::test_that("[html] Number of columns: number of models + 1", {

  dep_var_row <- html_table[grepl("<td></td><td colspan=\"2\">My depvar1</td><td colspan=\"1\">My depvar2</td>",
                                  html_table)]
  dep_var_row <- paste0(as.character(
    stringr::str_split(dep_var_row, "</td>", simplify = TRUE)
  ), "</td>")

  # 1 - First model supposed to be on two columns
  testthat::expect_equal(
    sum(grepl("<td colspan=\"2\">My depvar1</td>", dep_var_row)),
    1L
  )

  # 2 - Second model supposed to be on two columns
  testthat::expect_equal(
    sum(grepl("<td colspan=\"1\">My depvar2</td>", dep_var_row)),
    1L
  )

  # 3 - Last dep.var name does not appear (because of col.var.separate)
  testthat::expect_equal(sum(grepl("toomanyvar", dep_var_row)), 0L)


  # 4 - Column names Label1, Label2 and empty field (no column label!)
  testthat::expect_equal(sum(grepl("<td>Label1</td>", dep_var_row)), 1L)
  testthat::expect_equal(sum(grepl("<td>Label2</td>", dep_var_row)), 1L)
  testthat::expect_true(grepl('</tr>',
                              dep_var_row[grep("<td>Label2</td>", dep_var_row)+1])
  )

})



# OGLMX ---------

requireNamespace("oglmx", quietly = TRUE)

iris$y_r <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y_r ~ Sepal.Length ,
  data = iris
)

testthat::expect_warning(
  x <- tablelight::light_table(oglm,
                               modeltype = "outcome"),
  "attributes are not identical across measure variables"
)


testthat::expect_equal(
  class(x),
  "character"
)

testthat::expect_warning(
  x2 <- tablelight::light_table(list(oglm, oglm),
                                modeltype = c("outcome","outcome")),
  "attributes are not identical across measure variables"
)

testthat::expect_equal(
  class(x2),
  "character"
)




# PART 2: TEST OPTIONS -------------------






# covariate.labels =================

ols <- lm(
  Sepal.Length ~ Sepal.Width + Petal.Length,
  data = iris
)

latex_table1 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column")

latex_table2 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column",
                                        covariate.labels = c("Sepal Width", "Length Petal"))

testthat::test_that("Same table except for the covariate names", {
  latex_table1b <- gsub(pattern = "Sepal.Width", replacement = "Sepal Width", latex_table1)
  latex_table1b <- gsub(pattern = "Petal.Length", replacement = "Length Petal", latex_table1b)
  testthat::expect_equal(trimws(latex_table1b),trimws(latex_table2))
})


html_table1 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column")

html_table2 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column",
                                       covariate.labels = c("Sepal Width", "Length Petal"))


testthat::test_that("Same table except for the covariate names", {
  html_table1b <- gsub(pattern = "Sepal.Width", replacement = "Sepal Width", html_table1)
  html_table1b <- gsub(pattern = "Petal.Length", replacement = "Length Petal", html_table1b)
  testthat::expect_equal(trimws(html_table1b),trimws(html_table2))
})


# adjustbox ==================

latex_table <- tablelight::light_table(ols,
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column",
                                       adjustbox_width = 1.1)


testthat::test_that("Table header and foot are correct",{

  testthat::expect_true(startsWith(prefix = "\\begin{table}[!htbp]", latex_table[1])
  )
  testthat::expect_true(sum(grepl("\\\\caption{My table title}", perl = TRUE,
                                  x = latex_table))>0)

  testthat::expect_true(sum(grepl("\\\\begin{adjustbox}{width=1.1\\\\linewidth}", perl = TRUE,
                                  x = latex_table))>0)


  testthat::expect_true(sum(grepl("\\\\label{My table label}", perl = TRUE,
                                  x = latex_table))>0)

  testthat::expect_true(sum(grepl("My depvar", perl = TRUE,
                                  x = latex_table))>0)

  testthat::expect_true(sum(grepl("& My label column", perl = TRUE,
                                  x = latex_table))>0)

  testthat::expect_true(sum(grepl("\\\\end{adjustbox} ", perl = TRUE,
                                  x = latex_table))>0)


}
)


# stats.var.separate (when length 1) ==========

data("bioChemists", package = "pscl")
fm_zip <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                           dist = "negbin"))

latex_table <- light_table(list(fm_zip, fm_zip), modeltype = c("selection","outcome"),
                           dep.var.labels = c("Selection","Outcome"),
                           stats.var.separate = 2L,
                           stats.list = c("n","ll","lln","bic","alpha","link"))

html_table <- light_table(list(fm_zip, fm_zip), modeltype = c("selection","outcome"),
                          dep.var.labels = c("Selection","Outcome"),
                          stats.var.separate = 2L,
                          stats.list = c("n","ll","lln","bic","alpha","link"),
                          type = "html")


testthat::test_that("Summary statistics on two columns",{

  # ALPHA STATISTICS
  alpha_row <- stringr::str_split(
    latex_table[grepl("\\\\alpha", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(alpha_row), 2L)
  testthat::expect_equal(alpha_row[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 format(1/exp(fm_zip$coefficients$count['Log(theta)','Estimate']),
                                                        digits = 3L, nsmall = 3L)))

  # COUNT DISTRIBUTION
  count_row <- stringr::str_split(
    latex_table[grepl("Count distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(count_row), 2L)
  testthat::expect_equal(count_row[,2L], sprintf(" \\multicolumn{2}{c}{Negative Binomial} \\\\"))

  # LINK DISTRIBUTION
  link_row <- stringr::str_split(
    latex_table[grepl("Selection distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_row), 2L)
  testthat::expect_equal(link_row[,2L], sprintf(" \\multicolumn{2}{c}{Logit} \\\\"))

  # OBSERVATIONS
  link_obs <- stringr::str_split(
    latex_table[grepl("Observations", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_obs), 2L)
  testthat::expect_equal(link_obs[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                format(fm_zip$n, big.mark = ",")))


  # OBSERVATIONS
  link_llk <- stringr::str_split(
    latex_table[grepl("Log likelihood &", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk), 2L)
  testthat::expect_equal(link_llk[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zip$loglik, big.mark = ",", digits = 0L)))
  )

  # OBSERVATIONS (BY OBS)
  link_llk2 <- stringr::str_split(
    latex_table[grepl("Log likelihood \\(by obs.\\)", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk2), 2L)
  testthat::expect_equal(link_llk2[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 gsub("-","$-$",
                                                      format(fm_zip$loglik/fm_zip$n,
                                                             big.mark = ",", digits = 3L,
                                                             nsmall = 3)))
  )

  # BIC
  link_bic <- stringr::str_split(
    latex_table[grepl("Bayesian information criterion", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_bic), 2L)
  testthat::expect_equal(link_bic[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zip$bic, big.mark = ",", digits = 0L)))
  )


})



testthat::test_that("Summary statistics on two columns",{

  # ALPHA STATISTICS
  alpha_row <- html_table[grepl("alpha", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", alpha_row))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(1/exp(fm_zip$coefficients$count['Log(theta)','Estimate']),
                     digits = 3L, nsmall = 3L)
      ), alpha_row)
  )

  # COUNT DISTRIBUTION
  count_row <- html_table[grepl("Count distribution", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", count_row))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              "Negative Binomial"
      ), count_row)
  )

  # LINK DISTRIBUTION
  link_row <- html_table[grepl("Selection distribution", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_row))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              "Logit"
      ), link_row)
  )


  # OBSERVATIONS
  link_obs <- html_table[grepl("Observations", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_obs))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(fm_zip$n,big.mark = ",")
      ), link_obs)
  )


  # OBSERVATIONS (BY OBS)
  link_llk2 <- html_table[grepl("Log likelihood \\(by obs\\.\\)", html_table, perl = TRUE)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_llk2))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(fm_zip$loglik/fm_zip$n,
                     big.mark = ",", digits = 3L,
                     nsmall = 3)), link_llk2
    )
  )

  # BIC
  link_bic <- html_table[grepl("Bayesian information criterion", html_table, perl = TRUE)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_bic))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(fm_zip$bic, big.mark = ",", digits = 0L)
      ), link_bic
    ))



})



# stats.var.separate (when length > 1) ==========

data("bioChemists", package = "pscl")
fm_zip <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                           dist = "poisson"))
fm_zinb <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                            dist = "negbin"))

latex_table <- tablelight::light_table(list(fm_zip, fm_zip,
                                            fm_zinb, fm_zinb),
                                       modeltype = c("selection","outcome","selection","outcome"),
                                       stats.list = c("n","ll","lln","bic","alpha","link"),
                                       stats.var.separate = c(2L, 2L)
)

html_table <- tablelight::light_table(list(fm_zip, fm_zip,
                                           fm_zinb, fm_zinb),
                                      type = "html",
                                      modeltype = c("selection","outcome","selection","outcome"),
                                      stats.list = c("n","ll","lln","bic","alpha","link"),
                                      stats.var.separate = c(1L, 2L)
)



testthat::test_that("Summary statistics on two columns",{

  # ALPHA STATISTICS
  alpha_row <- stringr::str_split(
    latex_table[grepl("\\\\alpha", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(alpha_row), 3L)
  testthat::expect_equal(alpha_row[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                 ""))
  testthat::expect_equal(alpha_row[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 format(1/exp(fm_zinb$coefficients$count['Log(theta)','Estimate']),
                                                        digits = 3L, nsmall = 3L)))

  # COUNT DISTRIBUTION
  count_row <- stringr::str_split(
    latex_table[grepl("Count distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(count_row), 3L)
  testthat::expect_equal(count_row[,2L], sprintf(" \\multicolumn{2}{c}{Poisson} "))
  testthat::expect_equal(count_row[,3L], sprintf(" \\multicolumn{2}{c}{Negative Binomial} \\\\"))

  # LINK DISTRIBUTION
  link_row <- stringr::str_split(
    latex_table[grepl("Selection distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_row), 3L)
  testthat::expect_equal(link_row[,2L], sprintf(" \\multicolumn{2}{c}{Logit} "))
  testthat::expect_equal(link_row[,3L], sprintf(" \\multicolumn{2}{c}{Logit} \\\\"))

  # OBSERVATIONS
  link_obs <- stringr::str_split(
    latex_table[grepl("Observations", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_obs), 3L)
  testthat::expect_equal(link_obs[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                format(fm_zip$n, big.mark = ",")))
  testthat::expect_equal(link_obs[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                format(fm_zinb$n, big.mark = ",")))


  # LOGLIKELIHHOD
  link_llk <- stringr::str_split(
    latex_table[grepl("Log likelihood &", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk), 3L)
  testthat::expect_equal(link_llk[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                gsub("-","$-$",
                                                     format(fm_zip$loglik, big.mark = ",", digits = 0L)))
  )
  testthat::expect_equal(link_llk[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zinb$loglik, big.mark = ",", digits = 0L)))
  )

  # OBSERVATIONS (BY OBS)
  link_llk2 <- stringr::str_split(
    latex_table[grepl("Log likelihood \\(by obs.\\)", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk2), 3L)
  testthat::expect_equal(link_llk2[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                 gsub("-","$-$",
                                                      format(fm_zip$loglik/fm_zip$n,
                                                             big.mark = ",", digits = 3L,
                                                             nsmall = 3)))
  )
  testthat::expect_equal(link_llk2[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 gsub("-","$-$",
                                                      format(fm_zinb$loglik/fm_zinb$n,
                                                             big.mark = ",", digits = 3L,
                                                             nsmall = 3)))
  )

  # BIC
  link_bic <- stringr::str_split(
    latex_table[grepl("Bayesian information criterion", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_bic), 3L)
  testthat::expect_equal(link_bic[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                gsub("-","$-$",
                                                     format(fm_zip$bic, big.mark = ",", digits = 0L)))
  )
  testthat::expect_equal(link_bic[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zinb$bic, big.mark = ",", digits = 0L)))
  )


})



testthat::test_that("alpha: empty col1, empty col2-col3 (because Poisson in col2), non empty in col4",{

  # ALPHA STATISTICS
  alpha_row <- html_table[grepl("alpha", html_table, perl = TRUE)]
  alpha_row <- as.character(
    stringr::str_split(alpha_row, pattern = "</td>", simplify = TRUE)
  )
  alpha_row[1:(length(alpha_row)-1)] <- paste0(alpha_row[1:(length(alpha_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    alpha_row[2],
    "<td colspan=\"1\"></td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    alpha_row[3],
    "<td colspan=\"2\"></td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    alpha_row[4],
    sprintf("<td colspan=\"1\">%s</td>", format(1/exp(fm_zinb$coefficients$count['Log(theta)','Estimate']),
                                                digits = 3L, nsmall = 3L))
  )

})


testthat::test_that("count distrib: Poisson col1, Poisson col2-col3 (because Poisson in col2), Negative Binomial in col4",{

  # ALPHA STATISTICS
  count_row <- html_table[grepl("Count distribution", html_table, perl = TRUE)]
  count_row <- as.character(
    stringr::str_split(count_row, pattern = "</td>", simplify = TRUE)
  )
  count_row[1:(length(count_row)-1)] <- paste0(count_row[1:(length(count_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    count_row[2],
    "<td colspan=\"1\">Poisson</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[3],
    "<td colspan=\"2\">Poisson</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[4],
    "<td colspan=\"1\">Negative Binomial</td>"
  )

})


testthat::test_that("count distrib: Logit col1, Logit col2-col3 (because Logit in col2), Logit in col4",{

  # ALPHA STATISTICS
  count_row <- html_table[grepl("Selection distribution", html_table, perl = TRUE)]
  count_row <- as.character(
    stringr::str_split(count_row, pattern = "</td>", simplify = TRUE)
  )
  count_row[1:(length(count_row)-1)] <- paste0(count_row[1:(length(count_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    count_row[2],
    "<td colspan=\"1\">Logit</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[3],
    "<td colspan=\"2\">Logit</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[4],
    "<td colspan=\"1\">Logit</td>"
  )

})


testthat::test_that("observations consistent with models",{

  # ALPHA STATISTICS
  obs_row <- html_table[grepl("Observations", html_table, perl = TRUE)]
  obs_row <- as.character(
    stringr::str_split(obs_row, pattern = "</td>", simplify = TRUE)
  )
  obs_row[1:(length(obs_row)-1)] <- paste0(obs_row[1:(length(obs_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    obs_row[2],
    sprintf("<td colspan=\"1\">%s</td>", fm_zip$n)
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    obs_row[3],
    sprintf("<td colspan=\"2\">%s</td>", fm_zip$n)
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    obs_row[4],
    sprintf("<td colspan=\"1\">%s</td>", fm_zinb$n)
  )

})


testthat::test_that("loglikelihood consistent with models",{

  # ALPHA STATISTICS
  llk_row <- html_table[grepl("Log likelihood<", html_table, perl = TRUE)]
  llk_row <- as.character(
    stringr::str_split(llk_row, pattern = "</td>", simplify = TRUE)
  )
  llk_row[1:(length(llk_row)-1)] <- paste0(llk_row[1:(length(llk_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    llk_row[2],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zip$loglik, big.mark = ",", digits = 0L))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[3],
    sprintf("<td colspan=\"2\">%s</td>", format(fm_zip$loglik, big.mark = ",", digits = 0L))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[4],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zinb$loglik, big.mark = ",", digits = 0L))
  )

})



testthat::test_that("loglikelihood (by obs) consistent with models",{

  # ALPHA STATISTICS
  llk_row <- html_table[grepl("Log likelihood \\(by obs\\.\\)", html_table, perl = TRUE)]
  llk_row <- as.character(
    stringr::str_split(llk_row, pattern = "</td>", simplify = TRUE)
  )
  llk_row[1:(length(llk_row)-1)] <- paste0(llk_row[1:(length(llk_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    llk_row[2],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zip$loglik/fm_zip$n,
                                                big.mark = ",", digits = 3L,
                                                nsmall = 3))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[3],
    sprintf("<td colspan=\"2\">%s</td>", format(fm_zip$loglik/fm_zip$n,
                                                big.mark = ",", digits = 3L,
                                                nsmall = 3))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[4],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zinb$loglik/fm_zinb$n,
                                                big.mark = ",", digits = 3L,
                                                nsmall = 3))
  )

})




