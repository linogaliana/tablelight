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
                                       column.labels = "My label column")

html_table <- tablelight::light_table(ols,
                                      type = "html",
                                      title = "My table title",
                                      dep.var.labels = "My depvar",
                                      column.labels = "My label column")


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
                                       column.labels = c("Label1","Label2"))

html_table <- tablelight::light_table(list(ols, ols, ols),
                                      type = "html",
                                      title = "My table title",
                                      dep.var.labels = c("My depvar1", "My depvar2","toomanyvar"),
                                      dep.var.separate = c(2,1),
                                      column.labels = c("Label1","Label2"))


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


ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

# dep.var.options ===============

latex_table <- tablelight::light_table(ols,
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column")

latex_table2 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = rep("My depvar",10L),
                                        column.labels = "My label column")


html_table <- tablelight::light_table(ols,
                                      type = "html",
                                      title = "My table title",
                                      label = "My table label",
                                      dep.var.labels = "My depvar",
                                      column.labels = "My label column")

html_table2 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = rep("My depvar",10L),
                                       column.labels = "My label column")


testthat::test_that("Too many dep.var.labels do not change the output", {
  testthat::expect_equal(
    latex_table2, latex_table
  )
  testthat::expect_equal(
    html_table, html_table2
  )
})


# order_variable ============

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
                                        order = c("(Intercept)","Petal.Length", "Sepal.Width"))

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
                                       order = c("Petal.Length", "(Intercept)", "Sepal.Width"))


testthat::test_that("[latex] Same table except order of variables", {
  row_coef <- grep("Sepal.Width", latex_table1)
  testthat::expect_equal(c(
    latex_table1[1:(row_coef-1)],
    latex_table1[(row_coef+3):(row_coef+5)],
    latex_table1[(row_coef):(row_coef+2)],
    latex_table1[(row_coef+6):length(latex_table1)]
  ),
  latex_table2
  )
})


testthat::test_that("[html] Same table except order of variables", {

  row_coef  <- html_table1[grep("Petal.Length", html_table1)]
  row_coef2 <- html_table2[grep("Petal.Length", html_table2)]

  row_coef3 <- as.character(
    stringr::str_split(row_coef2, "</td>", simplify = TRUE)
  )
  row_coef3 <- paste0(row_coef3[1:(length(row_coef3)-1)], "</td>")

  var_pos <- grep("(Intercept)", row_coef3)
  row_coef3 <- row_coef3[c(var_pos:(var_pos+11),
                           1:(var_pos-1),(var_pos+12):length(row_coef3))]

  row_coefb <- as.character(
    stringr::str_split(row_coef, "</td>", simplify = TRUE)
  )
  row_coefb <- paste0(row_coefb[1:(length(row_coefb)-1)], "</td>")

  testthat::expect_equal(
    row_coef3,
    row_coefb)
})


latex_table2b <- tablelight::light_table(ols,
                                         title = "My table title",
                                         label = "My table label",
                                         dep.var.labels = "My depvar",
                                         column.labels = "My label column",
                                         order = c("Petal.Length", "Sepal.Width"))

testthat::test_that("[latex] Without (Intercept), constant goes at the bottom", {
  row_coef <- grep("(Intercept)", latex_table2)
  row_coef2 <- grep("Sepal.Width", latex_table2)
  testthat::expect_equal(c(
    latex_table2[1:(row_coef-1)],
    latex_table2[(row_coef+3):(row_coef2+2)],
    latex_table2[(row_coef):(row_coef+2)],
    latex_table2[(row_coef2+3):length(latex_table1)]
  ),
  latex_table2b
  )
})


html_table2b <- tablelight::light_table(ols,
                                        type = "html",
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column",
                                        order = c("Petal.Length", "Sepal.Width"))

testthat::test_that("[html] Same table except order of variables", {

  row_coef  <- html_table1[grep("Petal.Length", html_table2)]
  row_coef2 <- html_table2[grep("Petal.Length", html_table2b)]

  row_coef3 <- as.character(
    stringr::str_split(row_coef2, "</td>", simplify = TRUE)
  )
  row_coef3 <- paste0(row_coef3[1:(length(row_coef3)-1)], "</td>")

  var_pos <- grep("(Intercept)", row_coef3)
  row_coef3 <- row_coef3[c(var_pos:(var_pos+11),
                           1:(var_pos-1),(var_pos+12):length(row_coef3))]

  row_coefb <- as.character(
    stringr::str_split(row_coef, "</td>", simplify = TRUE)
  )
  row_coefb <- paste0(row_coefb[1:(length(row_coefb)-1)], "</td>")

  testthat::expect_equal(
    row_coef3,
    row_coefb)
})


# omit ============


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
                                        omit = c("Petal.Length", "Sepal.Width"))

testthat::test_that("[latex] Same table except for the omit variables", {
  row_coef <- grep("(Intercept)", latex_table1)
  row_coef2 <- grep("Petal.Length", latex_table1)
  testthat::expect_equal(c(
    latex_table1[1:(row_coef+2)],
    latex_table1[(row_coef2+3):length(latex_table1)]
  ),
  latex_table2
  )
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
                                        omit = c("Petal.Length", "Sepal.Width"))


testthat::test_that("[html] Same table except order of variables", {

  row_coef  <- html_table1[grep("(Intercept)", html_table1)]
  row_coef_smaller  <- html_table2[grep("(Intercept)", html_table2)]

  row_coef2 <- as.character(
    stringr::str_split(row_coef, "</td>", simplify = TRUE)
  )
  row_coef2 <- paste0(row_coef2[1:(length(row_coef2)-1)], "</td>")

  row_coef_smaller2 <- as.character(
    stringr::str_split(row_coef_smaller, "</td>", simplify = TRUE)
  )
  row_coef_smaller2 <- paste0(row_coef_smaller2[1:(length(row_coef_smaller2)-1)], "</td>")


  var_pos <- grep("Sepal.Width", row_coef2)
  row_coef3 <- row_coef2[c(1:(var_pos-1),
                           (var_pos+12):length(row_coef3))]

  testthat::expect_equal(
    row_coef3,
    row_coef_smaller2)
})




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
                           stats.var.separate = 2L)


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


# stats.var.separate (when length > 1) ==========

data("bioChemists", package = "pscl")
fm_zip <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                           dist = "poisson"))
fm_zinb <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                            dist = "negbin"))

latex_table <- tablelight::light_table(list(fm_zip, fm_zip,
                                            fm_zinb, fm_zinb),
                                       modeltype = c("selection","outcome","selection","outcome"),
                                       #                           dep.var.labels = c("ZIP","ZINB"),
                                       #                           dep.var.separate = 2L,
                                       stats.var.separate = c(2L, 2L)
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




