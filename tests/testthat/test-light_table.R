testthat::context("light_table produces the expected table")


# PART 1: BY MODEL TYPE -------------

# 1. OLS ==================

## 1.1. ONE MODEL

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

latex_table <- tablelight::light_table(ols,
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column")


testthat::test_that("Table header is correct",{

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


testthat::test_that("Body (coefficients) correct",{

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

# HEADER

testthat::test_that("Number of columns: number of models + 1", {
  tabular_row = stringr::str_split(pattern = "\n",
                                   latex_table[grepl("\\\\begin{tabular}", latex_table, perl = TRUE)], simplify = TRUE)[1,1]
  testthat::expect_equal(tabular_row, sprintf("\\begin{tabular}{@{\\extracolsep{5pt}}l%s}",
                                              paste(rep("c",3L), collapse = "")))
})

testthat::test_that("Number of columns: number of models + 1", {
  dep_var_row <- latex_table[grepl(
    pattern = "\\\\multicolumn\\{3\\}\\{c\\}\\{\\\\textit\\{Dependent variable:\\}\\}",
    latex_table)]
  dep_var_row <- as.character(stringr::str_split(pattern = "\n", dep_var_row, simplify = TRUE))
  dep_var_row2 <- dep_var_row[(which(grepl(
    pattern = "\\\\multicolumn\\{3\\}\\{c\\}\\{\\\\textit\\{Dependent variable:\\}\\}",
    latex_table))):length(dep_var_row)]
  testthat::expect_equal(dep_var_row2[1], "\\cline{2-4}")
  testthat::expect_equal(as.character(
    stringr::str_split(pattern = "&", dep_var_row2[2], simplify = TRUE)),
    c("\\\\[-1.8ex] ", " \\multicolumn{2}{c}{My depvar1} ", " \\multicolumn{1}{c}{My depvar2} \\\\"))
})

testthat::test_that("Correct column labels [NAs when vector is not long enough]", {
  testthat::expect_equal(
    sum(grepl(" & Label1 & Label2 & NA \\\\", latex_table)),
    1L
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

testthat::test_that("Too many dep.var.labels do not change the output", {
  testthat::expect_equal(
    latex_table2, latex_table
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

testthat::test_that("Same table except order of variables", {
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

latex_table2b <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column",
                                        order = c("Petal.Length", "Sepal.Width"))

testthat::test_that("Without (Intercept), constant goes at the bottom", {
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

testthat::test_that("Same table except for the omit variables", {
  row_coef <- grep("(Intercept)", latex_table1)
  row_coef2 <- grep("Petal.Length", latex_table1)
  testthat::expect_equal(c(
    latex_table1[1:(row_coef+2)],
    latex_table1[(row_coef2+3):length(latex_table1)]
  ),
  latex_table2
  )
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




