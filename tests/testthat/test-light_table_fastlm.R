testthat::context("[OLS] light_table produces the expected table")


# SINGLE OLS MODEL ----------------------------

ols <- RcppEigen::fastLm(
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

# iris$y_r <- as.numeric(iris$Species)
#
# oglm <- oglmx::oglmx(
#   y_r ~ Sepal.Length ,
#   data = iris
# )
#




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

  testthat::expect_true(sum(grepl("\\\\begin{adjustbox}{width=1.1\\\\linewidth, center}", perl = TRUE,
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


# R2 and Adjusted R2 ====================
ols <- lm(
  Sepal.Length ~ Sepal.Width + Petal.Length,
  data = iris
)

# 2. light objects
latex_table <- tablelight::light_table(tablelight::strip(ols),
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column",
                                       stats.list = c("rsq","adj.rsq"))
html_table <- tablelight::light_table(tablelight::strip(ols),
                                      type = "html",
                                      title = "My table title",
                                      label = "My table label",
                                      dep.var.labels = "My depvar",
                                      column.labels = "My label column",
                                      stats.list = c("rsq","adj.rsq"))


testthat::test_that("R2 correctly reported", {
  testthat::expect_equal(
    latex_table[grepl("^\\$R\\^2\\$", latex_table, perl = TRUE)],
    sprintf(
      "$R^2$ & %s \\\\", format(summary(ols)$r.squared, digits = 3L, nsmall = 3L)
    )
  )
}
)

testthat::test_that("Adjusted R2 correctly reported", {
  testthat::expect_equal(
    latex_table[grepl("Adjusted \\$R\\^2\\$", latex_table, perl = TRUE)],
    sprintf(
      "Adjusted $R^2$ & %s \\\\", format(summary(ols)$adj.r.squared, digits = 3L, nsmall = 3L)
    )
  )
})


testthat::test_that("R2 correctly reported", {
  testthat::expect_equal(
    html_table[grepl(">R<sup>2</sup>", html_table)],
    sprintf("<tr><td style=\"text-align:left\">R<sup>2</sup></td><td>%s</td></tr>",
            format(summary(ols)$r.squared, digits = 3L, nsmall = 3L))
  )
}
)

testthat::test_that("R2 correctly reported", {
  testthat::expect_equal(
    html_table[grepl("Adjusted R<sup>2</sup>", html_table)],
    sprintf("<tr><td style=\"text-align:left\">Adjusted R<sup>2</sup></td><td>%s</td></tr>",
            format(summary(ols)$adj.r.squared, digits = 3L, nsmall = 3L))
  )
}
)



