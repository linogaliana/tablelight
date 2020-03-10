testthat::context("light_table produces the expected table")


# PART 1: BY MODEL TYPE -------------

# 1. OLS -----------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

latex_table <- texlight::light_table(ols,
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
      texlight:::signif_stars(sd['(Intercept)']),
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
      texlight:::signif_stars(sd['Sepal.Width']),
      "\\\\"), pattern = "-", replacement = "$-$"
    )
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
  x <- texlight::light_table(oglm,
                             modeltype = "outcome"),
  "attributes are not identical across measure variables"
)


testthat::expect_equal(
  class(x),
  "character"
)

testthat::expect_warning(
  x2 <- texlight::light_table(list(oglm, oglm),
                              modeltype = c("outcome","outcome")),
  "attributes are not identical across measure variables"
)

testthat::expect_equal(
  class(x2),
  "character"
)
