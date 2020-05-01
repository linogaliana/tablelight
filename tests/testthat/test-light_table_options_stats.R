ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

# stats.digits option ===============

latex_table <- tablelight::light_table(ols,
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column",
                                       stats.digits = 2L,
                                       stats.list = c("rsq","adj.rsq","lln"))

html_table <- tablelight::light_table(ols,
                                      type = "html",
                                      title = "My table title",
                                      label = "My table label",
                                      dep.var.labels = "My depvar",
                                      column.labels = "My label column",
                                      stats.digits = 2L,
                                      stats.list = c("rsq","adj.rsq","lln"))


testthat::test_that(
  "Two digits after the comma",
  {

    testthat::expect_equal(
      latex_table[grepl("Adjusted \\$R\\^2\\$", x = latex_table)],
      sprintf(
        "$R^2$ & %s \\\\", format(round(summary(ols)$adj.r.squared, 2L), nsmall = 2L)
      )
    )

    testthat::expect_equal(
      latex_table[grepl("Adjusted \\$R\\^2\\$", x = latex_table)],
      sprintf(
        "Adjusted $R^2$ & %s \\\\", format(round(summary(ols)$r.squared, 2L), nsmall = 2L)
      )
    )

    testthat::expect_equal(
      latex_table[grepl("Log likelihood", x = latex_table)],
      sprintf(
        "Log likelihood (by obs.) & %s \\\\",
        gsub("-","$-$",format(round(logLik(ols)/nobs(ols),
                                    2L), nsmall = 2L))
      )
    )

  }
)
