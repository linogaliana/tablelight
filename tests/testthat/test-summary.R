testthat::context("internal summary functions work as expected")

testthat::test_that(
  "Without weights: same than summary",
  {
    testthat::expect_equal(
      as.character(summarize_data_(1:100, digits = 0L)),
      as.character(round(as.numeric(summary(1:100))))
    )
  }
)

df <- data.frame(x = exp(rnorm(100)), pond = sample(1:3, size = 100, replace = TRUE))

testthat::test_that(
  "summary_",{
    testthat::expect_equal(
      as.numeric(summary_(data = df, xvar = "x", digits = 0L)),
      as.numeric(round(c(summary(df$x),length(df$x))))
    )
  }
)

