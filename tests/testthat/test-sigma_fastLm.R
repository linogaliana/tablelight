testthat::context("sigma method for fastLm objects")

ols_arma <- RcppArmadillo::fastLm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)
ols_eigen <- RcppEigen::fastLm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)
ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)


testthat::test_that("sigma method for fastLm consistent with lm",{
  testthat::expect_equal(
    sigma(ols),
    sigma(ols_arma)
  )
  testthat::expect_equal(
    sigma(ols),
    sigma(ols_arma)
  )
}
)
