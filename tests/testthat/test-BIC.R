testthat::context("BIC for light objects is correct")

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- tablelight::strip(zeroinfl_negbin)


testthat::expect_equal(
  BIC(zeroinfl_negbin),
  BIC(zeroinfl_negbin_strip)
)

# FASTLM (RCPPARMADILLO) -------------

x <- RcppArmadillo::fastLm(data = iris, Sepal.Width ~ Sepal.Length)
x2 <- lm(data = iris, Sepal.Width ~ Sepal.Length)


testthat::expect_equal(
  BIC_fastLm(x),
  BIC(x2)
)
