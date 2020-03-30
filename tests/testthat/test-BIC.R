testthat::context("BIC for light objects is correct")

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- texlight::strip(zeroinfl_negbin)


testthat::expect_equal(
  BIC(zeroinfl_negbin),
  BIC(zeroinfl_negbin_strip)
)

