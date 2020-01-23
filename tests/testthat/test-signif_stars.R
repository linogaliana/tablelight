testthat::context("pvalues transformed into signifiance stars")

testthat::test_that(
  "Number of stars match with expectation", {
    testthat::expect_equal(signif_stars(NA),
                           "")
    testthat::expect_equal(signif_stars(0.005),
                           "$^{***}$")
    testthat::expect_equal(signif_stars(0.02),
                           "$^{**}$")
    testthat::expect_equal(signif_stars(0.07),
                           "$^{*}$")
    testthat::expect_equal(signif_stars(0.15),
                           "")
  }
)
