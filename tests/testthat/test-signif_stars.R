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


testthat::test_that(
  "Number of stars match with expectation", {
    testthat::expect_equal(signif_stars(NA, type = "html"),
                           "")
    testthat::expect_equal(signif_stars(0.005, type = "html"),
                           "<sup>***</sup>")
    testthat::expect_equal(signif_stars(0.02, type = "html"),
                           "<sup>**</sup>")
    testthat::expect_equal(signif_stars(0.07, type = "html"),
                           "<sup>*</sup>")
    testthat::expect_equal(signif_stars(0.15, type = "html"),
                           "")
  }
)


testthat::test_that(
  "Number of stars match with expectation", {
    testthat::expect_equal(signif_stars(NA, type = "none"),
                           "")
    testthat::expect_equal(signif_stars(0.005, type = "none"),
                           "***")
    testthat::expect_equal(signif_stars(0.02, type = "none"),
                           "**")
    testthat::expect_equal(signif_stars(0.07, type = "none"),
                           "*")
    testthat::expect_equal(signif_stars(0.15, type = "none"),
                           "")
  }
)


testthat::test_that("Only accepted answers are 'latex', 'html' or 'none'",{
  testthat::expect_error(signif_stars(0.02, type = "HTML"))
  testthat::expect_error(signif_stars(0.02, type = "LatEx"))
  testthat::expect_error(signif_stars(0.02, type = "None"))
  testthat::expect_error(signif_stars(0.02, type = "anything"))
})


# vectorized -------------------

testthat::expect_identical(
  signif_stars_vectorized(
    c(NA, 0.005, 0.02, 0.07, 0.15),
    type = "none"
  ),
  c("","***","**","*","")
)

testthat::expect_identical(
  signif_stars_vectorized(
    c(NA, 0.005, 0.02, 0.07, 0.15),
    type = "html"
  ),
  c("","<sup>***</sup>","<sup>**</sup>",
    "<sup>*</sup>","")
)

testthat::expect_identical(
  signif_stars_vectorized(
    c(NA, 0.005, 0.02, 0.07, 0.15),
    type = "latex"
  ),
  c("","$^{***}$","$^{**}$",
    "$^{*}$","")
)
