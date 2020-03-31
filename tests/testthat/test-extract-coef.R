testthat::context("extract_coeff recovers the coefficients")


library(texlight)

# GLM -------------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)
stats_glm <- texlight::extract_coeff(glm)

testthat::test_that("Coefficient names are consistent", {
  testthat::expect_equal(
    stats_glm[,'variable'],
    names(glm$coefficients)
  )
})

output_glm <- summary(glm)$coefficients

expected_coeff <- sapply(seq_len(nrow(output_glm)), function(i){
  paste0(format(output_glm[i, 'Estimate'], nsmall = 3L,
                digits = 2L),
         signif_stars(output_glm[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_coeff'],
    expected_coeff
  )
})


testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_sd'],
    paste0("(",
           format(output_glm[,'Std. Error'],
                  digits = 2L, nsmall = 3L
           ),
           ")")
  )
})



# LIGHT.GLM --------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

light_glm <- texlight::strip(glm)

stats_glm <- texlight::extract_coeff(light_glm)

testthat::test_that("Coefficient names are consistent", {
  testthat::expect_equal(
    stats_glm[,'variable'],
    names(glm$coefficients)
  )
})

output_glm <- summary(glm)$coefficients

expected_coeff <- sapply(seq_len(nrow(output_glm)), function(i){
  paste0(format(output_glm[i, 'Estimate'], nsmall = 3L,
                digits = 2L),
         signif_stars(output_glm[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_coeff'],
    expected_coeff
  )
})


testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_sd'],
    paste0("(",
           format(output_glm[,'Std. Error'],
                  digits = 2L, nsmall = 3L
                  ),
           ")")
  )
})


# LIGHT.ZEROINFL -------------------------

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- texlight::strip(zeroinfl_negbin)

testthat::test_that("When modeltype = 'missing', outcome is used", {

  testthat::expect_message(
    stats_glm <- texlight::extract_coeff(zeroinfl_negbin_strip),
    "'modeltype' argument missing, assuming 'outcome'"
  )

  testthat::expect_identical(
    stats_glm,
    texlight::extract_coeff(zeroinfl_negbin_strip, modeltype = "outcome")
  )
}
)

ZINB_outcome <- texlight::extract_coeff(zeroinfl_negbin_strip, modeltype = "outcome")
ZINB_zero <- texlight::extract_coeff(zeroinfl_negbin_strip, modeltype = "selection")

output_glm <- summary(zeroinfl_negbin)$coefficients

# COUNT MODEL =============

testthat::test_that("Coefficient names are consistent (theta not in parameter section)", {
  testthat::expect_equal(
    ZINB_outcome[,'variable'][ZINB_outcome[,'variable'] != "Log(theta)"],
    names(zeroinfl_negbin$coefficients$count)
  )
})

expected_coeff <- sapply(seq_len(nrow(output_glm$count)), function(i){
  paste0(format(round(output_glm$count[i, 'Estimate'],3L),  nsmall = 3L,
                digits = 2L, scientific = FALSE),
         signif_stars(output_glm$count[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    ZINB_outcome[,'text_coeff'],
    expected_coeff
  )
})

testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    ZINB_outcome[,'text_sd'],
    paste0("(",
           format(round(output_glm$count[,'Std. Error'], 3L),
                  digits = 2L, nsmall = 3L
           ),
           ")")
  )
})


# SELECTION MODEL =============

testthat::test_that("Coefficient names are consistent (theta not in parameter section)", {
  testthat::expect_equal(
    ZINB_zero[,'variable'][ZINB_zero[,'variable'] != "Log(theta)"],
    names(zeroinfl_negbin$coefficients$zero)
  )
})


expected_coeff <- sapply(seq_len(nrow(output_glm$zero)), function(i){
  paste0(format(round(output_glm$zero[i, 'Estimate'],3L),  nsmall = 3L,
                digits = 2L, scientific = FALSE),
         signif_stars(output_glm$zero[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    ZINB_zero[,'text_coeff'][ZINB_zero[,'variable'] != "Log(theta)"],
    expected_coeff
  )
})

testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    ZINB_outcome[,'text_sd'],
    paste0("(",
           format(round(output_glm$count[,'Std. Error'], 3L),
                  digits = 2L, nsmall = 3L
           ),
           ")")
  )
})

