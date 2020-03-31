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
