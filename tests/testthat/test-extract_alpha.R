testthat::context("extract_alpha returns alpha when needed")



# I - OLS: NO COEF ---------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

testthat::test_that(
  "[OLS] Empty string returned",
  testthat::expect_equal(tablelight::extract_alpha(ols),
                         "")
)

# II - GLM: NO COEF ---------------

glm <- glm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

testthat::test_that(
  "[glm] Empty string returned",
  testthat::expect_equal(tablelight::extract_alpha(glm),
                         "")
)



# III - GLM.NB: COEF ---------------

quine <- MASS::quine

glmnb <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)

testthat::test_that(
  "[glm.nb] alpha returned",
  testthat::expect_equal(tablelight::extract_alpha(glmnb),
                         as.character(
                           format(
                             1/glmnb$theta, digits = 3L, nsmall = 3L)
                         )
  )
)

# IV - PSCL NEGBIN: COEF ---------------

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")


testthat::test_that(
  "[zeroinfl] Empty string returned",
  testthat::expect_equal(tablelight::extract_alpha(zeroinfl_negbin),
                         format(
                           1/zeroinfl_negbin$theta, digits = 3L, nsmall = 3L)
  )
)


# V - PSCL POISSON: NO COEF ---------------


zeroinfl <- pscl::zeroinfl(art ~ . | ., data = bioChemists)


testthat::test_that(
  "[zeroinfl] Empty string returned",
  testthat::expect_equal(tablelight::extract_alpha(zeroinfl),"")
)

