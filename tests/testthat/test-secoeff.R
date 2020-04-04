testthat::context("se_coeff methods return coefficients s.e., t stat and pvalues")


# 1. OLS -------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)


testthat::expect_equal(
  secoeff(ols),
  summary(ols)$coefficients
)

testthat::expect_equal(
  secoeff(strip(ols)),
  summary(ols)$coefficients
)

# 2. OGLM -------------------

iris$y_r <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y_r ~ Sepal.Length ,
  data = iris
)


testthat::expect_equal(
  texlight::secoeff(oglm),
  summary(oglm)$estimate
)


# 3. NEGBIN ----------

quine.nb1 <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine)


# testthat::expect_equal(
#   texlight:::secoeff(quine.nb1),
#   summary(quine.nb1)$estimate
# )


# 4. secoeff.light.zeroinfl

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- texlight::strip(zeroinfl_negbin)

testthat::expect_equal(
  secoeff(zeroinfl_negbin_strip, modeltype = "count"),
  secoeff(zeroinfl_negbin)$count
)

testthat::expect_equal(
  secoeff(zeroinfl_negbin_strip, modeltype = "selection"),
  secoeff(zeroinfl_negbin)$zero
)



# 5. SUMMARY.LM ---------

df = data.frame(y = rnorm(100L),
                x = rnorm(100L))

object <- lm(y ~  x, df)

testthat::expect_equal(
  summary(object)$coefficients,
  secoeff(summary(object))
)

testthat::expect_equal(
  secoeff.summary.lm(summary(object)),
  secoeff(summary(object))
)
