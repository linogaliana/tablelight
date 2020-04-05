testthat::context("Methods to return log likelihood are correct")


# GLM OBJECTS ----------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

glm_light <- strip(glm)

testthat::expect_equal(
  as.numeric(logLik(glm)),
  as.numeric(logLik(glm_light))
)

testthat::expect_equal(
  as.numeric(logLik(glm)),
  as.numeric(logLik.light.glm(glm_light))
)

# ZEROINFL OBJECTS ----------

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- tablelight::strip(zeroinfl_negbin)


testthat::expect_equal(
  as.numeric(logLik(zeroinfl_negbin)),
  as.numeric(logLik(zeroinfl_negbin_strip))
)

testthat::expect_equal(
  as.numeric(logLik(zeroinfl_negbin)),
  as.numeric(logLik.light.zeroinfl(zeroinfl_negbin_strip))
)
