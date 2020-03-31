context("strip method has expected behavior for every class")


# 1. LM -------------------

df = data.frame(y = rnorm(100L),
                x = rnorm(100L))

object <- lm(y ~  x, df)
object_light <- strip(object)


testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), paste0("light.", class(object)))
  )
})


testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    "Gaussian"
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    ""
  )
})


# 2. GLM -------------------------

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
object <- glm(counts ~ outcome + treatment, family = poisson())

object_light <- strip(object)

testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), "light.glm"))
})


testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    "Poisson"
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    ""
  )
})


# 3. NEGBIN ----------------------

object <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine)
object_light <- strip(object)


testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), paste0("light.", class(object))))
})


testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    "Negative Binomial"
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    ""
  )
})



