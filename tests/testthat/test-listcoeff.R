testthat::context("listcoeff return the expected list of coefficients statistics")

# OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)


texlight::listcoeff(ols)


testthat::test_that(
  "Coefficient names returned consistent with formula",
  testthat::expect_equal(
    texlight::listcoeff(
      lm(
        Sepal.Length ~ Sepal.Width,
        data = iris
      )
    ),
    c("(Intercept)", "Sepal.Width")
  )
)

testthat::test_that(
  "[OLS] Coefficient names returned consistent with formula",
  testthat::expect_equal(
    texlight::listcoeff(
      lm(
        Sepal.Length ~ 0 + Sepal.Width,
        data = iris
      )
    ),
    "Sepal.Width"
  )
)


# OGLMX -----------------------------

data(iris)
iris$y <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y ~ Sepal.Width,
  data = iris
)





testthat::test_that(
  "Coefficient names returned consistent with formula",
  testthat::expect_equal(
    texlight::listcoeff(oglm),
    c("(Intercept)", "Sepal.Width", "Threshold (1->2)", "Threshold (2->3)", NA)
  )
)

