testthat::context("listcoeff return the expected list of coefficients statistics")

# 1. OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)



testthat::test_that(
  "Coefficient names returned consistent with formula",
  testthat::expect_equal(
    tablelight::listcoeff(
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
    tablelight::listcoeff(
      lm(
        Sepal.Length ~ 0 + Sepal.Width,
        data = iris
      )
    ),
    "Sepal.Width"
  )
)


# 2. GLM ---------------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)


testthat::test_that(
  "[glm] Coefficient names returned consistent with formula",
  testthat::expect_equal(
    tablelight::listcoeff(glm),
    c("(Intercept)", "Sepal.Width")
  )
)

testthat::test_that(
  "[glm] Coefficient names returned consistent with formula",
  testthat::expect_equal(
    tablelight::listcoeff(
      glm(
        Sepal.Length ~ 0 + Sepal.Width,
        data = iris
      )
    ),
    "Sepal.Width"
  )
)



# 3. GLM.NB OBJECT ---------------------


quine <- MASS::quine

glmnb <- MASS::glm.nb(Days ~ Sex + Age, data = quine)


testthat::test_that(
  "[glm] Coefficient names returned consistent with formula",
  testthat::expect_equal(
    tablelight::listcoeff(glmnb),
    c("(Intercept)", paste0("Sex", levels(quine$Sex)[2]),
      paste0("Age", levels(quine$Age)[2:length(levels(quine$Age))]))
  )
)

# Quand il n'y a pas d'intercept, on a tous les niveaux d'un facteur
testthat::test_that(
  "[glmnb] Coefficient names returned consistent with formula",
  testthat::expect_equal(
    tablelight::listcoeff(
      MASS::glm.nb(Days ~ 0 + Sex + Age, data = quine)
    ),
    c(paste0("Sex", levels(quine$Sex)),
      paste0("Age", levels(quine$Age)[2:length(levels(quine$Age))]))
  )
)



# 4. ZEROINFL OBJECT ---------------------

data("bioChemists", package = "pscl")

ZINB <- pscl::zeroinfl(art ~ kid5 + phd | kid5 + phd,
                       data = bioChemists, dist = "negbin")

# Same variables in outcome and selection
testthat::test_that(
  "[zinb] Coefficient names returned consistent with formula",
  testthat::expect_equal(
    tablelight::listcoeff(ZINB),
    c("(Intercept)", "kid5", "phd")
  )
)

# Not same variables in outcome and selection
testthat::test_that(
  "[zinb] Coefficient names returned consistent with formula",{

    testthat::expect_equal(
      tablelight::listcoeff(
        pscl::zeroinfl(art ~ kid5 | phd,
                       data = bioChemists, dist = "negbin")
      ),
      c("(Intercept)", "kid5", "phd")
    )

    testthat::expect_equal(
      tablelight::listcoeff(
        pscl::zeroinfl(art ~ kid5 + phd | phd,
                       data = bioChemists, dist = "negbin")
      ),
      c("(Intercept)", "kid5", "phd")
    )

    testthat::expect_equal(
      tablelight::listcoeff(
        pscl::zeroinfl(art ~ kid5 + phd | 1,
                       data = bioChemists, dist = "negbin")
      ),
      c("(Intercept)", "kid5", "phd")
    )

    testthat::expect_equal(
      tablelight::listcoeff(
        pscl::zeroinfl(art ~ 1 | phd,
                       data = bioChemists, dist = "negbin")
      ),
      c("(Intercept)", "phd")
    )


  }

)


# 5. LIGHT.OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

light_ols <- tablelight::strip(ols)


testthat::test_that(
  "Coefficient names returned consistent with formula", {

    testthat::expect_equal(
      tablelight::listcoeff(light_ols),
      c("(Intercept)", "Sepal.Width")
    )

    testthat::expect_equal(
      tablelight::listcoeff(light_ols),
      tablelight::listcoeff(ols)
    )


  }

)


# 6. LIGHT.GLM --------------------


glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

light_glm <- tablelight::strip(glm)


testthat::test_that(
  "Coefficient names returned consistent with formula", {

    testthat::expect_equal(
      tablelight::listcoeff(light_glm),
      c("(Intercept)", "Sepal.Width")
    )

    testthat::expect_equal(
      tablelight::listcoeff(light_glm),
      tablelight::listcoeff(glm)
    )


  }

)


# 7. LIGHT.NEGBIN --------------------


quine <- MASS::quine

glmnb <- MASS::glm.nb(Days ~ Sex + Age, data = quine)
light_glmnb <- tablelight::strip(glmnb)


testthat::test_that(
  "Coefficient names returned consistent with formula", {

    testthat::expect_equal(
      tablelight::listcoeff(light_glmnb),
      c("(Intercept)", paste0("Sex", levels(quine$Sex)[2]),
        paste0("Age", levels(quine$Age)[2:length(levels(quine$Age))]))
    )


    testthat::expect_equal(
      tablelight::listcoeff(light_glmnb),
      tablelight::listcoeff(glmnb)
    )


  }

)


# 8. LIGHT.ZEROINFL ----------------

# Not same variables in outcome and selection
testthat::test_that(
  "[zinb] Coefficient names returned consistent with formula",{

    testthat::expect_equal(
      tablelight::listcoeff(
        tablelight::strip(
          pscl::zeroinfl(art ~ kid5 | phd,
                         data = bioChemists, dist = "negbin")
        )
      ),
      c("(Intercept)", "kid5", "Log(theta)","phd")
    )

    testthat::expect_equal(
      tablelight::listcoeff(
        tablelight::strip(
          pscl::zeroinfl(art ~ kid5 + phd | phd,
                         data = bioChemists, dist = "negbin")
        )
      ),
      c("(Intercept)", "kid5", "phd", "Log(theta)")
    )

    testthat::expect_equal(
      tablelight::listcoeff(
        tablelight::strip(
          pscl::zeroinfl(art ~ kid5 + phd | 1,
                         data = bioChemists, dist = "negbin")
        )
      ),
      c("(Intercept)", "kid5", "phd", "Log(theta)")
    )

    testthat::expect_equal(
      tablelight::listcoeff(
        tablelight::strip(
          pscl::zeroinfl(art ~ 1 | phd,
                         data = bioChemists, dist = "negbin")
        )
      ),
      c("(Intercept)", "Log(theta)","phd")
    )


  }

)



# 9. FASTLM ------------------------

ols <- RcppEigen::fastLm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)



testthat::test_that(
  "Coefficient names returned consistent with formula",
  testthat::expect_equal(
    tablelight::listcoeff(
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
    tablelight::listcoeff(
      lm(
        Sepal.Length ~ 0 + Sepal.Width,
        data = iris
      )
    ),
    "Sepal.Width"
  )
)



# OGLMX -----------------------------

# data(iris)
# iris$y <- as.numeric(iris$Species)
#
# oglm <- oglmx::oglmx(
#   y ~ Sepal.Width,
#   data = iris
# )
#
#
#
#
#
# testthat::test_that(
#   "Coefficient names returned consistent with formula",
#   testthat::expect_equal(
#     tablelight::listcoeff(oglm),
#     c("(Intercept)", "Sepal.Width", "Threshold (1->2)", "Threshold (2->3)", NA)
#   )
# )
#
