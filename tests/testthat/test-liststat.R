testthat::context("listcoeff return the coefficients")


# OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

stats_ols <- texlight::liststats(ols)
stats_ols_bis <- texlight::liststats(ols, add_link = TRUE)


testthat::test_that(
  "Default method gives information for OLS",
  testthat::expect_equal(
    nrow(na.omit(stats_ols)),
    nrow(stats_ols)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, you have new lines not filled with in OLS",
  testthat::expect_equal(
    as.character(stats_ols_bis[stats_ols_bis$val == "",'stat']),
    c('Count distribution', 'Selection distribution')
  )
)

testthat::test_that(
  "add_link = TRUE does not modify other rows",
  testthat::expect_equal(
    stats_ols_bis[stats_ols_bis$val != "", c('stat','val')],
    stats_ols[, c('stat','val')]
  )
)



# GLM ---------------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

stats_glm <- texlight::liststats(glm)
stats_glm_bis <- texlight::liststats(glm, add_link = TRUE)



testthat::test_that(
  "Default method gives information for glm",
  testthat::expect_equal(
    nrow(na.omit(stats_glm)),
    nrow(stats_glm)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, count distribution added but no selection distribution",
  testthat::expect_equal(
    tolower(
      as.character(stats_glm_bis[grepl(x = as.character(stats_glm_bis$stat),
                                       pattern = "(Count|Selection)"),'val'])
    ),
    c(glm$family$family, '')
  )
)



# GLM.NB OBJECT ---------------------


glmnb <- MASS::glm.nb(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris
)


stats_glmnb <- texlight::liststats(glmnb)
stats_glmnb_bis <- texlight::liststats(glmnb, add_link = TRUE,
                                       add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for glm.nb objects",
  testthat::expect_equal(
    nrow(na.omit(stats_glmnb)),
    nrow(stats_glmnb)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, count distribution added but no selection distribution",
  testthat::expect_equal(
    as.character(stats_glmnb_bis[grepl(x = as.character(stats_glmnb_bis$stat),
                                       pattern = "(Count|Selection)"),'val']),
    c("Negative Binomial", '')
  )
)


testthat::test_that(
  "If you add argument add_alpha = TRUE, dispersion parameter is returned",{
    testthat::expect_equal(
      length(as.character(stats_glmnb_bis[grepl(x = as.character(stats_glmnb_bis$stat),
                                                pattern = "alpha"),'stat'])
      ),
      1L
    )
    testthat::expect_equal(
      as.character(
        stats_glmnb_bis[grepl(x = as.character(stats_glmnb_bis$stat),
                                                pattern = "alpha"),'val']
        ),
      as.character(
        format(1/glmnb$theta, digits = 3L, nsmall = 3L)
        )
    )
  }
)


# ZEROINFL OBJECT ---------------------

## POISSON COUNT DISTRIBUTION ========

iris$y <- iris$`Sepal.Length`
iris$y[sample(seq_len(nrow(iris)),
              .1*nrow(iris),
              replace = FALSE)] <- 0

zeroinfl <- pscl::zeroinfl(
  I(round(y)) ~ Sepal.Width,
  data = iris
)

stats_zeroinfl <- texlight::liststats(zeroinfl)
stats_zeroinfl <- texlight::liststats(zeroinfl, add_link = TRUE,
                                       add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for glm.nb objects",
  testthat::expect_equal(
    nrow(na.omit(zeroinfl)),
    nrow(zeroinfl)
  )
)


## NEGATIVE BINOMIAL COUNT DISTRIBUTION ========

zeroinfl2 <- pscl::zeroinfl(
  I(round(y)) ~ Sepal.Width,
  data = iris,
  dist = "negbin"
)

