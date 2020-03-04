testthat::context("liststat return the expected list of summary statistics")


# OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

stats_ols <- texlight:::liststats(ols)
stats_ols_bis <- texlight::liststats(ols, add_link = TRUE)

## PART A/ CHECK STATISTICS RETURNED ======

testthat::test_that(
  "Default method gives information for OLS",
  testthat::expect_equal(
    nrow(na.omit(stats_ols)),
    nrow(stats_ols)
  )
)

testthat::test_that(
  "Count distribution is 'Gaussian' for OLS",
  testthat::expect_equal(
    as.character(stats_ols_bis[stats_ols_bis$stat == "Count distribution",'val']),
    'Gaussian'
  )
)


testthat::test_that(
  "If you add argument add_link = TRUE, you have new lines not filled in OLS case",
  testthat::expect_equal(
    as.character(stats_ols_bis[stats_ols_bis$val == "",'stat']),
    'Selection distribution'
  )
)

testthat::test_that(
  "add_link = TRUE does not modify other rows",
  testthat::expect_equal(
    stats_ols_bis[!(stats_ols_bis$stat %in% c("Count distribution","Selection distribution")),
                  c('stat','val')],
    stats_ols[, c('stat','val')],
    check.attributes = FALSE
  )
)


## PART B/ CHECK STATISTICS VALUES ======

testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_ols_bis[stats_ols_bis$stat == "Observations","val"])),
      stats::nobs(ols)
    )

    testthat::expect_equal(
      as.numeric(as.character(stats_ols[stats_ols$stat == "Observations","val"])),
      stats::nobs(ols)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_ols_bis[stats_ols_bis$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(ols)), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_ols[stats_ols$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(ols)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_ols_bis[stats_ols_bis$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(ols)/stats::nobs(ols)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_ols[stats_ols$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(ols)/stats::nobs(ols)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_ols_bis[stats_ols_bis$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(ols), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_ols[stats_ols$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(ols), digits = 0L, big.mark = ",")
    )

  }

)



# GLM ---------------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

stats_glm <- texlight::liststats(glm)
stats_glm_bis <- texlight::liststats(glm, add_link = TRUE)


# A/ CHECK STATISTICS RETURNED ======

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


## PART B/ CHECK STATISTICS VALUES ======

testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_glm_bis[stats_glm_bis$stat == "Observations","val"])),
      stats::nobs(glm)
    )

    testthat::expect_equal(
      as.numeric(as.character(stats_glm[stats_glm$stat == "Observations","val"])),
      stats::nobs(glm)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_glm_bis[stats_glm_bis$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(glm)), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_glm[stats_glm$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(glm)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_glm_bis[stats_glm_bis$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(glm)/stats::nobs(glm)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_glm[stats_glm$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(glm)/stats::nobs(glm)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_glm_bis[stats_glm_bis$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(glm), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_glm[stats_glm$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(glm), digits = 0L, big.mark = ",")
    )

  }

)



# GLM.NB OBJECT ---------------------


quine <- MASS::quine

glmnb <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)


# A/ CHECK STATISTICS RETURNED ======

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
        format(1/glmnb$theta, digits = 3L, nsmall = 3L, big.mark = ",")
      )
    )
  }
)

## PART B/ CHECK STATISTICS VALUES ======

testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_glmnb_bis[stats_glmnb_bis$stat == "Observations","val"])),
      stats::nobs(glmnb)
    )

    testthat::expect_equal(
      as.numeric(as.character(stats_glmnb[stats_glmnb$stat == "Observations","val"])),
      stats::nobs(glmnb)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_glmnb_bis[stats_glmnb_bis$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(glmnb)), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_glmnb[stats_glmnb$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(glmnb)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_glmnb_bis[stats_glmnb_bis$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(glmnb)/stats::nobs(glmnb)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_glmnb[stats_glmnb$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(glmnb)/stats::nobs(glmnb)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_glmnb_bis[stats_glmnb_bis$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(glmnb), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_glmnb[stats_glmnb$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(glmnb), digits = 0L, big.mark = ",")
    )

  }

)






# ZEROINFL OBJECT ---------------------

## NEGBIN COUNT DISTRIBUTION ============

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")



stats_bis_zeroinfl_negbin <- texlight::liststats(zeroinfl_negbin)
stats_bis_zeroinfl_negbin <- texlight::liststats(zeroinfl_negbin, add_link = TRUE,
                                                 add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for glm.nb objects",
  testthat::expect_equal(
    nrow(na.omit(zeroinfl_negbin)),
    nrow(zeroinfl_negbin)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, count distribution added but no selection distribution",
  testthat::expect_equal(
    as.character(stats_bis_zeroinfl_negbin[grepl(x = as.character(stats_bis_zeroinfl_negbin$stat),
                                                 pattern = "(Count|Selection)"),'val']),
    c("Negative Binomial", 'Logit')
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
        format(1/glmnb$theta, digits = 3L, nsmall = 3L, big.mark = ",")
      )
    )
  }
)


# B/ CHECK STATISTICS VALUES ++++++


testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Observations","val"])),
      stats::nobs(zeroinfl_negbin)
    )

    testthat::expect_equal(
      as.numeric(as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Observations","val"])),
      stats::nobs(zeroinfl_negbin)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(zeroinfl_negbin)), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(zeroinfl_negbin)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(zeroinfl_negbin)/stats::nobs(zeroinfl_negbin)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(zeroinfl_negbin)/stats::nobs(zeroinfl_negbin)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(zeroinfl_negbin), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(zeroinfl_negbin), digits = 0L, big.mark = ",")
    )

  }

)


## POISSON COUNT DISTRIBUTION ============

zeroinfl_poisson <- pscl::zeroinfl(art ~ . | ., data = bioChemists)


stats_zeroinfl_poisson <- texlight::liststats(zeroinfl_poisson)
stats_bis_zeroinfl_poisson <- texlight::liststats(zeroinfl_poisson, add_link = TRUE,
                                                 add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for glm.nb objects",
  testthat::expect_equal(
    nrow(na.omit(zeroinfl_poisson)),
    nrow(zeroinfl_poisson)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, count distribution added but no selection distribution",
  testthat::expect_equal(
    as.character(stats_bis_zeroinfl_poisson[grepl(x = as.character(stats_bis_zeroinfl_poisson$stat),
                                                 pattern = "(Count|Selection)"),'val']),
    c("Poisson", 'Logit')
  )
)



# B/ CHECK STATISTICS VALUES ++++++


testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Observations","val"])),
      stats::nobs(zeroinfl_poisson)
    )

    testthat::expect_equal(
      as.numeric(as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Observations","val"])),
      stats::nobs(zeroinfl_poisson)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(zeroinfl_poisson)), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(zeroinfl_poisson)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(zeroinfl_poisson)/stats::nobs(zeroinfl_poisson)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(zeroinfl_poisson)/stats::nobs(zeroinfl_poisson)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(zeroinfl_poisson), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson[stats_bis_zeroinfl_poisson$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(zeroinfl_poisson), digits = 0L, big.mark = ",")
    )

  }

)





# oglmx OBJECTS -------------------

iris$y_r <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y_r ~ Sepal.Length ,
  data = iris
)


stats_oglm <- texlight::liststats(oglm)


testthat::test_that(
  "Default method gives information for OLS",
  testthat::expect_equal(
    nrow(na.omit(stats_oglm)),
    nrow(stats_oglm)
  )
)

