testthat::context("liststat return the expected list of summary statistics")


# 1. OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

stats_ols <- texlight:::liststats(ols)
stats_ols_bis <- texlight::liststats(ols, add_link = TRUE)

## 1.A. CHECK STATISTICS RETURNED ======

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


## 1.B. CHECK STATISTICS VALUES ======

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



# 2. GLM ---------------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

stats_glm <- texlight::liststats(glm)
stats_glm_bis <- texlight::liststats(glm, add_link = TRUE)


# 2.A. CHECK STATISTICS RETURNED ======

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


## 2.B. CHECK STATISTICS VALUES ======

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



# 3. GLM.NB OBJECT ---------------------


quine <- MASS::quine

glmnb <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)


# 3.A. CHECK STATISTICS RETURNED ======

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

## 3.B. CHECK STATISTICS VALUES ======

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






# 4. ZEROINFL OBJECT ---------------------

## 4.A. NEGBIN COUNT DISTRIBUTION ============

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- texlight::strip(zeroinfl_negbin)


stats_bis_zeroinfl_negbin <- texlight::liststats(zeroinfl_negbin,
                                                 add_link = TRUE,
                                                 add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for glm.nb objects",
  testthat::expect_equal(
    nrow(na.omit(stats_bis_zeroinfl_negbin)),
    nrow(stats_bis_zeroinfl_negbin)
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


## 4.B. POISSON COUNT DISTRIBUTION ============

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


# 5. LIGHT.OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

light_ols <- texlight::strip(ols)

stats_ols <- texlight:::liststats(ols, add_link = TRUE)
stats_ols_strip <- texlight:::liststats(light_ols, add_link = TRUE)


## 5.A. CHECK STATISTICS RETURNED ======

testthat::test_that(
  "light.ols method gives same information than ols method",
  testthat::expect_equal(
    nrow(stats_ols_strip),
    nrow(stats_ols)
  )
)

testthat::test_that(
  "Count distribution is 'Gaussian' for OLS",
  testthat::expect_equal(
    as.character(stats_ols_strip[stats_ols_strip$stat == "Count distribution",'val']),
    'Gaussian'
  )
)


testthat::test_that(
  "If you add argument add_link = TRUE, you have new lines not filled in OLS case",
  testthat::expect_equal(
    as.character(stats_ols_strip[stats_ols_strip$val == "",'stat']),
    'Selection distribution'
  )
)

testthat::test_that(
  "add_link = TRUE does not modify other rows",
  testthat::expect_equal(
    stats_ols_strip[!(stats_ols_strip$stat %in% c("Count distribution","Selection distribution")),
                  c('stat','val')],
    stats_ols[!(stats_ols$stat %in% c("Count distribution","Selection distribution")),
                    c('stat','val')],
    check.attributes = FALSE
  )
)


## 5.B. CHECK STATISTICS VALUES ======

testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_ols_strip[stats_ols_strip$stat == "Observations","val"])),
      stats::nobs(ols)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_ols_strip[stats_ols_strip$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(ols)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_ols_strip[stats_ols_strip$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(ols)/stats::nobs(ols)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_ols_strip[stats_ols_strip$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(ols), digits = 0L, big.mark = ",")
    )

  }

)











# 6. LIGHT.GLM ------------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

light_glm <- texlight::strip(glm)

stats_glm <- texlight::liststats(glm, add_link = TRUE, add_alpha = TRUE)
stats_glm_strip <- texlight::liststats(light_glm, add_link = TRUE)


## 6.A. CHECK STATISTICS RETURNED ======

testthat::test_that(
  "light.glm method gives same information than glm method when add_alpha=TRUE",
  testthat::expect_equal(
    nrow(stats_glm),
    nrow(stats_glm_strip)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, count distribution added but no selection distribution",
  testthat::expect_equal(
    tolower(
      as.character(stats_glm_strip[grepl(x = as.character(stats_glm_strip$stat),
                                       pattern = "(Count|Selection)"),'val'])
    ),
    c(glm$family$family, '')
  )
)


## 6.B. CHECK STATISTICS VALUES ======

testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_glm_strip[stats_glm_strip$stat == "Observations","val"])),
      stats::nobs(glm)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_glm_strip[stats_glm_strip$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(glm)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_glm_strip[stats_glm_strip$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(glm)/stats::nobs(glm)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )


  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_glm_strip[stats_glm_strip$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(glm), digits = 0L, big.mark = ",")
    )

  }

)




# 7. LIGHT.NEGBIN OBJECT ---------------------


quine <- MASS::quine

glmnb <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
glmnb_light <- texlight::strip(glmnb)

# 7.A. CHECK STATISTICS RETURNED ======

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

## 7.B. CHECK STATISTICS VALUES ======

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














# 8. FAST GLM.NB

requireNamespace("gravity", quietly = TRUE)

quine <- MASS::quine

glmnb <- gravity::fastglm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
glmnb_light <- texlight::strip(glmnb)


# 8.A. CHECK STATISTICS RETURNED ======

stats_glmnb <- texlight::liststats(glmnb)
stats_glmnb_bis <- texlight::liststats(glmnb_light, add_link = TRUE,
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

## 8.B. CHECK STATISTICS VALUES ======

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



# 9. FASTZEROINFL OBJECT ---------------------

## 9.A. NEGBIN COUNT DISTRIBUTION ============

data("bioChemists", package = "pscl")

zeroinfl_negbin <- gravity::fastzeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
stats_bis_zeroinfl_negbin <- texlight::liststats(zeroinfl_negbin,
                                                 add_link =TRUE, add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for fast.zeroinfl objects",
  testthat::expect_equal(
    nrow(na.omit(stats_bis_zeroinfl_negbin)),
    nrow(stats_bis_zeroinfl_negbin)
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
      length(as.character(stats_bis_zeroinfl_negbin[grepl(x = as.character(stats_bis_zeroinfl_negbin$stat),
                                                pattern = "alpha"),'stat'])
      ),
      1L
    )
    testthat::expect_equal(
      as.character(
        stats_bis_zeroinfl_negbin[grepl(x = as.character(stats_bis_zeroinfl_negbin$stat),
                              pattern = "alpha"),'val']
      ),
      as.character(
        format(1/zeroinfl_negbin$theta, digits = 3L, nsmall = 3L, big.mark = ",")
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

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

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

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin[stats_bis_zeroinfl_negbin$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(zeroinfl_negbin), digits = 0L, big.mark = ",")
    )

  }

)


## 9.B. POISSON COUNT DISTRIBUTION ============

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




# 10. LIGHT.FASTZEROINFL OBJECT ---------------------

## 10.A. NEGBIN COUNT DISTRIBUTION ============

data("bioChemists", package = "pscl")

zeroinfl_negbin <- gravity::fastzeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- texlight::strip(zeroinfl_negbin)


stats_bis_zeroinfl_negbin <- texlight::liststats(zeroinfl_negbin, add_link = TRUE,
                                                 add_alpha = TRUE)
stats_bis_zeroinfl_negbin_strip <- texlight::liststats(zeroinfl_negbin_strip, add_link = TRUE,
                                                 add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for fastzeroinfl objects",
  testthat::expect_equal(
    as.character(stats_bis_zeroinfl_negbin$val),
    as.character(stats_bis_zeroinfl_negbin_strip$val)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, count distribution added but no selection distribution",
  testthat::expect_equal(
    as.character(stats_bis_zeroinfl_negbin_strip[grepl(x = as.character(stats_bis_zeroinfl_negbin$stat),
                                                 pattern = "(Count|Selection)"),'val']),
    c("Negative Binomial", 'Logit')
  )
)


testthat::test_that(
  "If you add argument add_alpha = TRUE, dispersion parameter is returned",{
    testthat::expect_equal(
      length(as.character(stats_bis_zeroinfl_negbin_strip[grepl(x = as.character(stats_bis_zeroinfl_negbin_strip$stat),
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
      as.numeric(as.character(stats_bis_zeroinfl_negbin_strip[stats_bis_zeroinfl_negbin_strip$stat == "Observations","val"])),
      stats::nobs(zeroinfl_negbin)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin_strip[stats_bis_zeroinfl_negbin_strip$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(zeroinfl_negbin)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin_strip[stats_bis_zeroinfl_negbin_strip$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(zeroinfl_negbin)/stats::nobs(zeroinfl_negbin)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )


  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_negbin_strip[stats_bis_zeroinfl_negbin_strip$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(zeroinfl_negbin), digits = 0L, big.mark = ",")
    )

  }

)


## 10.B. POISSON COUNT DISTRIBUTION ============

data("bioChemists", package = "pscl")

zeroinfl_poisson <- pscl::zeroinfl(art ~ . | ., data = bioChemists)
zeroinfl_poisson_strip <- texlight::strip(zeroinfl_poisson)


stats_bis_zeroinfl_poisson <- texlight::liststats(zeroinfl_poisson, add_link = TRUE,
                                                  add_alpha = TRUE)
stats_bis_zeroinfl_poisson_strip <- texlight::liststats(zeroinfl_poisson_strip, add_link = TRUE,
                                                       add_alpha = TRUE)



testthat::test_that(
  "Default method gives information for fastzeroinfl objects",
  testthat::expect_equal(
    as.character(stats_bis_zeroinfl_poisson$val),
    as.character(stats_bis_zeroinfl_poisson_strip$val)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, count distribution added but no selection distribution",
  testthat::expect_equal(
    as.character(stats_bis_zeroinfl_poisson_strip[grepl(x = as.character(stats_bis_zeroinfl_poisson_strip$stat),
                                                       pattern = "(Count|Selection)"),'val']),
    c("Poisson", 'Logit')
  )
)


testthat::test_that(
  "If you add argument add_alpha = TRUE, dispersion parameter is returned",{
    testthat::expect_equal(
      length(as.character(stats_bis_zeroinfl_poisson_strip[grepl(x = as.character(stats_bis_zeroinfl_poisson_strip$stat),
                                                                pattern = "alpha"),'stat'])
      ),
      1L
    )
    testthat::expect_equal(
      as.character(
        stats_bis_zeroinfl_poisson_strip[grepl(x = as.character(stats_bis_zeroinfl_poisson_strip$stat),
                              pattern = "alpha"),'val']
      ),
      ""
    )
  }
)


# B/ CHECK STATISTICS VALUES ++++++


testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_bis_zeroinfl_poisson_strip[stats_bis_zeroinfl_poisson_strip$stat == "Observations","val"])),
      stats::nobs(zeroinfl_poisson)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson_strip[stats_bis_zeroinfl_poisson_strip$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(zeroinfl_poisson)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson_strip[stats_bis_zeroinfl_poisson_strip$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(zeroinfl_poisson)/stats::nobs(zeroinfl_poisson)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )


  }

)


testthat::test_that(
  "'BIC' field is OK",{

  # tO DO
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

