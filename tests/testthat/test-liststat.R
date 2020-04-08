testthat::context("liststat return the expected list of summary statistics")


# 1. OLS ------------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

stats_ols <- tablelight:::liststats(ols)
stats_ols_bis <- tablelight::liststats(ols, add_link = TRUE)

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
  "add_link = TRUE equivalent to link in stats.list",
  testthat::expect_identical(
    stats_ols_bis,
    tablelight::liststats(ols, stats.list = c("n","lln","bic","link"))
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
  "'Log likelihood' field is OK (when asked)",{

    # Not by default
    testthat::expect_equal(
      sum(stats_ols_bis$stat == "Log likelihood"), 0L
    )


    testthat::expect_equal(
      as.character(tablelight::liststats(ols, stats.list = "ll")[,"val"]),
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

stats_glm <- tablelight::liststats(glm)
stats_glm_bis <- tablelight::liststats(glm, add_link = TRUE)


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

testthat::test_that(
  "add_link = TRUE equivalent to 'link' in stats.list",
  testthat::expect_equal(
    as.character(
      stats_glm_bis[grepl(x = as.character(stats_glm_bis$stat),
                          pattern = "(Count|Selection)"),'val']
  ),
  as.character(tablelight::liststats(glm, stats.list = "link")[,'val'])
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
  "'Log likelihood' field is OK (when asked)",{

    # Not by default
    testthat::expect_equal(
      sum(stats_glm_bis$stat == "Log likelihood"), 0L
    )


    testthat::expect_equal(
      as.character(tablelight::liststats(glm, stats.list = "ll")[,"val"]),
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

stats_glmnb <- tablelight::liststats(glmnb, stats.list = c("n","ll","lln","bic"))
stats_glmnb_bis <- tablelight::liststats(glmnb, add_link = TRUE, stats.list = c("n","ll","lln","bic"),
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
  "add_link = TRUE or stat = 'link' equivalent",
  testthat::expect_equal(
    as.character(stats_glmnb_bis[grepl(x = as.character(stats_glmnb_bis$stat),
                                       pattern = "(Count|Selection)"),'val']),
    as.character(tablelight::liststats(glmnb, stats.list = c("link"))[,'val'])
  )
)

testthat::test_that(
  "add_link = FALSE or stat = 'link' still produces link stat",
  testthat::expect_equal(
    as.character(stats_glmnb_bis[grepl(x = as.character(stats_glmnb_bis$stat),
                                       pattern = "(Count|Selection)"),'val']),
    as.character(tablelight::liststats(glmnb, add_link = FALSE, stats.list = c("link"))[,'val'])
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


testthat::test_that(
  "add_alpha = TRUE equivalent to stats.list = 'alpha' ",{
    testthat::expect_equal(
      as.character(
        stats_glmnb_bis[grepl(x = as.character(stats_glmnb_bis$stat),
                              pattern = "alpha"),'val']
      ),
      as.character(
        tablelight::liststats(glmnb, stats.list = c("alpha"))[,'val']
      )
    )
  }
)

testthat::test_that(
  "add_alpha = FALSE ignored if stats.list = 'alpha' ",{
    testthat::expect_equal(
      tablelight::liststats(glmnb, add_alpha = FALSE, stats.list = c("alpha")),
      tablelight::liststats(glmnb, add_alpha = TRUE, stats.list = c("alpha"))
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
zeroinfl_negbin_strip <- tablelight::strip(zeroinfl_negbin)


stats_bis_zeroinfl_negbin <- tablelight::liststats(zeroinfl_negbin,
                                                   stats.list = c("n","ll","lln","bic"),
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
  "add_link = TRUE equivalent to stat = 'link'",
  testthat::expect_equal(
    as.character(stats_bis_zeroinfl_negbin[grepl(x = as.character(stats_bis_zeroinfl_negbin$stat),
                                                 pattern = "(Count|Selection)"),'val']),
    as.character(tablelight::liststats(zeroinfl_negbin,
                          stats.list = c("link"),
                          add_link = TRUE)[,'val'])
    )
)

testthat::test_that(
  "add_link = TRUE equivalent to stat = 'link'",
  testthat::expect_identical(
    tablelight::liststats(zeroinfl_negbin,
                          stats.list = c("link"),
                          add_link = TRUE),
    tablelight::liststats(zeroinfl_negbin,
                          stats.list = c("link"),
                          add_link = FALSE)
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


stats_zeroinfl_poisson <- tablelight::liststats(zeroinfl_poisson)
stats_bis_zeroinfl_poisson <- tablelight::liststats(zeroinfl_poisson, add_link = TRUE,
                                                    stats.list = c("n","ll","lln","bic"),
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

light_ols <- tablelight::strip(ols)

stats_ols <- tablelight:::liststats(ols, add_link = TRUE, stats.list = c("n","ll","lln","bic"))
stats_ols_strip <- tablelight:::liststats(light_ols, add_link = TRUE,
                                          stats.list = c("n","ll","lln","bic"))


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
  "add_link = TRUE equivalent to stat.list 'link'",
  testthat::expect_equal(
    stats_ols_strip[grepl("(Count|Selection)", stats_ols_strip$stat),],
    liststats(light_ols, add_link = TRUE,
              stats.list = c("link"))
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

light_glm <- tablelight::strip(glm)

stats_glm <- tablelight::liststats(glm, add_link = TRUE, add_alpha = TRUE,
                                   list.stats = c("n","ll","lln","bic"))
stats_glm_strip <- tablelight::liststats(light_glm, add_link = TRUE,
                                         add_alpha = TRUE,
                                         list.stats = c("n","ll","lln","bic"))


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
glmnb_light <- tablelight::strip(glmnb)

# 7.A. CHECK STATISTICS RETURNED ======

stats_glmnb <- tablelight::liststats(glmnb)
stats_glmnb_bis <- tablelight::liststats(glmnb, add_link = TRUE,
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














# 8. FAST GLM.NB --------------------

requireNamespace("gravity", quietly = TRUE)

quine <- MASS::quine

glmnb <- gravity::fastglm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
# glmnb_light <- tablelight::strip(glmnb)


# 8.A. CHECK STATISTICS RETURNED ======

stats_glmnb <- tablelight::liststats(glmnb)
stats_glmnb_bis <- tablelight::liststats(glmnb_light, add_link = TRUE,
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
stats_bis_zeroinfl_negbin <- tablelight::liststats(zeroinfl_negbin,
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


stats_zeroinfl_poisson <- tablelight::liststats(zeroinfl_poisson)
stats_bis_zeroinfl_poisson <- tablelight::liststats(zeroinfl_poisson, add_link = TRUE,
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
zeroinfl_negbin_strip <- tablelight::strip(zeroinfl_negbin)


stats_bis_zeroinfl_negbin <- tablelight::liststats(zeroinfl_negbin, add_link = TRUE,
                                                   add_alpha = TRUE)
stats_bis_zeroinfl_negbin_strip <- tablelight::liststats(zeroinfl_negbin_strip, add_link = TRUE,
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
zeroinfl_poisson_strip <- tablelight::strip(zeroinfl_poisson)


stats_bis_zeroinfl_poisson <- tablelight::liststats(zeroinfl_poisson, add_link = TRUE,
                                                    add_alpha = TRUE)
stats_bis_zeroinfl_poisson_strip <- tablelight::liststats(zeroinfl_poisson_strip, add_link = TRUE,
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

    testthat::expect_equal(
      as.character(stats_bis_zeroinfl_poisson_strip[stats_bis_zeroinfl_poisson_strip$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(zeroinfl_poisson), digits = 0L, big.mark = ",")
    )


  }

)






# 11. OGLMX OBJECTS -------------------


# CREATE OGLMX OBJECT (FROM THE DOC)

n<-250
x1<-sample(c(0,1),n,replace=TRUE,prob=c(0.75,0.25))
x2<-vector("numeric",n)
x2[x1==0]<-sample(c(0,1),n-sum(x1==1),replace=TRUE,prob=c(2/3,1/3))
z<-rnorm(n,0.5)
# create latent outcome variable
latenty<-0.5+1.5*x1-0.5*x2+0.5*z+rnorm(n,sd=exp(0.5*x1-0.5*x2))
# observed y has four possible values: -1,0,1,2
# threshold values are: -0.5, 0.5, 1.5.
y<-vector("numeric",n)
y[latenty< -0.5]<--1
y[latenty>= -0.5 & latenty<0.5]<- 0
y[latenty>= 0.5 & latenty<1.5]<- 1
y[latenty>= 1.5]<- 2
dataset<-data.frame(y,x1,x2)


oglm <- oglmx::oglmx(y ~ x1 + x2 + z, data=dataset,link="probit",constantMEAN=FALSE,
                     constantSD=FALSE,delta=0,threshparam=NULL)

stats_oglm <- tablelight::liststats(oglm)
stats_oglm_bis <- tablelight::liststats(oglm, add_link = TRUE)


# 2.A. CHECK STATISTICS RETURNED ======

testthat::test_that(
  "Default method gives information for oglm",
  testthat::expect_equal(
    nrow(na.omit(stats_oglm)),
    nrow(stats_oglm)
  )
)

testthat::test_that(
  "If you add argument add_link = TRUE, empty fields for count and selection distribution",
  testthat::expect_equal(
    tolower(
      as.character(stats_oglm_bis[grepl(x = as.character(stats_oglm_bis$stat),
                                        pattern = "(Count|Selection)"),'val'])
    ),
    c('', '')
  )
)


## 2.B. CHECK STATISTICS VALUES ======

testthat::test_that(
  "'Observations' field is OK",{

    testthat::expect_equal(
      as.numeric(as.character(stats_oglm_bis[stats_oglm_bis$stat == "Observations","val"])),
      stats::nobs(oglm)
    )

    testthat::expect_equal(
      as.numeric(as.character(stats_oglm[stats_oglm$stat == "Observations","val"])),
      stats::nobs(oglm)
    )

  }

)


testthat::test_that(
  "'Log likelihood' field is OK",{

    testthat::expect_equal(
      as.character(stats_oglm_bis[stats_oglm_bis$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(oglm)), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_oglm[stats_oglm$stat == "Log likelihood","val"]),
      format(as.numeric(stats::logLik(oglm)), digits = 0L, big.mark = ",")
    )

  }

)


testthat::test_that(
  "'Log likelihood (by obs.)' field is OK",{

    testthat::expect_equal(
      as.character(stats_oglm_bis[stats_oglm_bis$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(oglm)/stats::nobs(oglm)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_oglm[stats_oglm$stat == "Log likelihood (by obs.)","val"]),
      format(as.numeric(stats::logLik(oglm)/stats::nobs(oglm)), digits = 3L, nsmall = 3L,
             big.mark = ",")
    )

  }

)


testthat::test_that(
  "'BIC' field is OK",{

    testthat::expect_equal(
      as.character(stats_oglm_bis[stats_oglm_bis$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(oglm), digits = 0L, big.mark = ",")
    )

    testthat::expect_equal(
      as.character(stats_oglm[stats_oglm$stat == "Bayesian information criterion","val"]),
      format(stats::BIC(oglm), digits = 0L, big.mark = ",")
    )

  }

)



