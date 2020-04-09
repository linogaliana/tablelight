# stats.var.separate (when length 1) ==========

data("bioChemists", package = "pscl")
fm_zip <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                           dist = "negbin"))

latex_table <- light_table(list(fm_zip, fm_zip), modeltype = c("selection","outcome"),
                           dep.var.labels = c("Selection","Outcome"),
                           stats.var.separate = 2L,
                           stats.list = c("n","ll","lln","bic","alpha","link"))

html_table <- light_table(list(fm_zip, fm_zip), modeltype = c("selection","outcome"),
                          dep.var.labels = c("Selection","Outcome"),
                          stats.var.separate = 2L,
                          stats.list = c("n","ll","lln","bic","alpha","link"),
                          type = "html")


testthat::test_that("Summary statistics on two columns",{

  # ALPHA STATISTICS
  alpha_row <- stringr::str_split(
    latex_table[grepl("\\\\alpha", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(alpha_row), 2L)
  testthat::expect_equal(alpha_row[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 format(1/exp(fm_zip$coefficients$count['Log(theta)','Estimate']),
                                                        digits = 3L, nsmall = 3L)))

  # COUNT DISTRIBUTION
  count_row <- stringr::str_split(
    latex_table[grepl("Count distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(count_row), 2L)
  testthat::expect_equal(count_row[,2L], sprintf(" \\multicolumn{2}{c}{Negative Binomial} \\\\"))

  # LINK DISTRIBUTION
  link_row <- stringr::str_split(
    latex_table[grepl("Selection distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_row), 2L)
  testthat::expect_equal(link_row[,2L], sprintf(" \\multicolumn{2}{c}{Logit} \\\\"))

  # OBSERVATIONS
  link_obs <- stringr::str_split(
    latex_table[grepl("Observations", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_obs), 2L)
  testthat::expect_equal(link_obs[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                format(fm_zip$n, big.mark = ",")))


  # OBSERVATIONS
  link_llk <- stringr::str_split(
    latex_table[grepl("Log likelihood &", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk), 2L)
  testthat::expect_equal(link_llk[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zip$loglik, big.mark = ",", digits = 0L)))
  )

  # OBSERVATIONS (BY OBS)
  link_llk2 <- stringr::str_split(
    latex_table[grepl("Log likelihood \\(by obs.\\)", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk2), 2L)
  testthat::expect_equal(link_llk2[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 gsub("-","$-$",
                                                      format(fm_zip$loglik/fm_zip$n,
                                                             big.mark = ",", digits = 3L,
                                                             nsmall = 3)))
  )

  # BIC
  link_bic <- stringr::str_split(
    latex_table[grepl("Bayesian information criterion", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_bic), 2L)
  testthat::expect_equal(link_bic[,2L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zip$bic, big.mark = ",", digits = 0L)))
  )


})



testthat::test_that("Summary statistics on two columns",{

  # ALPHA STATISTICS
  alpha_row <- html_table[grepl("alpha", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", alpha_row))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(1/exp(fm_zip$coefficients$count['Log(theta)','Estimate']),
                     digits = 3L, nsmall = 3L)
      ), alpha_row)
  )

  # COUNT DISTRIBUTION
  count_row <- html_table[grepl("Count distribution", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", count_row))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              "Negative Binomial"
      ), count_row)
  )

  # LINK DISTRIBUTION
  link_row <- html_table[grepl("Selection distribution", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_row))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              "Logit"
      ), link_row)
  )


  # OBSERVATIONS
  link_obs <- html_table[grepl("Observations", html_table)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_obs))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(fm_zip$n,big.mark = ",")
      ), link_obs)
  )


  # OBSERVATIONS (BY OBS)
  link_llk2 <- html_table[grepl("Log likelihood \\(by obs\\.\\)", html_table, perl = TRUE)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_llk2))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(fm_zip$loglik/fm_zip$n,
                     big.mark = ",", digits = 3L,
                     nsmall = 3)), link_llk2
    )
  )

  # BIC
  link_bic <- html_table[grepl("Bayesian information criterion", html_table, perl = TRUE)]
  testthat::expect_true(grepl("<td colspan=\"2\">", link_bic))
  testthat::expect_true(
    grepl(
      sprintf("<td colspan=\"2\">%s</td></tr>",
              format(fm_zip$bic, big.mark = ",", digits = 0L)
      ), link_bic
    ))



})



# stats.var.separate (when length > 1) ==========

data("bioChemists", package = "pscl")
fm_zip <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                           dist = "poisson"))
fm_zinb <- tablelight::strip(pscl::zeroinfl(art ~ . | ., data = bioChemists,
                                            dist = "negbin"))

latex_table <- tablelight::light_table(list(fm_zip, fm_zip,
                                            fm_zinb, fm_zinb),
                                       modeltype = c("selection","outcome","selection","outcome"),
                                       stats.list = c("n","ll","lln","bic","alpha","link"),
                                       stats.var.separate = c(2L, 2L)
)

html_table <- tablelight::light_table(list(fm_zip, fm_zip,
                                           fm_zinb, fm_zinb),
                                      type = "html",
                                      modeltype = c("selection","outcome","selection","outcome"),
                                      stats.list = c("n","ll","lln","bic","alpha","link"),
                                      stats.var.separate = c(1L, 2L)
)



testthat::test_that("Summary statistics on two columns",{

  # ALPHA STATISTICS
  alpha_row <- stringr::str_split(
    latex_table[grepl("\\\\alpha", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(alpha_row), 3L)
  testthat::expect_equal(alpha_row[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                 ""))
  testthat::expect_equal(alpha_row[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 format(1/exp(fm_zinb$coefficients$count['Log(theta)','Estimate']),
                                                        digits = 3L, nsmall = 3L)))

  # COUNT DISTRIBUTION
  count_row <- stringr::str_split(
    latex_table[grepl("Count distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(count_row), 3L)
  testthat::expect_equal(count_row[,2L], sprintf(" \\multicolumn{2}{c}{Poisson} "))
  testthat::expect_equal(count_row[,3L], sprintf(" \\multicolumn{2}{c}{Negative Binomial} \\\\"))

  # LINK DISTRIBUTION
  link_row <- stringr::str_split(
    latex_table[grepl("Selection distribution", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_row), 3L)
  testthat::expect_equal(link_row[,2L], sprintf(" \\multicolumn{2}{c}{Logit} "))
  testthat::expect_equal(link_row[,3L], sprintf(" \\multicolumn{2}{c}{Logit} \\\\"))

  # OBSERVATIONS
  link_obs <- stringr::str_split(
    latex_table[grepl("Observations", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_obs), 3L)
  testthat::expect_equal(link_obs[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                format(fm_zip$n, big.mark = ",")))
  testthat::expect_equal(link_obs[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                format(fm_zinb$n, big.mark = ",")))


  # LOGLIKELIHHOD
  link_llk <- stringr::str_split(
    latex_table[grepl("Log likelihood &", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk), 3L)
  testthat::expect_equal(link_llk[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                gsub("-","$-$",
                                                     format(fm_zip$loglik, big.mark = ",", digits = 0L)))
  )
  testthat::expect_equal(link_llk[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zinb$loglik, big.mark = ",", digits = 0L)))
  )

  # OBSERVATIONS (BY OBS)
  link_llk2 <- stringr::str_split(
    latex_table[grepl("Log likelihood \\(by obs.\\)", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_llk2), 3L)
  testthat::expect_equal(link_llk2[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                 gsub("-","$-$",
                                                      format(fm_zip$loglik/fm_zip$n,
                                                             big.mark = ",", digits = 3L,
                                                             nsmall = 3)))
  )
  testthat::expect_equal(link_llk2[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                 gsub("-","$-$",
                                                      format(fm_zinb$loglik/fm_zinb$n,
                                                             big.mark = ",", digits = 3L,
                                                             nsmall = 3)))
  )

  # BIC
  link_bic <- stringr::str_split(
    latex_table[grepl("Bayesian information criterion", latex_table, perl = TRUE)],
    "&",
    simplify = TRUE)
  testthat::expect_equal(ncol(link_bic), 3L)
  testthat::expect_equal(link_bic[,2L], sprintf(" \\multicolumn{2}{c}{%s} ",
                                                gsub("-","$-$",
                                                     format(fm_zip$bic, big.mark = ",", digits = 0L)))
  )
  testthat::expect_equal(link_bic[,3L], sprintf(" \\multicolumn{2}{c}{%s} \\\\",
                                                gsub("-","$-$",
                                                     format(fm_zinb$bic, big.mark = ",", digits = 0L)))
  )


})



testthat::test_that("alpha: empty col1, empty col2-col3 (because Poisson in col2), non empty in col4",{

  # ALPHA STATISTICS
  alpha_row <- html_table[grepl("alpha", html_table, perl = TRUE)]
  alpha_row <- as.character(
    stringr::str_split(alpha_row, pattern = "</td>", simplify = TRUE)
  )
  alpha_row[1:(length(alpha_row)-1)] <- paste0(alpha_row[1:(length(alpha_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    alpha_row[2],
    "<td colspan=\"1\"></td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    alpha_row[3],
    "<td colspan=\"2\"></td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    alpha_row[4],
    sprintf("<td colspan=\"1\">%s</td>", format(1/exp(fm_zinb$coefficients$count['Log(theta)','Estimate']),
                                                digits = 3L, nsmall = 3L))
  )

})


testthat::test_that("count distrib: Poisson col1, Poisson col2-col3 (because Poisson in col2), Negative Binomial in col4",{

  # ALPHA STATISTICS
  count_row <- html_table[grepl("Count distribution", html_table, perl = TRUE)]
  count_row <- as.character(
    stringr::str_split(count_row, pattern = "</td>", simplify = TRUE)
  )
  count_row[1:(length(count_row)-1)] <- paste0(count_row[1:(length(count_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    count_row[2],
    "<td colspan=\"1\">Poisson</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[3],
    "<td colspan=\"2\">Poisson</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[4],
    "<td colspan=\"1\">Negative Binomial</td>"
  )

})


testthat::test_that("count distrib: Logit col1, Logit col2-col3 (because Logit in col2), Logit in col4",{

  # ALPHA STATISTICS
  count_row <- html_table[grepl("Selection distribution", html_table, perl = TRUE)]
  count_row <- as.character(
    stringr::str_split(count_row, pattern = "</td>", simplify = TRUE)
  )
  count_row[1:(length(count_row)-1)] <- paste0(count_row[1:(length(count_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    count_row[2],
    "<td colspan=\"1\">Logit</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[3],
    "<td colspan=\"2\">Logit</td>"
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    count_row[4],
    "<td colspan=\"1\">Logit</td>"
  )

})


testthat::test_that("observations consistent with models",{

  # ALPHA STATISTICS
  obs_row <- html_table[grepl("Observations", html_table, perl = TRUE)]
  obs_row <- as.character(
    stringr::str_split(obs_row, pattern = "</td>", simplify = TRUE)
  )
  obs_row[1:(length(obs_row)-1)] <- paste0(obs_row[1:(length(obs_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    obs_row[2],
    sprintf("<td colspan=\"1\">%s</td>", fm_zip$n)
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    obs_row[3],
    sprintf("<td colspan=\"2\">%s</td>", fm_zip$n)
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    obs_row[4],
    sprintf("<td colspan=\"1\">%s</td>", fm_zinb$n)
  )

})


testthat::test_that("loglikelihood consistent with models",{

  # ALPHA STATISTICS
  llk_row <- html_table[grepl("Log likelihood<", html_table, perl = TRUE)]
  llk_row <- as.character(
    stringr::str_split(llk_row, pattern = "</td>", simplify = TRUE)
  )
  llk_row[1:(length(llk_row)-1)] <- paste0(llk_row[1:(length(llk_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    llk_row[2],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zip$loglik, big.mark = ",", digits = 0L))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[3],
    sprintf("<td colspan=\"2\">%s</td>", format(fm_zip$loglik, big.mark = ",", digits = 0L))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[4],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zinb$loglik, big.mark = ",", digits = 0L))
  )

})



testthat::test_that("loglikelihood (by obs) consistent with models",{

  # ALPHA STATISTICS
  llk_row <- html_table[grepl("Log likelihood \\(by obs\\.\\)", html_table, perl = TRUE)]
  llk_row <- as.character(
    stringr::str_split(llk_row, pattern = "</td>", simplify = TRUE)
  )
  llk_row[1:(length(llk_row)-1)] <- paste0(llk_row[1:(length(llk_row)-1)], "</td>")

  testthat::expect_equal(#Poisson (col1): empty alpha
    llk_row[2],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zip$loglik/fm_zip$n,
                                                big.mark = ",", digits = 3L,
                                                nsmall = 3))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[3],
    sprintf("<td colspan=\"2\">%s</td>", format(fm_zip$loglik/fm_zip$n,
                                                big.mark = ",", digits = 3L,
                                                nsmall = 3))
  )

  testthat::expect_equal(#Poisson (col2-col3): empty alpha
    llk_row[4],
    sprintf("<td colspan=\"1\">%s</td>", format(fm_zinb$loglik/fm_zinb$n,
                                                big.mark = ",", digits = 3L,
                                                nsmall = 3))
  )

})

