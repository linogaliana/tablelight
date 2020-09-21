testthat::context('add rules')

vect <- paste0(rep(c("row_coeff", "row_sd", "row_empty"), 10), unlist(lapply(1:10,rep,3)))


testthat::expect_equal(
  add_rules(body_table = vect,
            rules_between_covariates = NULL,
            type = "latex", ncols_models = 1L),
  vect
)


testthat::expect_equal(
  add_rules(body_table = vect,
            rules_between_covariates = c(1,2,5),
            type = "latex", ncols_models = 1L),
  paste0(c(rep("",3),
           c(" \\hline \\\\[-1.8ex] ", rep("",2)),
           c(" \\hline \\\\[-1.8ex] ", rep("",2)),
           rep("",3*2),
           c(" \\hline \\\\[-1.8ex] ", rep("",2)),
           rep("",3*3)
  ),
  vect)
)


testthat::test_that("ncols_models ignored in latex tables", {
  testthat::expect_equal(
    add_rules(body_table = vect,
              rules_between_covariates = c(1,2,5),
              type = "latex", ncols_models = 2L),
    add_rules(body_table = vect,
              rules_between_covariates = c(1,2,5),
              type = "latex", ncols_models = 10L),
  )
})


add_rules(body_table = vect,
          rules_between_covariates = c(1,2,5),
          type = "html", ncols_models = 2L)

testthat::test_that("ncols_models plugged-in in html tables", {
  testthat::expect_equal(
    add_rules(body_table = vect,
              rules_between_covariates = c(1,2,5),
              type = "html", ncols_models = 1L),
    paste0(c(rep("",3),
             c("<tr><td colspan=\"2\"style=\"border-bottom: 1px solid black\"></td></tr>", rep("",2)),
             c("<tr><td colspan=\"2\"style=\"border-bottom: 1px solid black\"></td></tr>", rep("",2)),
             rep("",3*2),
             c("<tr><td colspan=\"2\"style=\"border-bottom: 1px solid black\"></td></tr>", rep("",2)),
             rep("",3*3)
    ),
    vect)
  )
  testthat::expect_equal(
    add_rules(body_table = vect,
              rules_between_covariates = c(1,2,5),
              type = "html", ncols_models = 12L),
    paste0(c(rep("",3),
             c("<tr><td colspan=\"13\"style=\"border-bottom: 1px solid black\"></td></tr>", rep("",2)),
             c("<tr><td colspan=\"13\"style=\"border-bottom: 1px solid black\"></td></tr>", rep("",2)),
             rep("",3*2),
             c("<tr><td colspan=\"13\"style=\"border-bottom: 1px solid black\"></td></tr>", rep("",2)),
             rep("",3*3)
    ),
    vect)
  )
})
