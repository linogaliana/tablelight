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
