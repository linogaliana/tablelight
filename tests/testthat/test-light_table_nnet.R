# WITHOUT reference_level_position

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


logit <- nnet::multinom(y ~ x1 + x2 + z, data=dataset)


latex_table <- light_table(
  logit, type = "latex", stats.list = c("n","ll","lln","bic","link")
)



testthat::test_that("Count and Selection distributions: empty over 3 columns", {

  testthat::expect_equal(
    sum(
      grepl("Count distribution & \\\\multicolumn\\{3\\}\\{c\\}\\{\\} \\\\\\\\", latex_table,
            perl = TRUE)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl("Selection distribution & \\\\multicolumn\\{3\\}\\{c\\}\\{\\} \\\\\\\\", latex_table,
            perl = TRUE)
    ),
    1L
  )


})


testthat::test_that("Log-likelihood, Log-likelihood by obs and BIC over 3 columns", {

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Log likelihood & \\\\multicolumn\\{3\\}\\{c\\}\\{\\$-\\$%s\\} \\\\\\\\",
                abs(as.numeric(round(logLik(logit))))),
        latex_table)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Log likelihood \\(by obs\\.\\) & \\\\multicolumn\\{3\\}\\{c\\}\\{\\$-\\$%s\\} \\\\\\\\",
                format(
                  abs(as.numeric(logLik(logit)))/nrow(logit$residuals),
                  digits = 3L, nsmall = 3L)
        ),
        latex_table)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Bayesian information criterion & \\\\multicolumn\\{3\\}\\{c\\}\\{%s\\} \\\\\\\\",
                round(BIC(logit))),
        latex_table)
    ),
    1L
  )


})


# With reference_level_position -----------------

# ref level position not in last position ======

latex_table <- light_table(
  logit, type = "latex", stats.list = c("n","ll","lln","bic","link"),
  reference_level_position = 2L
)


testthat::test_that("Count and Selection distributions: empty over 4 columns", {

  testthat::expect_equal(
    sum(
      grepl("Count distribution & \\\\multicolumn\\{4\\}\\{c\\}\\{\\} \\\\\\\\", latex_table,
            perl = TRUE)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl("Selection distribution & \\\\multicolumn\\{4\\}\\{c\\}\\{\\} \\\\\\\\", latex_table,
            perl = TRUE)
    ),
    1L
  )


})


testthat::test_that("Log-likelihood, Log-likelihood by obs and BIC over 4 columns", {

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Log likelihood & \\\\multicolumn\\{4\\}\\{c\\}\\{\\$-\\$%s\\} \\\\\\\\",
                abs(as.numeric(round(logLik(logit))))),
        latex_table)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Log likelihood \\(by obs\\.\\) & \\\\multicolumn\\{4\\}\\{c\\}\\{\\$-\\$%s\\} \\\\\\\\",
                format(
                  abs(as.numeric(logLik(logit)))/nrow(logit$residuals),
                  digits = 3L, nsmall = 3L)
        ),
        latex_table)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Bayesian information criterion & \\\\multicolumn\\{4\\}\\{c\\}\\{%s\\} \\\\\\\\",
                round(BIC(logit))),
        latex_table)
    ),
    1L
  )


})




latex_table_noref <- light_table(
  logit, type = "latex", stats.list = c("n","ll","lln","bic","link")
)

body_part_noref <- latex_table_noref[grep("(Intercept)", latex_table_noref):(grep("^z", latex_table_noref) + 2)]
body_part_ref <- latex_table[grep("(Intercept)", latex_table_noref):(grep("^z", latex_table_noref) + 2)]


body_part_noref <- stringr::str_split(body_part_noref, "&", simplify = TRUE)
body_part_ref   <- stringr::str_split(body_part_ref, "&", simplify = TRUE)


testthat::test_that("Same table except for reference column", {

  testthat::expect_equal(
    cbind(
      trimws(body_part_noref[,1:2]),
      trimws(rep(c("(Ref) ", " ", " "), 4L)),
      trimws(body_part_noref[,3:4])
    ),
    trimws(body_part_ref)
  )

})



# reference position in last position ===========


latex_table2 <- light_table(
  logit, type = "latex", stats.list = c("n","ll","lln","bic","link"),
  reference_level_position = 4L
)


testthat::test_that("Count and Selection distributions: empty over 4 columns", {

  testthat::expect_equal(
    sum(
      grepl("Count distribution & \\\\multicolumn\\{4\\}\\{c\\}\\{\\} \\\\\\\\",
            latex_table2,
            perl = TRUE)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl("Selection distribution & \\\\multicolumn\\{4\\}\\{c\\}\\{\\} \\\\\\\\",
            latex_table2,
            perl = TRUE)
    ),
    1L
  )


})


testthat::test_that("Log-likelihood, Log-likelihood by obs and BIC over 4 columns", {

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Log likelihood & \\\\multicolumn\\{4\\}\\{c\\}\\{\\$-\\$%s\\} \\\\\\\\",
                abs(as.numeric(round(logLik(logit))))),
        latex_table2)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Log likelihood \\(by obs\\.\\) & \\\\multicolumn\\{4\\}\\{c\\}\\{\\$-\\$%s\\} \\\\\\\\",
                format(
                  abs(as.numeric(logLik(logit)))/nrow(logit$residuals),
                  digits = 3L, nsmall = 3L)
        ),
        latex_table2)
    ),
    1L
  )

  testthat::expect_equal(
    sum(
      grepl(
        sprintf("Bayesian information criterion & \\\\multicolumn\\{4\\}\\{c\\}\\{%s\\} \\\\\\\\",
                round(BIC(logit))),
        latex_table2)
    ),
    1L
  )


})


# latex_table_noref <- light_table(
#   logit, type = "latex",
#   stats.list = c("n","ll","lln","bic","link")
# )
#
# body_part_noref <- latex_table_noref[grep("(Intercept)", latex_table_noref):(grep("^z", latex_table_noref) + 2)]
# body_part_ref <- latex_table2[grep("(Intercept)", latex_table_noref):(grep("^z", latex_table_noref) + 2)]
#
#
# body_part_noref <- stringr::str_split(body_part_noref, "&", simplify = TRUE)
# body_part_ref   <- stringr::str_split(body_part_ref, "&", simplify = TRUE)
#
#
# testthat::test_that("Same table except for reference column", {
#
#   testthat::expect_equal(
#     cbind(
#       trimws(body_part_noref),
#       trimws(rep(c("(Ref)\\\\ ", " ", " "), 4L))
#     ),
#     trimws(body_part_ref)
#   )
#
# })
