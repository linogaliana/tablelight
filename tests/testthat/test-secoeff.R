testthat::context("se_coeff methods return coefficients s.e., t stat and pvalues")


# 1. OLS -------------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)


testthat::expect_equal(
  secoeff(ols),
  summary(ols)$coefficients
)

testthat::expect_equal(
  secoeff(strip(ols)),
  summary(ols)$coefficients
)

# 2. OGLM -------------------

iris$y_r <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y_r ~ Sepal.Length ,
  data = iris
)


testthat::expect_equal(
  tablelight::secoeff(oglm),
  summary(oglm)$estimate
)


# 3. NEGBIN ----------

quine.nb1 <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine)


testthat::expect_equal(
  tablelight:::secoeff(quine.nb1),
  summary(quine.nb1)$coefficients
)


# 4. secoeff.light.zeroinfl ------------

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- tablelight::strip(zeroinfl_negbin)

testthat::expect_equal(
  secoeff(zeroinfl_negbin_strip, modeltype = "count"),
  secoeff(zeroinfl_negbin)$count
)

testthat::expect_equal(
  secoeff(zeroinfl_negbin_strip, modeltype = "selection"),
  secoeff(zeroinfl_negbin)$zero
)


# 5. secoeff.fastzeroinfl

# Twist to use functions with gravity installed
class(zeroinfl_negbin) <- c("fastzeroinfl", class(zeroinfl_negbin))


testthat::expect_equal(
  secoeff(zeroinfl_negbin, modeltype = "count"),
  summary(zeroinfl_negbin)$coefficients$count
)

testthat::expect_equal(
  secoeff(zeroinfl_negbin, modeltype = "selection"),
  summary(zeroinfl_negbin)$coefficients$zero
)



# 5. SUMMARY.LM ---------

df = data.frame(y = rnorm(100L),
                x = rnorm(100L))

object <- lm(y ~  x, df)

testthat::expect_equal(
  summary(object)$coefficients,
  secoeff(summary(object))
)

testthat::expect_equal(
  secoeff.summary.lm(summary(object)),
  secoeff(summary(object))
)


# 6. gravity::fastglm.nb

fastquine.nb1 <- gravity::fastglm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine)
quine.nb1 <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine)

testthat::expect_equal(
  as.numeric(tablelight:::secoeff(quine.nb1)[,'Estimate']),
  as.numeric(tablelight:::secoeff(fastquine.nb1)[,'Estimate'])
)
# testthat::expect_equal(
#   as.numeric(tablelight:::secoeff(quine.nb1)[,'Std. Error']),
#   as.numeric(tablelight:::secoeff(fastquine.nb1)[,'Std. Error'])
# )



# 7 - nnet::multinom --------------

requireNamespace("nnet", quietly = TRUE)

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

coeffs <- secoeff(logit)

testthat::test_that("Return a list with 4 elements", {

  testthat::expect_identical(
    class(coeffs), "list"
  )

  testthat::expect_equal(
    names(coeffs), c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  )

})

testthat::test_that("As many cols as y modality - 1", {

  testthat::expect_equal(
    ncol(coeffs[[1]]), length(unique(dataset$y))-1
  )
  testthat::expect_equal(
    ncol(coeffs[[2]]), length(unique(dataset$y))-1
  )
  testthat::expect_equal(
    ncol(coeffs[[3]]), length(unique(dataset$y))-1
  )
  testthat::expect_equal(
    ncol(coeffs[[4]]), length(unique(dataset$y))-1
  )

})

testthat::test_that("As many rows as number variables + intercept", {

  testthat::expect_equal(
    nrow(coeffs[[1]]), 4
  )
  testthat::expect_equal(
    nrow(coeffs[[2]]), 4
  )
  testthat::expect_equal(
    nrow(coeffs[[3]]), 4
  )
  testthat::expect_equal(
    nrow(coeffs[[4]]), 4
  )

})


testthat::test_that("Values returned are correct", {
  testthat::expect_equal(
    coeffs[["Estimate"]],
    t(summary(logit)$coefficients)
  )
  testthat::expect_equal(
    coeffs[["Std. Error"]],
    t(summary(logit)$standard.errors)
  )
})


# 8: mindist -----

requireNamespace("mindist", quietly = TRUE)

n <- 1000L
ncol <- 3

mu <- 2
sd <- 2

x <- replicate(ncol, rnorm(n))

df <- data.frame(x1 = x[,1], x2 = x[,2],
                 x3 = x[,3])

df$y <- exp(1 + 2*df$x1) + rnorm(n)


# FORMALISM REQUIRED FOR OUR FUNCTIONS
moment_poisson <- function(theta, ...){
  return(
    data.table::data.table(
      'y' = df$y,
      'y_hat' = as.numeric(cbind(1L, df$x1) %*% theta),
      'epsilon' = as.numeric(df$x1*(df$y - exp(cbind(1L, df$x1) %*% theta)))
    )
  )
}


msm1 <- mindist::estimation_theta(theta_0 = c("const" = 0.1, "beta1" = 0),
                                  prediction_function = moment_poisson,
                                  approach = "two_step")


testthat::test_that("We consistently get information from mindist model", {

  testthat::expect_equal(
    secoeff(msm1)[,'Estimate'],
    as.numeric(msm1$estimates$theta_hat)
  )
  testthat::expect_equal(
    secoeff(msm1)[,'Std. Error'],
    as.numeric(msm1$estimates$se_theta_hat)
  )
  testthat::expect_equal(
    secoeff(msm1)[,'z value'],
    as.numeric(abs(msm1$estimates$theta_hat/msm1$estimates$se_theta_hat))
  )
})



