testthat::context("sigma.oglmx apply sigma method to objects of class oglmx")


# GENERATE DATAFRAME ---------------------


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


# WITHOUT THRESHOLDS ----------------------

# delta = 0 : variance is one in any case

testthat::test_that("With delta = 0, we have a variance = 1", {
  testthat::expect_equal(
    sigma(oglmx::oglmx(y ~ x1 + x2 + z, data=dataset,
                 link="probit",
                 constantMEAN=FALSE,
                 constantSD=FALSE,
                 delta=0,threshparam=NULL)),
    1L
  )
})


testthat::test_that("With delta = 0, we have a variance = 1 (newdata ignored)", {
  testthat::expect_equal(
    sigma(oglmx::oglmx(y ~ x1 + x2 + z, data=dataset,
                       link="probit",
                       constantMEAN=FALSE,
                       constantSD=FALSE,
                       delta=0,threshparam=NULL), newdata = dataset),
    1L
  )
})

# estimate ordered probit with homoscedasticity  sigma = exp(est_sigma_sd)

oglmx2 <- oglmx::oglmx(y ~ x1 + x2, data=dataset, link="probit",
                                 constantMEAN=FALSE, constantSD=FALSE,threshparam=NULL)


testthat::test_that("With homoscedasticity, we have a variance = exp(constant_sd) [vector form]", {
  testthat::expect_equal(
    as.numeric(sigma(oglmx2)),
    as.numeric(rep(exp(oglmx2$allparams$delta), nrow(dataset)))
  )
})


testthat::test_that("With homoscedasticity, we have a variance = exp(constant_sd) [vector form]", {
  testthat::expect_equal(
    as.numeric(sigma(oglmx2, newdata = dataset[1:10,])),
    as.numeric(rep(exp(oglmx2$allparams$delta), 10L))
  )
})


# estimate ordered probit with heteroskedasticity

results.oprobhet <- oglmx::oglmx(y ~ x1 + x2 + z, ~ x1 + x2, data=dataset, link="probit",
                        constantMEAN=FALSE, constantSD=FALSE,threshparam=NULL)
