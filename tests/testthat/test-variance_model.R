testthat::context("variance_model produces the expected matrix")


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


# ERROR ------------------

testthat::expect_error(variance_model(lm(y ~  x1, data = dataset)),
                       "a 'oglmx' object")


# WITHOUT NEWDATA

oglm <- oglmx::oglmx(y ~ x1 + x2 + z, ~ x1 + x2, data=dataset, link="probit",
                       constantMEAN=FALSE, constantSD=FALSE,threshparam=NULL)


testthat::test_that("Without newdata, returns the variance matrix",{
  testthat::expect_equal(
    variance_model(oglm)[,-c(1)],
    oglm$modelframes$Z
  )
})

# WITH SAME NEWDATA THAN MODEL

testthat::test_that("Same newdata than data, returns the variance matrix",{
  testthat::expect_equal(
    variance_model(oglm),
    variance_model(oglm, newdata = dataset)
  )
})


# WITH NEWDATA SMALLER THAN ORIGINAL


testthat::test_that("Newdata, returns the variance matrix",{
  testthat::expect_equal(
    data.frame(variance_model(oglm)[1:10,]),
    data.frame(variance_model(oglm, newdata = dataset[1:10,]))
  )
})
