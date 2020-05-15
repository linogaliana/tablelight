ols <- lm(Sepal.Width ~ Sepal.Length, data = iris)

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

multinom <- nnet::multinom(y ~ x1 + x2 + z + I(z^2), data=dataset)


# JUST ONE MODEL ---------------

# MODEL
testthat::expect_identical(
  ncolsmodels(ols), 1L
)

testthat::expect_identical(
  ncolsmodels(ols), 1L
)

testthat::expect_equal(
  ncolsmodels(multinom), length(unique(dataset$y))-1
)



# SEVERAL MODELS --------------

testthat::expect_identical(
  ncolsmodels(list(ols, ols, ols)), 3L
)


testthat::expect_equal(
  ncolsmodels(list(multinom, multinom)), 2*(length(unique(dataset$y))-1)
)
