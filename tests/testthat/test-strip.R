context("strip method has expected behavior for every class")


# 1. LM -------------------

df = data.frame(y = rnorm(100L),
                x = rnorm(100L))

object <- lm(y ~  x, df)
object_light <- strip(object)
# summary_light <- summary(object_light)

testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), paste0("light.", class(object)))
  )
})

# testthat::test_that('stripped summary is just summary without dev.res', {
#   testthat::expect_equal(
#     names(summary(object))[(names(summary(object)) != "deviance.resid")],
#     names(summary_light)
#   )
# })


testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    "Gaussian"
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    ""
  )
})

testthat::test_that("R2 and Adjusted R2 fields same than summary(object)", {

  testthat::expect_equal(
    object_light$rsq,
    summary(object)$r.squared
  )
  testthat::expect_equal(
    object_light$adj.rsq,
    summary(object)$adj.r.squared
  )

})


# 2. GLM -------------------------

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
object <- glm(counts ~ outcome + treatment, family = poisson())

object_light <- strip(object)
summary_light <- strip(summary(object))

testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), "light.glm"))
})

testthat::test_that('stripped summary is just summary without dev.res', {
  testthat::expect_equal(
    names(summary(object))[(names(summary(object)) != "deviance.resid")],
    names(summary_light)
  )
})


testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    "Poisson"
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    ""
  )
})


# 3. NEGBIN ----------------------

object <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine)
object_light <- strip(object)


testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), paste0("light.", class(object))))
})


testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    "Negative Binomial"
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    ""
  )
})


# 4. OGLMX --------------------

set.seed(242)
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

object <- oglmx::oglmx(y ~ x1 + x2 + z, data=dataset,link="probit",constantMEAN=FALSE,
             constantSD=FALSE,delta=0,threshparam=NULL)
object_light <- strip(object)
summary_light <- summary(object)

testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), paste0("light.", class(object))))
})


testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    ""
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    ""
  )
})




# 5. ZEROINFL -------------------

data("bioChemists", package = "pscl")

object <- pscl::zeroinfl(art ~ . | 1, data = bioChemists)
object_light <- strip(object)
summary_light <- strip(summary(object))


testthat::test_that("New class light.", {
  testthat::expect_equal(class(object_light),
                         c(class(object), paste0("light.", class(object))))
})


testthat::test_that('stripped summary is just summary without dev.res', {
  testthat::expect_equal(
    names(summary(object))[!(names(summary(object)) %in% c("weights","residuals", "fitted.values"))],
    names(summary_light)
  )
})



testthat::test_that("Coefficients field is same than summary(object)$coefficients", {
  testthat::expect_equal(
    object_light$coefficients,
    summary(object)$coefficients
  )
})

testthat::test_that("Observations field is same than nobs(.)", {
  testthat::expect_equal(
    object_light$n,
    nobs(object)
  )
})

testthat::test_that("loglikelihood field is same than Loglik(.)", {
  testthat::expect_equal(
    object_light$loglikelihood,
    as.numeric(logLik(object))
  )
})

testthat::test_that("bic field is same than BIC(.)", {
  testthat::expect_equal(
    object_light$bic,
    BIC(object)
  )
})

testthat::test_that("link_count is 'Gaussian'", {
  testthat::expect_equal(
    object_light$link_count,
    Hmisc::capitalize(object$dist)
  )
})

testthat::test_that("link_selection is empty", {
  testthat::expect_equal(
    object_light$link_selection,
    Hmisc::capitalize(object$link)
  )
})




