testthat::context("extract_coeff recovers the coefficients")


library(tablelight)

# GLM -------------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)
stats_glm <- tablelight::extract_coeff(glm)

testthat::test_that("Coefficient names are consistent", {
  testthat::expect_equal(
    stats_glm[,'variable'],
    names(glm$coefficients)
  )
})

output_glm <- summary(glm)$coefficients

expected_coeff <- sapply(seq_len(nrow(output_glm)), function(i){
  paste0(format(output_glm[i, 'Estimate'], nsmall = 3L,
                digits = 2L),
         signif_stars(output_glm[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_coeff'],
    expected_coeff
  )
})


testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_sd'],
    paste0("(",
           format(output_glm[,'Std. Error'],
                  digits = 2L, nsmall = 3L
           ),
           ")")
  )
})



# LIGHT.GLM --------------------

glm <- glm(
  I(round(Sepal.Length)) ~ Sepal.Width,
  data = iris,
  family = poisson()
)

light_glm <- tablelight::strip(glm)

stats_glm <- tablelight::extract_coeff(light_glm)

testthat::test_that("Coefficient names are consistent", {
  testthat::expect_equal(
    stats_glm[,'variable'],
    names(glm$coefficients)
  )
})

output_glm <- summary(glm)$coefficients

expected_coeff <- sapply(seq_len(nrow(output_glm)), function(i){
  paste0(format(output_glm[i, 'Estimate'], nsmall = 3L,
                digits = 2L),
         signif_stars(output_glm[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_coeff'],
    expected_coeff
  )
})


testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    stats_glm[,'text_sd'],
    paste0("(",
           format(output_glm[,'Std. Error'],
                  digits = 2L, nsmall = 3L
           ),
           ")")
  )
})


# LIGHT.ZEROINFL -------------------------

data("bioChemists", package = "pscl")

zeroinfl_negbin <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
zeroinfl_negbin_strip <- tablelight::strip(zeroinfl_negbin)

testthat::test_that("When modeltype = 'missing', outcome is used", {

  testthat::expect_message(
    stats_glm <- tablelight::extract_coeff(zeroinfl_negbin_strip),
    "'modeltype' argument missing, assuming 'outcome'"
  )

  testthat::expect_identical(
    stats_glm,
    tablelight::extract_coeff(zeroinfl_negbin_strip, modeltype = "outcome")
  )
}
)

ZINB_outcome <- tablelight::extract_coeff(zeroinfl_negbin_strip, modeltype = "outcome")
ZINB_zero <- tablelight::extract_coeff(zeroinfl_negbin_strip, modeltype = "selection")

output_glm <- summary(zeroinfl_negbin)$coefficients

# COUNT MODEL =============

testthat::test_that("Coefficient names are consistent (theta not in parameter section)", {
  testthat::expect_equal(
    ZINB_outcome[,'variable'][ZINB_outcome[,'variable'] != "Log(theta)"],
    names(zeroinfl_negbin$coefficients$count)
  )
})

expected_coeff <- sapply(seq_len(nrow(output_glm$count)), function(i){
  paste0(format(round(output_glm$count[i, 'Estimate'],3L),  nsmall = 3L,
                digits = 2L, scientific = FALSE),
         signif_stars(output_glm$count[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    ZINB_outcome[,'text_coeff'],
    expected_coeff
  )
})

testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    ZINB_outcome[,'text_sd'],
    paste0("(",
           format(round(output_glm$count[,'Std. Error'], 3L),
                  digits = 2L, nsmall = 3L
           ),
           ")")
  )
})


# SELECTION MODEL =============

testthat::test_that("Coefficient names are consistent (theta not in parameter section)", {
  testthat::expect_equal(
    ZINB_zero[,'variable'][ZINB_zero[,'variable'] != "Log(theta)"],
    names(zeroinfl_negbin$coefficients$zero)
  )
})


expected_coeff <- sapply(seq_len(nrow(output_glm$zero)), function(i){
  paste0(format(round(output_glm$zero[i, 'Estimate'],3L),  nsmall = 3L,
                digits = 2L, scientific = FALSE),
         signif_stars(output_glm$zero[i,'Pr(>|z|)']))
})

testthat::test_that("Coefficient values are consistent", {
  testthat::expect_equal(
    ZINB_zero[,'text_coeff'][ZINB_zero[,'variable'] != "Log(theta)"],
    expected_coeff
  )
})


testthat::test_that("Coefficient s.e. are consistent", {
  testthat::expect_equal(
    ZINB_outcome[,'text_sd'],
    paste0("(",
           format(round(output_glm$count[,'Std. Error'], 3L),
                  digits = 2L, nsmall = 3L
           ),
           ")")
  )
})


# NNET ====================================

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

testthat::test_that("Default behavior: return all modalities", {
  testthat::expect_identical(
    extract_coeff(logit),
    extract_coeff(logit, modality = c("0","1","2"))
  )
})

testthat::test_that("All modalities returned if needed", {
  testthat::expect_identical(
    extract_coeff(logit, modality = c("0","1","2")),
    list(
      extract_coeff(logit, modality = c("0")),
      extract_coeff(logit, modality = c("1")),
      extract_coeff(logit, modality = c("2"))
    )
  )
})



models_stat <- secoeff(logit)

expected_coeff_modal0 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    trimws(
      format(round(models_stat[['Estimate']][i,"0"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),  signif_stars(models_stat[['Pr(>|z|)']][i,"0"])
  )
})
expected_coeff_modal1 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    trimws(
      format(round(models_stat[['Estimate']][i,"1"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),  signif_stars(models_stat[['Pr(>|z|)']][i,"1"])
  )
})
expected_coeff_modal2 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    trimws(
      format(round(models_stat[['Estimate']][i,"2"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),  signif_stars(models_stat[['Pr(>|z|)']][i,"2"])
  )
})
expected_sd_modal0 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    "(",
    trimws(
      format(round(models_stat[['Std. Error']][i,"0"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),
    ")"
  )
})
expected_sd_modal1 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    "(",
    trimws(
      format(round(models_stat[['Std. Error']][i,"1"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),
    ")"
  )
})
expected_sd_modal2 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    "(",
    trimws(
      format(round(models_stat[['Std. Error']][i,"2"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),
    ")"
  )
})



testthat::test_that("Coefficients are okay", {

  testthat::expect_equal(
    expected_coeff_modal0,
    extract_coeff(logit, modality = "0")[,"text_coeff"]
  )

  testthat::expect_equal(
    expected_coeff_modal1,
    extract_coeff(logit, modality = "1")[,"text_coeff"]
  )

  testthat::expect_equal(
    expected_coeff_modal2,
    extract_coeff(logit, modality = "2")[,"text_coeff"]
  )

})


testthat::test_that("Standard errors are okay", {

  testthat::expect_equal(
    expected_sd_modal0,
    extract_coeff(logit, modality = "0")[,"text_sd"]
  )

  testthat::expect_equal(
    expected_sd_modal1,
    extract_coeff(logit, modality = "1")[,"text_sd"]
  )

  testthat::expect_equal(
    expected_sd_modal2,
    extract_coeff(logit, modality = "2")[,"text_sd"]
  )

})

# TEST ALSO COEFFICIENTS FOR HTML TABLES ===============

expected_coeff_modal0 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    trimws(
      format(round(models_stat[['Estimate']][i,"0"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),  signif_stars(models_stat[['Pr(>|z|)']][i,"0"], type = "html")
  )
})
expected_coeff_modal1 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    trimws(
      format(round(models_stat[['Estimate']][i,"1"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),  signif_stars(models_stat[['Pr(>|z|)']][i,"1"], type = "html")
  )
})
expected_coeff_modal2 <- sapply(seq_along(rownames(models_stat[[1]])), function(i){
  paste0(
    trimws(
      format(round(models_stat[['Estimate']][i,"2"],3L),  nsmall = 3L,
             digits = 2L, scientific = FALSE)
    ),  signif_stars(models_stat[['Pr(>|z|)']][i,"2"], type = "html")
  )
})

testthat::test_that("Coefficients are okay", {

  testthat::expect_equal(
    expected_coeff_modal0,
    extract_coeff(logit, modality = "0", type = "html")[,"text_coeff"]
  )

  testthat::expect_equal(
    expected_coeff_modal1,
    extract_coeff(logit, modality = "1", type = "html")[,"text_coeff"]
  )

  testthat::expect_equal(
    expected_coeff_modal2,
    extract_coeff(logit, modality = "2", type = "html")[,"text_coeff"]
  )

})
