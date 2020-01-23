testthat::context("se_coeff methods return coefficients s.e., t stat and pvalues")



ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)


testthat::expect_equal(
  secoeff(ols),
  summary(ols)$coefficients
)




iris$y_r <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y_r ~ Sepal.Length ,
  data = iris
)


testthat::expect_equal(
  texlight::secoeff(oglm),
  summary(oglm)$estimate
)


