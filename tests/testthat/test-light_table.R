testthat::context("latex table")


# OLS -----------



# OGLMX ---------


iris$y_r <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y_r ~ Sepal.Length ,
  data = iris
)


testthat::expect_equal(
  class(texlight::light_table(oglm)),
  "character"
)


texlight::light_table(model = list(oglm, oglm))

