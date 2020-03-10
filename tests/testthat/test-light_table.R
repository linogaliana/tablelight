testthat::context("latex table")


# OLS -----------



# OGLMX ---------

requireNamespace("oglmx", quietly = TRUE)

iris$y_r <- as.numeric(iris$Species)

oglm <- oglmx::oglmx(
  y_r ~ Sepal.Length ,
  data = iris
)

testthat::expect_warning(
  x <- texlight::light_table(oglm,
                             modeltype = "outcome"),
  "attributes are not identical across measure variables"
)


testthat::expect_equal(
  class(x),
  "character"
)

testthat::expect_warning(
  x2 <- texlight::light_table(list(oglm, oglm),
                        modeltype = c("outcome","outcome")),
  "attributes are not identical across measure variables"
)

testthat::expect_equal(
  class(x2),
  "character"
)
