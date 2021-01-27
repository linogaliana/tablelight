testthat::context("stack_summary makes nice summary stats tables")

df <- data.frame(
  x = rnorm(10),
  y = 1:10
)

tablelight:::stack_summary(df, x_vars = c("x"))
tablelight:::stack_summary(list(df, df), x_vars = c("x","y"))


testthat::expect_equal(
  tablelight:::stack_summary(df, x_vars = c("x"),
                           stats = c("1Q","mean",'median','3Q','P90','N')),
  tablelight:::stack_summary(df, x_vars = c("x"),
                           stats = c("mean","1Q",'median','3Q','P90','N'))
)
