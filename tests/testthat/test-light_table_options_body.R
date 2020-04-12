ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

# dep.var.options ===============

latex_table <- tablelight::light_table(ols,
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column")

latex_table2 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = rep("My depvar",10L),
                                        column.labels = "My label column")


html_table <- tablelight::light_table(ols,
                                      type = "html",
                                      title = "My table title",
                                      label = "My table label",
                                      dep.var.labels = "My depvar",
                                      column.labels = "My label column")

html_table2 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = rep("My depvar",10L),
                                       column.labels = "My label column")


testthat::test_that("Too many dep.var.labels do not change the output", {
  testthat::expect_equal(
    latex_table2, latex_table
  )
  testthat::expect_equal(
    html_table, html_table2
  )
})


# order_variable ============

ols <- lm(
  Sepal.Length ~ Sepal.Width + Petal.Length,
  data = iris
)

latex_table1 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column")
latex_table2 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column",
                                        order = c("(Intercept)","Petal.Length", "Sepal.Width"))

html_table1 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column")
html_table2 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column",
                                       order = c("Petal.Length", "(Intercept)", "Sepal.Width"))


testthat::test_that("[latex] Same table except order of variables", {
  row_coef <- grep("Sepal.Width", latex_table1)
  testthat::expect_equal(c(
    latex_table1[1:(row_coef-1)],
    latex_table1[(row_coef+3):(row_coef+5)],
    latex_table1[(row_coef):(row_coef+2)],
    latex_table1[(row_coef+6):length(latex_table1)]
  ),
  latex_table2
  )
})


testthat::test_that("[html] Same table except order of variables", {

  row_coef  <- html_table1[grep("Petal.Length", html_table1)]
  row_coef2 <- html_table2[grep("Petal.Length", html_table2)]

  row_coef3 <- as.character(
    stringr::str_split(row_coef2, "</td>", simplify = TRUE)
  )
  row_coef3 <- paste0(row_coef3[1:(length(row_coef3)-1)], "</td>")

  var_pos <- grep("(Intercept)", row_coef3)
  row_coef3 <- row_coef3[c(var_pos:(var_pos+11),
                           1:(var_pos-1),(var_pos+12):length(row_coef3))]

  row_coefb <- as.character(
    stringr::str_split(row_coef, "</td>", simplify = TRUE)
  )
  row_coefb <- paste0(row_coefb[1:(length(row_coefb)-1)], "</td>")

  testthat::expect_equal(
    row_coef3,
    row_coefb)
})


latex_table2b <- tablelight::light_table(ols,
                                         title = "My table title",
                                         label = "My table label",
                                         dep.var.labels = "My depvar",
                                         column.labels = "My label column",
                                         order = c("Petal.Length", "Sepal.Width"))

testthat::test_that("[latex] Without (Intercept), constant goes at the bottom", {
  row_coef <- grep("(Intercept)", latex_table2)
  row_coef2 <- grep("Sepal.Width", latex_table2)
  testthat::expect_equal(c(
    latex_table2[1:(row_coef-1)],
    latex_table2[(row_coef+3):(row_coef2+2)],
    latex_table2[(row_coef):(row_coef+2)],
    latex_table2[(row_coef2+3):length(latex_table1)]
  ),
  latex_table2b
  )
})


html_table2b <- tablelight::light_table(ols,
                                        type = "html",
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column",
                                        order = c("Petal.Length", "Sepal.Width"))

testthat::test_that("[html] Same table except order of variables", {

  row_coef  <- html_table1[grep("Petal.Length", html_table2)]
  row_coef2 <- html_table2[grep("Petal.Length", html_table2b)]

  row_coef3 <- as.character(
    stringr::str_split(row_coef2, "</td>", simplify = TRUE)
  )
  row_coef3 <- paste0(row_coef3[1:(length(row_coef3)-1)], "</td>")

  var_pos <- grep("(Intercept)", row_coef3)
  row_coef3 <- row_coef3[c(var_pos:(var_pos+11),
                           1:(var_pos-1),(var_pos+12):length(row_coef3))]

  row_coefb <- as.character(
    stringr::str_split(row_coef, "</td>", simplify = TRUE)
  )
  row_coefb <- paste0(row_coefb[1:(length(row_coefb)-1)], "</td>")

  testthat::expect_equal(
    row_coef3,
    row_coefb)
})


# omit ============


ols <- lm(
  Sepal.Length ~ Sepal.Width + Petal.Length,
  data = iris
)

latex_table1 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column")
latex_table2 <- tablelight::light_table(ols,
                                        title = "My table title",
                                        label = "My table label",
                                        dep.var.labels = "My depvar",
                                        column.labels = "My label column",
                                        omit = c("Petal.Length", "Sepal.Width"))

testthat::test_that("[latex] Same table except for the omit variables", {
  row_coef <- grep("(Intercept)", latex_table1)
  row_coef2 <- grep("Petal.Length", latex_table1)
  testthat::expect_equal(c(
    latex_table1[1:(row_coef+2)],
    latex_table1[(row_coef2+3):length(latex_table1)]
  ),
  latex_table2
  )
})


ols <- lm(
  Sepal.Length ~ Sepal.Width + Petal.Length,
  data = iris
)

html_table1 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column")
html_table2 <- tablelight::light_table(ols,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column",
                                       omit = c("Petal.Length", "Sepal.Width"))


testthat::test_that("[html] Same table except order of variables", {

  row_coef  <- html_table1[grep("(Intercept)", html_table1)]
  row_coef_smaller  <- html_table2[grep("(Intercept)", html_table2)]

  row_coef2 <- as.character(
    stringr::str_split(row_coef, "</td>", simplify = TRUE)
  )
  row_coef2 <- paste0(row_coef2[1:(length(row_coef2)-1)], "</td>")

  row_coef_smaller2 <- as.character(
    stringr::str_split(row_coef_smaller, "</td>", simplify = TRUE)
  )
  row_coef_smaller2 <- paste0(row_coef_smaller2[1:(length(row_coef_smaller2)-1)], "</td>")


  var_pos <- grep("Sepal.Width", row_coef2)
  row_coef3 <- row_coef2[c(1:(var_pos-1),
                           (var_pos+12):length(row_coef2))]

  testthat::expect_equal(
    row_coef3,
    row_coef_smaller2)
})


# footprint ------------

ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

latex_table <- tablelight::light_table(ols, footprint = TRUE,
                                      title = "My table title",
                                      label = "My table label",
                                      dep.var.labels = "My depvar",
                                      column.labels = "My label column")

html_table <- tablelight::light_table(ols, footprint = TRUE,
                                       type = "html",
                                       title = "My table title",
                                       label = "My table label",
                                       dep.var.labels = "My depvar",
                                       column.labels = "My label column")


testthat::test_that("Add footprint in header", {
  testthat::expect_true(
    startsWith(latex_table[1],"% Table generated using {tablelight}")
  ),
  testthat::expect_true(
    startsWith(html_table[1],"<!------Table generated using {tablelight}")
  )
})
