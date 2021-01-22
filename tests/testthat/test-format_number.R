testthat::test_that("Digits reporting ok", {

  testthat::expect_equal(
    format_number_p(0.01, digits = max(3L, getOption("digits") - 3L)),
    "0.0100"
    )
  testthat::expect_equal(
    format_number_p(0.01, digits = 2),
    "0.01"
  )
  testthat::expect_equal(
    format_number_p(0.01, digits = 3),
    "0.010"
  )
  testthat::expect_equal(
    format_number_p(0.01, digits = 4),
    "0.0100"
  )

  testthat::expect_equal(
    format_number_p(0.001, digits = 1),
    "1e-03"
  )
  testthat::expect_equal(
    format_number_p(1e-7, digits = 1),
    "1e-07"
  )


  testthat::expect_equal(
    format_number_p(10000.01, digits = 3L),
    "10,000.010"
  )


})



testthat::test_that("Vectorized version works", {

  testthat::expect_equal(
    format_number(c(0.01, 1.01), digits = max(3L, getOption("digits") - 3L)),
    c("0.0100", "1.0100")
  )
  testthat::expect_equal(
    format_number(c(0.01, 1.01), digits = 2),
    c("0.01", "1.01")
  )
  testthat::expect_equal(
    format_number(c(0.01, 1.01), digits = 3),
    c("0.010", "1.010")
  )
  testthat::expect_equal(
    format_number(c(0.01, 1.01), digits = 4),
    c("0.0100", "1.0100")
  )

  testthat::expect_equal(
    format_number(c(0.001, 1.01), digits = 1),
    c("1e-03", "1.0")
  )
  testthat::expect_equal(
    format_number(1e-7, digits = 1),
    "1e-07"
  )


  testthat::expect_equal(
    format_number(c(10000.01, -10000.01), digits = 3L),
    c("10,000.010", "-10,000.010")
  )


})

