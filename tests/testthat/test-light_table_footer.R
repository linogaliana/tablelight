
ols <- lm(
  Sepal.Length ~ Sepal.Width,
  data = iris
)

glm <- glm(
  Sepal.Length ~ Sepal.Width,
  data = iris,
  family = poisson()
)


# add.lines ----------

latex_table <- tablelight::light_table(
  list(ols, glm), type = "latex", add.lines = "blablabla I am a very long note that is very very long"
)
html_table <- tablelight::light_table(
  list(ols, glm), type = "html", add.lines = "blablabla I am a very long note that is very very long"
  )

bloc_note_html <- html_table[(length(html_table)-2):length(html_table)]

testthat::test_that("We report the note correctly in HTML", {
  testthat::expect_equal(
    sprintf("<tr><td colspan=\"%s\"></td></tr>", length(list(ols, glm))+1),
    bloc_note_html[1]
  )
  testthat::expect_equal(
    '<tr><td style=\"text-align:left\">blablabla I am a very long note that is very very long</td><td></td><td></td></tr>',
    bloc_note_html[2]
  )
  testthat::expect_equal(
    '</table>',
    bloc_note_html[3]
  )
})


bloc_note_latex <- latex_table[grep("Note:", latex_table)+1]

testthat::test_that("We report the note correctly in HTML", {
  testthat::expect_equal(
    trimws(bloc_note_latex),
    "\\multicolumn{3}{p{0.9\\linewidth}}{blablabla I am a very long note that is very very long} \\\\"
  )
})
