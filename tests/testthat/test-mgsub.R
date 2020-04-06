testthat::context("mgsub: vectorized multigsub")

requireNamespace("mgsub", quietly = TRUE)

testthat::expect_equal(
  mgsub(c("it's", "I'm"), c("it is", "I am"), c("I'm the king of the worlds, it's so funny","it's impossible!")),
  c("I am the king of the worlds, it is so funny", "it is impossible!")
)

testthat::expect_equal(
  mgsub(c("it's", "I'm"), c("it is", "I am"), c("I'm the king of the worlds, it's so funny","it's impossible!")),
  qdap::mgsub(c("it's", "I'm"), c("it is", "I am"), c("I'm the king of the worlds, it's so funny","it's impossible!"))
)


testthat::expect_equal(
  mgsub("[[:punct:]]", "PUNC", c('a.','!'), fixed = FALSE),
  c('aPUNC','PUNC')
)

testthat::expect_equal(
  mgsub("[[:punct:]]", "PUNC", c('a.','!'), fixed = FALSE),
  qdap::mgsub("[[:punct:]]", "PUNC", c('a.','!'), fixed = FALSE)
)
