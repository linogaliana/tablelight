testthat::context("Escape special characters for regex")


testthat::expect_equal(
  str_to_regex(
    c("^", "$", ".",
      "|", "?", "*",
      "+", "(", ")",
      "[", "]" ,"{", "}")
  ),
  c("\\^", "\\$", "\\.",
    "\\|", "\\?", "\\*",
    "\\+", "\\(", "\\)",
    "\\[", "\\]" ,"\\{", "\\}")
)


testthat::expect_equal(
  str_to_regex(
    c("^A", "$red car", "never.used",
      "with| some", "nice wheels?")
  ),
  c("\\^A", "\\$red car", "never\\.used",
    "with\\| some", "nice wheels\\?")
)
