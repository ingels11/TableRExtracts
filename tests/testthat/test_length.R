context("Length with no missing")
library(TableRExtracts)

test_that("length_nona returns number of elements, excluding missing values", {
  expect_equal(length_nona(c(seq.int(from = 1, to = 20, by = 1), NA)), 20)
})
