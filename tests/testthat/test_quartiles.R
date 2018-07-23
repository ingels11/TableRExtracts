context("Quartiles")
library(TableRExtracts)

test_that("q1 is the first quartile and q3 the third quartile of a numeric vector", {
  expect_equal(q1(c(seq.int(from = 1, to = 20, by = 1), NA)), 5.75)
  expect_equal(q3(c(seq.int(from = 1, to = 20, by = 1), NA)), 15.25)
  expect_equal(q1(c(seq.int(from = 1, to = 20, by = 1), NA), na.rm = FALSE), NA)
  expect_equal(q3(c(seq.int(from = 1, to = 20, by = 1), NA), na.rm = FALSE), NA)
})
