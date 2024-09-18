# unit test for the 'construct_round_excel' function

library(testthat)


source("../../R/construct_round_excel.R")

test_that("construct_round_excel function works correctly", {
  expect_equal(construct_round_excel(1.14, 1), 1.1)  # Test round down
  expect_equal(construct_round_excel(1.15, 1), 1.2) # Test round up
  expect_equal(construct_round_excel(1.15, 0), 1.0)  # Test with zero
  expect_equal(construct_round_excel(-1.4,0), -1)  # Test round down with negative number
  expect_equal(construct_round_excel(-1.5, 0), -2) # Test round up with negative number
  expect_error(construct_round_excel("a", 1))  # Test with a non-numeric input
})
