library(tidyr)
library(dplyr)

test_that("create_t7.1 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Load the marriage data
  marriage_data <- read_sample_marriage_data()
  # Load the death data
  divorce_data <- read_sample_divorce_data()

  # Run the function without output path
  result <- create_t7.1(marriage_data, divorce_data, population_data)

  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")

  # Run the function with output path
  result_with_path <- create_t7.1(marriage_data, divorce_data, population_data, tablename = "Table_7_1", output_path = "outputs/")

  # Check if the file is written
  expect_true(file.exists("outputs/Table_7_1.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_6_2.csv")

  # Reset working dir
  setwd(working_dir)
})

test_that("create_t7.1 function writes the table correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Load the marriage data
  marriage_data <- read_sample_marriage_data()
  # Load the death data
  divorce_data <- read_sample_divorce_data()

  # Run the function with output path
  result_with_path <- create_t7.1(marriage_data, divorce_data, population_data, tablename = "Table_7_1", output_path = "outputs/")

  # Check if the file is written
  expect_true(file.exists("outputs/Table_7_1.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_7_1.csv")

  # Reset working dir
  setwd(working_dir)
})
