test_that("create_t4.4_to_4_6 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  bth_data <- read.csv("inst/extdata/created_birth_data.csv", header = T)
  # Add timeliness data
  bth_data <- construct_timeliness(bth_data)
  # Add dobyr
  bth_data <- construct_year(bth_data, date_col = "birth1a", year_col = "dobyr")
  # Add boryr
  bth_data <- construct_year(bth_data, date_col = "birth1b",  year_col = "doryr")
  # Add empty birth1j
  bth_data <- construct_empty_var(bth_data)
  # Add fertility age groups
  bth_data <- construct_age_group(bth_data, "birth3b")

  result <- create_t4.4_to_4_6(bth_data, data_year = 2022, col_var = "fert_age_grp", by_var = "birth1g", rural_urban = "no", tablename = "Table_4_4", output_path = "outputs/")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check for expected columns in the result
  # Establish number of columns expected (as dependent on levels in birth1g)
  # Number of columns expected is number of levels +2 (fert_age_grp, Total):
  ncols_expected <- nlevels(factor(bth_data$birth1g)) + 2
  expect_equal(ncol(result), ncols_expected)

  # Check if the file is written
  ### NOTE ONLY ONE OF THREE IS CHECKED CURRENTLY
  expect_true(file.exists("outputs/Table_4_4.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_4_4.csv")
  #unlink(c("outputs/Table_4_4.csv",
  #         "outputs/Table_4_5.csv",
  #         "outputs/Table_4_6.csv"))

  # Reset working dir
  setwd(working_dir)
})
