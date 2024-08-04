test_that("create_t4.1 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  #bth_data <- data.frame(
  #  dobyr = c(2018, 2018, 2019, 2019, 2020, 2018, 2018, 2019, 2019, 2020),
  #  fert_age_grp = c("15-19", "15-19", "15-19", "15-19", "15-19", "20-24", "20-24", "20-24", "20-24", "20-24"),
  #  birth1j = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  #  birth2a = c("male", "female", "male", "female", "male", "male","male","female","female","male")
  #)
  bth_data <- read.csv("inst/extdata/created_birth_data.csv", header =T)
  # Add timeliness data
  bth_data <- construct_timeliness(bth_data)
  # Add dobyr
  bth_data <- construct_year(bth_data, date_col = "birth1a", year_col = "dobyr")
  # Add doryr
  bth_data <- construct_year(bth_data, date_col = "birth1b",  year_col = "doryr")
  # Add empty birth1j (stillbirth)
  bth_data <- construct_empty_var(bth_data)
  # Add fertility age groups
  bth_data <- construct_age_group(bth_data, "birth3b")

  #est_data <- data.frame(
  #  year = c(2018, 2019, 2020),
  #  female = c(100, 200, 300),
  #  male = c(150, 250, 350)
  #)
  est_data <- read.csv("inst/extdata/created_birth_estim.csv", header = T)
  # Add the total column for birth estimates
  est_data <- est_data %>%
    mutate(total = male + female)
  # Add fertility age groups for the birth estimates
  est_data <- construct_age_group(est_data, "age")

  #pops <- data.frame(
  #  population_2018 = c(5000, 6000, 4500, 5500),
  #  population_2019 = c(5200, 6200, 4500, 5500),
  #  population_2020 = c(5400, 6400, 4500, 5500),
  #  birth2a = c("male", "female", "male", "female"),
  #  fert_age_grp = c("15-19", "15-19","20-24","20-24")
  #)
  pops <- read.csv("inst/extdata/created_population_estim.csv")
  # Add fertility age groups for the population data
  pops <- construct_age_group(pops, "age")

  # Call the function
  result <- create_t4.1(bth_data, est_data, pops, date_var = "dobyr", tablename = "Table_4_1", output_path = "outputs/")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the file is written
  expect_true(file.exists("./outputs/Table_4_1.csv"))

  # Check for expected columns in the result
  expected_cols <- c("Indicator", "2018", "2019", "2020", "2021", "2022")
  expect_equal(colnames(result), expected_cols)

  # Clean up the generated file after the test
  unlink("./outputs/Table_4_1.csv")

  setwd(working_dir)
})
