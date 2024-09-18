test_that("create_t5.1 function works correctly", {

  # Load & construct expected output from external file
  suppressMessages({expected_output <- read_csv("../../inst/extdata/expected_5_1_output.csv")})
  d_cols <- 2:ncol(expected_output)
  # Note rounding required to ensure match to rounding by create_table_5_1:
  expected_output[3,d_cols] <- construct_round_excel(expected_output[3,d_cols],1)
  expected_output[4,d_cols] <- construct_round_excel(expected_output[4,d_cols],1)
  expected_output[5,d_cols] <- construct_round_excel(expected_output[5,d_cols],2)

  # Input
  death_data <- construct_sample_death_data()
  estim_death_data <- construct_sample_estim_death_data()
  sample_pop_estim <- construct_sample_pop_estim()
  death_data <- construct_year(death_data, date_col = "death1a",
                               year_col = "dodyr")

  result <- create_t5.1(data = death_data, est_data = estim_death_data,
                        pops = sample_pop_estim, tablename = "Table_5_1",
                        output_path = "outputs/")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the file is written
  expect_true(file.exists("./outputs/Table_5_1.csv"))

  # Check for expected columns in the result
  expect_equal(colnames(result), colnames(expected_output))

  # Check values against expected_output
  n_expected_vals <- ncol(result[,d_cols]) * nrow(result[,d_cols])
  # All vals equal?
  as_expected <- sum(result[,d_cols] == expected_output[,d_cols])
  expect_true(n_expected_vals == as_expected)

  # Clean up the generated file after the test
  unlink("./outputs/Table_5_1.csv")
})
