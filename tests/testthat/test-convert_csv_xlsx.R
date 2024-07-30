# Load necessary packages
library(readr)
library(writexl)
library(fs)
library(purrr)

# Helper function to create temporary CSV files for testing
create_temp_csv <- function(data, filename) {
  temp_dir <- tempdir()
  file_path <- file.path(temp_dir, filename)
  write_csv(data, file_path)
  return(file_path)
}

test_that("convert_csv_xlsx creates an Excel file with the correct sheets", {
  # Create temporary CSV files
  temp_csv1 <- create_temp_csv(data.frame(A = 1:3, B = letters[1:3]), "test1.csv")
  temp_csv2 <- create_temp_csv(data.frame(X = 4:6, Y = letters[4:6]), "test2.csv")

  # Define temporary directory and output file path
  temp_dir <- tempdir()
  output_file <- file.path(temp_dir, "output.xlsx")

  # Run the convert_csv_xlsx function
  convert_csv_xlsx(input_path = temp_dir, output_path = output_file)

  # Check that the output file exists
  expect_true(file_exists(output_file))

  # Read the Excel file and check sheet names
  sheets <- readxl::excel_sheets(output_file)
  expect_true("test1" %in% sheets)
  expect_true("test2" %in% sheets)

  # Clean up temporary files
  file.remove(temp_csv1, temp_csv2, output_file)
})

test_that("convert_csv_xlsx warns if no CSV files are found", {
  # Create an empty temporary directory
  empty_dir <- tempdir()

  # Expect a warning when no CSV files are found
  expect_warning(convert_csv_xlsx(input_path = empty_dir, output_path = file.path(empty_dir, "output.xlsx")), "There are no .csv files in this directory")
})
