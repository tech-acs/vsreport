
test_that("Check handle_variable_names renames and validates correctly", {
  # Sample data and configuration for testing
  sample_df <- data.frame(
    birth_date = as.Date("2023-01-01"),
    birth_type = "single",
    mother_dob = as.Date("1990-05-20"),
    weight = 3.5,
    stringsAsFactors = FALSE
  )

  # Configuration mimicking the origin_variables
  test_origin_variables <- list(
    list(variable_id = "birth1a", variable_name = "birth_date", variable_type = "Date"),
    list(variable_id = "birth1g", variable_name = "birth_type", variable_type = "character"),
    list(variable_id = "birth3a", variable_name = "mother_dob", variable_type = "Date"),
    list(variable_id = "birth2b", variable_name = "weight", variable_type = "numeric"),
    list(variable_id = "birth3c", variable_name = "mother_status", variable_type = "character") # Missing
  )

  # Run the function
  result <- handle_variable_names(sample_df, test_origin_variables)
  modified_df <- result$dataframe
  missing_vars <- result$missing_vars

  # Test that variable renaming was successful
  expect_true("birth1a" %in% names(modified_df))
  expect_true("birth1g" %in% names(modified_df))
  expect_true("birth3a" %in% names(modified_df))
  expect_true("birth2b" %in% names(modified_df))

  # Test that original names are not present
  expect_false("birth_date" %in% names(modified_df))
  expect_false("birth_type" %in% names(modified_df))
  expect_false("mother_dob" %in% names(modified_df))
  expect_false("weight" %in% names(modified_df))

  # Test for missing variables
  expect_equal(missing_vars, "birth3c")

  # Test that data types match
  expect_type(modified_df$birth1a, "double") # Date stored as double in R
  expect_type(modified_df$birth1g, "character")
  expect_type(modified_df$birth3a, "double") # Date stored as double in R
})

