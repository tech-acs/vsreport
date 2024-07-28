# Define the unit tests
test_that("add_year_column works correctly with default date column", {
  df <- data.frame(birth1c = c("1990-05-15", "1985-10-30", "2000-07-22"))
  expected_df <- data.frame(
    birth1c = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
    dobyr = c("1990", "1985", "2000")
  )
  
  result_df <- add_year_column(df)
  
  expect_equal(result_df, expected_df)
})

test_that("add_year_column works correctly with specified date column", {
  df <- data.frame(event_date = c("1990-05-15", "1985-10-30", "2000-07-22"))
  expected_df <- data.frame(
    event_date = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
    dobyr = c("1990", "1985", "2000")
  )
  
  result_df <- add_year_column(df, date_col = "event_date")
  
  expect_equal(result_df, expected_df)
})

test_that("add_year_column handles different date formats", {
  df <- data.frame(birth1c = c("1990/05/15", "1985-10-30", "2000.07.22"))
  df$birth1c <- as.Date(df$birth1c, format = "%Y/%m/%d")
  expected_df <- data.frame(
    birth1c = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
    dobyr = c("1990", "1985", "2000")
  )
  
  result_df <- add_year_column(df)
  
  expect_equal(result_df, expected_df)
})
