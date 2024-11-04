
test_that("create_t3.1 function works correctly", {
    working_dir <- getwd()
    setwd("../../")

    # Sample data
    bth_data <- data.frame(
      dobyr = c(2019, 2020, 2021, 2022, 2023),
      timeliness = c("Current", "Late", "Delayed", "Current", "Late"),
      birth1j = c(NA, NA, NA, 1, NA)
    )

    dth_data <- data.frame(
      dodyr = c(2019, 2020, 2021, 2022, 2023),
      timeliness = c("Current", "Late", "Delayed", "Current", "Late")
    )

    # Call the function
    data_year = NA
    result <- create_t3.1(bth_data = bth_data, dth_data = dth_data,
                          bth_yr_var = "dobyr", dth_yr_var = "dodyr",
                          tablename = "Table_3_1", output_path = "outputs/")
    # Check if result is a data frame
    expect_s3_class(result, "data.frame")

    # Check if the file is written
    expect_true(file.exists("outputs/Table_3_1.csv"))

    # Check for expected columns in the result
    expected_cols <- c("timeliness",
                       "2019 1 Live births","2019 2 Deaths",
                       "2020 1 Live births","2020 2 Deaths",
                       "2021 1 Live births","2021 2 Deaths",
                       "2022 2 Deaths",
                       "2023 1 Live births","2023 2 Deaths")
    expect_equal(colnames(result), expected_cols)

    # Clean up the generated file after the test
    unlink("outputs/Table_3_1.csv")
    # Reset working dir
    setwd(working_dir)
})
