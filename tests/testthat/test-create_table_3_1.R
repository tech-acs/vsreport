
test_that("create_t3.1 function works correctly", {
    # Sample data
    bth_data <- data.frame(
      dobyr = c(2019, 2020, 2021, 2022, 2023),
      timeliness = c("Current", "Late", "Delayed", "Current", "Late"),
      sbind = c(NA, NA, NA, 1, NA)
    )

    dth_data <- data.frame(
      dodyr = c(2019, 2020, 2021, 2022, 2023),
      timeliness = c("Current", "Late", "Delayed", "Current", "Late")
    )

    # Call the function
    result <- create_t3.1(bth_data, dth_data, dobyr, dodyr)

    # Check if result is a data frame
    expect_s3_class(result, "data.frame")


    # Check if the file is written
    expect_true(file.exists("../../outputs/Table_3_1.csv"))

    # Clean up the generated file after the test
    unlink("../../outputs/Table_3_1.csv")
})
