library(dplyr)
library(tidyr)
library(lubridate)

# Read the variable information
variable_info <- read.csv("./vignettes/data/data_dictionary.csv")

# Helper function to generate realistic data
generate_data <- function(var_info) {
  n <- 100  # Number of simulated records

  data <- lapply(var_info$variable_name, function(var) {
    switch(var,
           "Date of occurrence" = sample(seq(as.Date('2000/01/01'), as.Date('2023/01/01'), by="day"), n, replace = TRUE),
           "Date of registration" = sample(seq(as.Date('2000/01/01'), as.Date('2023/01/01'), by="day"), n, replace = TRUE),
           "Place of occurrence" = sample(c("Hospital", "Home", "Clinic", "Other"), n, replace = TRUE),
           "Type of birth (i.e., single, twin, triplet, quadruplet or higher multiple delivery)" = sample(c("Single", "Twin", "Triplet", "Quadruplet or higher"), n, replace = TRUE),
           "Attendant at birth" = sample(c("Doctor", "Nurse", "Midwife", "Other"), n, replace = TRUE),
           "Type of place of occurrence (hospital, home, etc.)" = sample(c("Hospital", "Home", "Clinic", "Other"), n, replace = TRUE),
           "Sex" = sample(c("Male", "Female"), n, replace = TRUE),
           "Date of birth" = sample(seq(as.Date('1970/01/01'), as.Date('2005/01/01'), by="day"), n, replace = TRUE),
           "Age" = sample(18:50, n, replace = TRUE),
           "Marital status" = sample(c("Single", "Married", "Widowed", "Divorced"), n, replace = TRUE),
           "Marital status (previous)" = sample(c("Single", "Widowed", "Divorced"), n, replace = TRUE),
           "Place of usual residence" = sample(c("RegionA", "RegionB", "RegionC", "RegionD"), n, replace = TRUE),
           "Place of usual residence of the mother (for deaths under 1 year of age)" = sample(c("RegionA", "RegionB", "RegionC", "RegionD", NA), n, replace = TRUE),
           "Urban/rural residence" = sample(c("Urban", "Rural"), n, replace = TRUE),
           "Cause of death" = sample(c("Disease", "Accident", "Natural", "Other"), n, replace = TRUE),
           "Death occurring during pregnancy, childbirth and puerperium (for females between 15 and 49 years of age)" = sample(c("Yes", "No"), n, replace = TRUE),
           "Number of dependent children of divorced persons" = sample(0:5, n, replace = TRUE),
           "Date of marriage" = sample(seq(as.Date('1990/01/01'), as.Date('2020/01/01'), by="day"), n, replace = TRUE),
           "Duration of marriage" = sample(1:30, n, replace = TRUE),
           rep(NA, n))  # Default for unknown variables
  })

  # Convert list to data frame
  data <- as.data.frame(data)
  names(data) <- var_info$variable_name

  return(data)
}

# Split variable info by event
events <- split(variable_info, variable_info$event)

# Generate and save data for each event
for (event in names(events)) {
  event_data <- generate_data(events[[event]])
  file_name <- paste0("inst/extdata/created_", tolower(event), "_data.csv")
  write.csv(event_data, file = file_name, row.names = FALSE)
  cat("Data for", event, "written to", file_name, "\n")
}
