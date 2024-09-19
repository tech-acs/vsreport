#' Read Sample Birth Data
#'
#' @description
#' Loads the dataframe with test birth data
#'
#' @return Data frame with test birth data
#' @export
#'
#' @import dplyr
#'
#' @note
#' The Test data in the package is fake and some links between variables will not
#' represent real relationships.
#'
#' @examples test_data_birth <- construct_sample_birth_data()
#'
construct_sample_birth_data <- function() {
  file_path <- system.file("extdata", "created_birth_data.csv", package = "crvsreportpackage")
  if (file_path == "") {
    stop("Sample data file created_birth_data.csv not found in the package extdata directory.")
  }
  data <- read.csv(file_path)
  return(data)
}

#' Read Sample Death Data
#'
#' Reads the sample data file created_death_data.csv from the package's extdata directory.
#'
#' @return A data frame containing the sample death data.
#' @export
construct_sample_death_data <- function() {
  file_path <- system.file("extdata", "created_death_data.csv", package = "crvsreportpackage")
  if (file_path == "") {
    stop("Sample data file created_death_data.csv not found in the package extdata directory.")
  }
  data <- read.csv(file_path)
  return(data)
}

#' Read Sample Estimated Death Data (i.e. "expected" deaths by region, age, sex)
#'
#' Reads the sample data file created_death_estim.csv from the package's extdata directory.
#'
#' @return A data frame containing the sample death data.
#' @export
construct_sample_estim_death_data <- function() {
  file_path <- system.file("extdata", "created_death_estim.csv", package = "crvsreportpackage")
  if (file_path == "") {
    stop("Sample data file created_death_estim.csv not found in the package extdata directory.")
  }
  data <- read.csv(file_path)
  return(data)
}

#' Read Sample Divorce Data
#'
#' Reads the sample data file created_divorce_data.csv from the package's extdata directory.
#'
#' @return A data frame containing the sample divorce data.
#' @export
construct_sample_divorce_data <- function() {
  file_path <- system.file("extdata", "created_divorce_data.csv", package = "crvsreportpackage")
  if (file_path == "") {
    stop("Sample data file created_divorce_data.csv not found in the package extdata directory.")
  }
  data <- read.csv(file_path)
  return(data)
}

#' Read Sample Marriage Data
#'
#' Reads the sample data file created_marriage_data.csv from the package's extdata directory.
#'
#' @return A data frame containing the sample marriage data.
#' @export
construct_sample_marriage_data <- function() {
  file_path <- system.file("extdata", "created_marriage_data.csv", package = "crvsreportpackage")
  if (file_path == "") {
    stop("Sample data file created_marriage_data.csv not found in the package extdata directory.")
  }
  data <- read.csv(file_path)
  return(data)
}

#' Read Sample Population Estimates Data
#'
#' Reads the sample data file created_population_estim.csv from the package's extdata directory.
#'
#' @return A data frame containing the sample marriage data.
#' @export
construct_sample_pop_estim <- function() {
  file_path <- system.file("extdata", "created_population_estim.csv", package = "crvsreportpackage")
  if (file_path == "") {
    stop("Sample data file created_population_estim.csv not found in the package extdata directory.")
  }
  data <- read.csv(file_path)
  return(data)
}

