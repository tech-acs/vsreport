#' Creates Table 5.11
#'
#'@description
#'Table 5.11 Deaths by year of occurrence and sex (Defaults to Foetal Deaths).
#'
#' Variables needed per record:
#'
#' - `dodyr`: Year of death.
#'
#' - `death1j`: Foetal death Identifier. Optional.
#'
#' - `death2c`: Sex of the deceased.
#'
#' @param data Death records data frame.
#' @param foetal_filter Flag to filter deaths. Defaults to "Yes". Can be NULL.
#' @param num_years Number of years to include in the table. Defaults to 5.
#' @param date_var Event year column of the dataframe. Defaults to "dodyr".
#' @param data_year The year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return data frame with tabulated result
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' death_data <- construct_sample_death_data()
#' death_data <- construct_year(death_data, date_col = "death1a", year_col = "dodyr")
#' table_5_11 <- create_t5.11(death_data, neonatal_filter = NULL, date_var= "dodyr")
#'
create_t5.11 <- function(data, foetal_filter = "Yes", num_yrs = 5,
                         date_var = "dodyr", data_year = NA,
                         tablename = "Table_5_11", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)
  years <- construct_year_sequence(latest_year = data_year, num_yrs = num_yrs)

  # Filter the data by foetal deaths if desired.
  if (!is.null(foetal_filter)){
    data <- data |>
      filter(death1j == "Yes")
  }

  output <- data |>
    filter(!!sym(date_var) %in% years) |>
    group_by(!!sym(date_var), death2c) |>
    summarise(total = n()) |>
    pivot_wider(names_from = death2c, values_from = total, values_fill = 0) |>
    adorn_totals("col")

  return(handle_table_output(output, output_path, tablename))
}
