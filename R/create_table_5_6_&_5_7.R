#' Creates Tables 5.6 & 5.7
#'
#' @description
#' Table 5.6 Deaths by age and sex of decedent (urban areas)
#' @description
#' Table 5.7 Deaths by age and sex of decedent (rural areas)
#'
#' Variables needed per record:
#'
#' - `dodyr`: Year of death.
#'
#' - `death2r`: Rural-Urban Identifier. Optional.
#'
#' - `death2c`: Sex of the deceased.
#'
#' - `death2b`: Age of the deceased. Ideally in a grouped format.
#'
#' @param data Death records data frame.
#' @param ru_filter Type of region to filer by (urban/rural). Optional.
#' @param age_var Age of deceased column of the dataframe.
#' @param date_var Event year column of the dataframe. Defaults to "dodyr".
#' @param data_year The year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return A data frame of tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' death_data <- construct_sample_death_data()
#' death_data <- construct_year(death_data, date_col = "death1a", year_col = "dodyr")
#' death_data <- construct_age(death_data, dob_col = "death2a", age_col = "death2b", end_date_col = "death1a")
#' death_data <- construct_age_group(death_data, age_col = "death2b", new_col = "death2b_grp")
#' table_5_6 <- create_t5.6_and_t5.7(death_data, ru_filter = "urban", age_var = "death2b_grp", data_year = 2022)
create_t5.6_and_t5.7 <- function(data, ru_filter = NULL, age_var,
                                 date_var = "dodyr", data_year = NA,
                                 tablename = "Table_5_6", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  # Filter the data by urban/rural if desired.
  if (!is.null(ru_filter)){
    data <- data |>
      filter(death2r == ru_filter)
  }

  output <- data |>
    filter(!!sym(date_var) == data_year) |>
    group_by(!!sym(age_var), death2c) |>
    summarise(total = n()) |>
    pivot_wider(names_from = death2c, values_from = total, values_fill = 0) |>
    adorn_totals(c("col", "row")) |>
    rename(`age_of_decedent` = !!sym(age_var))

  return(handle_table_output(output, output_path, tablename))
}
