#' Creates Tables 5.2
#'
#' This function generates Table 5.2, which shows deaths by place of usual residence
#' and the sex of the deceased.
#'
#' Variables needed per record:
#'
#' - `dodyr`: Year of death.
#'
#' - `death2o`: Region of death.
#'
#' - `death2c`: Sex of the deceased.
#'
#' @param data Death records data frame.
#' @param date_var Event year column of the dataframe. Defaults to "dodyr".
#' @param data_year The year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return A data frame of tabulated results
#' @export
#'
#' @examples
#' death_data <- construct_sample_death_data()
#' death_data <- construct_year(death_data, date_col = "death1a", year_col = "dodyr")
#' table_5_2 <- create_t5.2(death_data, date_var="dodyr", data_year = 2022)
#' View(table_5_2)
create_t5.2 <- function(data, date_var = "dodyr", data_year = NA,
                        tablename = "Table_5_2", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  output <- data |>
    filter(!!sym(date_var) == data_year) |>
    group_by(death2o, death2c) |>
    summarise(total = n()) |>
    pivot_wider(names_from = death2c, values_from = total, values_fill = 0) |>
    arrange(death2o)|>
    adorn_totals(c("col", "row"))
    rename(region_of_residence = death2o)

  return(handle_table_output(output, output_path, tablename))
}
