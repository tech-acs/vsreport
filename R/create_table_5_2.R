#' Creates Tables 5.2
#'
#' Table 5.2 Deaths by place of usual residence and sex of deceased
#' Variables needed:
#' - Year of Death: passed through date_var (default is "dodyr")
#' - Region of Death: Has to be "death2o"
#' - Sex of Deceased: Has to be "death2c"
#'
#' @param data death registration data frame.
#' @param date_var event year column of the dataframe like dodyr.
#' @param data_year the year to report on.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return data frame of tabulated results
#' @export
#'
#' @examples
#' death_data <- construct_sample_death_data()
#' death_data <- construct_year(death_data, date_col = "death1a", year_col = "dodyr")
#' table_5_2 <- create_t5.2(death_data, date_var="dodyr", data_year = 2022)
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
    adorn_totals("row")|>
    adorn_totals("col") |>
    rename(region_of_death = death2o)

  return(handle_table_output(output, output_path, tablename))
}
