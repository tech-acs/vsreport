#' Creates Tables 5.3 & 5.4
#'
#' @description
#' Table 5.3 Deaths by place of occurrence and place of usual residence of decedent (males)
#' @description
#' Table 5.4 Deaths by place of occurrence and place of usual residence of decedent (females)
#'
#' Variables needed per record:
#'
#' - `dodyr`: Year of death.
#'
#' - `death1c`: Region of death.
#'
#' - `death2o`: Region of usual residence.
#'
#' - `death2c`: Sex of the deceased.
#'
#'
#' @param data Death records data frame.
#' @param sex_filter Sex to filter by. Defaults to "male".
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
#' table_5_3 <- create_t5.3_and_t5.4(death_data, data_year = 2022)
create_t5.3_and_t5.4 <- function(data, sex_filter = "male",
                                 date_var = "dodyr", data_year = NA,
                                 tablename = "Table_5_3", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  output <- data |>
    filter(death2c == sex_filter & !!sym(date_var) == data_year) |>
    group_by(death1c, death2o)|>
    summarise(total = n()) |>
    pivot_wider(names_from = death2o, values_from = total, values_fill = 0)
  output <- output |>
    select(1, order(names(output)[-1]) + 1)|>
    arrange(death1c)|>
    adorn_totals(c("col", "row"))|>
    rename(region_of_death = death1c)

  return(handle_table_output(output, output_path, tablename))
}
