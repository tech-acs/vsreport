#' Creates Table 5.5
#'
#' @description
#' Table 5.5 Deaths by region of death and site of occurrence
#'
#' Variables needed per record:
#'
#' - `dodyr`: Year of death.
#'
#' - `death1c`: Region of death.
#'
#' - `death1n`: Site of occurrence.
#'
#' @param data data frame being used
#' @param date_var occurrence data variable being used
#' @param data_year year the data is for
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' death_data <- construct_sample_death_data()
#' death_data <- construct_year(death_data, date_col = "death1a", year_col = "dodyr")
#' table_5_5 <- create_t5_5(death_data, date_var= "dodyr", data_year = 2022)
create_t5_5 <- function(data, date_var = "dodyr", data_year = NA,
                        tablename = "Table_5_5", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  output <- data |>
    filter(!!sym(date_var) == data_year) |>
    group_by(death1c, death1n) |>
    summarise(total = n()) |>
    pivot_wider(names_from = death1n, values_from = total, values_fill = 0) |>
    arrange(death1c)|>
    adorn_totals(c("col", "row"))|>
    rename(region_of_death = death1c)

  return(handle_table_output(output, output_path, tablename))
}
