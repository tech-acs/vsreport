#' Create Table 5.12
#'
#' @description
#' Table 5.12 Foetal deaths by gestational age and birth weight of foetus
#'
#' Variables needed per record:
#'
#' - `dodyr`: Year of death.
#'
#' - `death1j`: Foetal death Identifier.
#'
#' - `gest_time`: Gestation time. Ideally this will be grouped.
#'
#' - `bthwgt_grp`: Birth weight. Ideally this will be grouped.
#'
#' @param data Death records data frame.
#' @param date_var Event year column of the dataframe. Defaults to "dodyr".
#' @param data_year The year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return data frame with tablutated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' table_5_12 <- create_t5.12(death_data)
#'
create_t5.12 <- function(data, date_var = "dodyr", data_year = NA,
                         tablename = "Table_5_12", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  output<- data |>
    filter(death1j == "Yes" & !!sym(date_var) == data_year)  |>
    group_by(gest_time, bthwgt_grp) |>
    summarise(total = n()) |>
    pivot_wider(names_from = bthwgt_grp, values_from = total, values_fill = 0)

  return(handle_table_output(output, output_path, tablename))
}



