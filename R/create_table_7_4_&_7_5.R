#' Create Tables 7.4 and 7.5
#'
#' @description
#' Table 7.4 Marriages by age of groom and previous marital status
#' @description
#' Table 7.5 Marriages by age of bride and previous marital status
#'
#' @param data dataframe being used
#' @param groombride whether the table is for groom or bride data.
#' @param data_year The year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return data frame of tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' t7.4 <- create_table_7.4_and_7.5(marr_data, data_year = 2020, groombride = "groom", tablename = "Table_7_4")
#' t7.5 <- create_table_7.4_and_7.5(marr_data, data_year = 2020, groombride = "bride", tablename = "Table_7_5")
#'
create_table_7.4_and_7.5 <- function(data, data_year = NA, groombride = "groom",
                                     tablename = "Table_7_4", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  if(groombride == "groom"){
  output <- data |>
    filter(!!sym(date_var) == data_year) |>
    group_by(g_age_grp, marriage3b) |>
    summarise(total = n()) |>
    pivot_wider(names_from = marriage3b, values_from = total, values_fill = 0) |>
    adorn_totals(c("row", "col"))
  } else {
    output <- data |>
      filter(year == data_year) |>
      group_by(b_age_grp, marriage2b) |>
      summarise(total = n()) |>
      pivot_wider(names_from = marriage2b, values_from = total, values_fill = 0) |>
      adorn_totals(c("row", "col"))
  }

  return(handle_table_output(output, output_path, tablename))
}


