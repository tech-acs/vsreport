#' Create Tables 7.2 and 7.3
#'
#' @description
#' Table 7.2 Marriages by place of usual residence of groom and age of bride and groom, urban areas
#' @description
#' Table 7.3 Marriages by place of usual residence of groom and age of bride and groom, rural areas
#'
#' @param data data frame being used
#' @param data_year year the data is for
#' @param rural_urban whether the data is for urban or rural
#' @param tablename name of the table being saved as a csv file
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
#' t7.2 <- create_t7.2_and_7.3(marr_data, data_year = 2020, rural_urban = "urban", tablename = "Table_7_1")
#' t7.3 <- create_t7.2_and_7.3(marr_data, data_year = 2020, rural_urban = "rural", tablename = "Table_7_2")
#'
create_t7.2_and_7.3 <- function(data, data_year = NA, rural_urban = "urban",
                                tablename = "Table_7_1", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  output <- data |>
    filter(year == data_year & marriage3n == rural_urban) |>
    group_by(g_age_grp, b_age_grp) |>
    summarise(total = n()) |>
    pivot_wider(names_from = b_age_grp, values_from = total, values_fill = 0) |>
    adorn_totals(c("col", "row"))

  return(handle_table_output(output, output_path, tablename))
}
