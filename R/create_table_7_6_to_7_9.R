#' Creates Tables 7.6 to 7.9
#'
#' @description
#' Table 7.6 Divorces by age of husband and age of wife
#' @description
#' Table 7.7 Divorces by duration of marriage and age of husband
#' @description
#' Table 7.8 Divorces by duration of marriage and age of wife
#' @description
#' Table 7.9 Divorces by duration of marriage and number of dependent children
#'
#' @param data data frame being used
#' @param group_var the tables columns
#' @param by_var tables rows
#' @param data_year The year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' t7.6 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "age_w", by_var = "age_h", tablename = "Table_7_6")
#' t7.7 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "age_h", by_var = "dur_grp", tablename = "Table_7_7")
#' t7.8 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "age_w", by_var = "dur_grp", tablename = "Table_7_8")
#' t7.9 <- create_t7.6_to_7.9(div_data, data_year = 2019, group_var = "child", by_var = "dur_grp", tablename = "Table_7_9")
#'
#'
create_t7.6_to_7.9 <- function(data, group_var = "age_w", by_var = "age_h",
                               data_year = NA,
                               tablename = "Table_7_6", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  dur_order <- c("<1", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                 "10-14", "15-19", "20-24", "25-29", "30+", "Total")
  output <- data |>
    filter(!!sym(date_var) == data_year) |>
    group_by(!!sym(group_var), !!sym(by_var)) |>
    summarise(total = n()) |>
    pivot_wider(names_from = !!sym(group_var), values_from = total, values_fill = 0) |>
    adorn_totals(c("row", "col"))

  if(by_var == "dur_grp"){
    output <- output %>%
      mutate(dur_grp = factor(dur_grp, levels = dur_order)) %>%
      arrange(dur_grp)
  }

  return(handle_table_output(output, output_path, tablename))
}


