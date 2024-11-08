#' Create Table 3.1
#'
#' Table 3.1 Number of vital events by registration timeliness, year
#'
#' @param bth_data name of births data frame
#' @param dth_data name of deaths data frame
#' @param bth_yr_var name of year variable (Births)
#' @param dth_yr_var name of year variable (Deaths)
#' @param data_year The last year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return Data frame with tabulated results (if an output_path is given, it will export a .csv)
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.1 <- create_t3.1(bth_data = bth_data, dth_data = dth_data, bth_yr_var = dobyr, dth_yr_var = dodyr, tablename = "Table_3_1")
#'
create_t3.1 <- function(bth_data, dth_data, bth_yr_var = "dobyr",
                        dth_yr_var = "dodyr", data_year = NA,
                        tablename = "Table_3_1", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, bth_data, bth_yr_var)
  years <- construct_year_sequence(data_year)

  outputb <- bth_data |>
    filter(is.na(birth1j) & !!sym(bth_yr_var) %in% years) |>
    group_by(!!sym(bth_yr_var), timeliness) |>
    summarise(total = n()) |>
    mutate(type = "1 Live births") |>
    rename(year = {{bth_yr_var}})

  outputd <- dth_data |>
    filter(!!sym(dth_yr_var) %in% years) |>
    group_by(!!sym(dth_yr_var), timeliness) |>
    summarise(total = n()) |>
    mutate(type = "2 Deaths") |>
    rename(year = {{dth_yr_var}})

  output <- rbind(outputb, outputd) |>
    pivot_wider(names_from = c(year, type), values_from = total, values_fill = 0, names_sep = " ", names_sort = TRUE) |>
    arrange(match(timeliness, c("Current", "Late", "Delayed"))) |>
    adorn_totals("row", name = "Grand total")

  return(handle_table_output(output, output_path, tablename))
}

