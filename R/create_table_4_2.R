#' Creates Table 4.2
#'
#' Table 4.2 Live births by place of occurrence (birth1c) and sex (birth2a) of newborn
#'
#' @param data Birth records data frame
#' @param est_data Estimated births data frame
#' @param date_var Name of date variable (years)
#' @param data_year Year of which to select the data
#' @param tablename Name of the table to be saved as a csv file
#' @param output_path The path to export the generated csv table
#'
#' @return A table of counts of births for each place of occurance split by sex.
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.2 <- create_t4.2(birth_data, est_data, date_var = "dobyr",
#' data_year = 2023, tablename = "Table_4_2", output_path = "outputs/births/")
create_t4.2 <- function(data, est_data, date_var = "dobyr", data_year = NA,
                        tablename = "Table_4_2", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  output <- data |>
    filter(!!sym(date_var) == data_year & is.na(birth1j)) |>
    group_by(birth1c, birth2a) |>
    summarise(total = n()) |>
    pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
    adorn_totals(c("row","col")) |>
    mutate(ratio = construct_round_excel(male/female,1))

  est <- est_data |>
    filter(year == data_year) |>
    group_by(birth1c) |>
    summarise(total = sum(total))

  output <- left_join(output, est, by = c("birth1c")) |>
    mutate(completeness = construct_round_excel(Total/total*100, 1)) |>
    select("birth1c", "male", "female", "not stated", "Total", "ratio", "completeness")

  return(handle_table_output(output, output_path, tablename))
}

