#' Creates Tables 3.8 and 3.9
#'
#' @description
#' Table 3.8 Adjustment of live births by place of usual residence of mother and sex of newborn
#' @description
#' Table 3.9 Adjustment of deaths by place of usual residence and sex of decedent
#'
#'
#' @param data name of data frame being used
#' @param date_var occurrence data variable being used
#' @param est_data data frame of estimated data being used
#' @param by_var the occurrence year being used e.g. dobyr or dodyr
#' @param topic whether births or deahts is being run
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
#' @examples t3.8 <- create_t3.8_and_t3.9(data = bth_data, est_data = bth_est,
#' date_var = dobyr, data_year = 2022, by_var = birth1c,
#' topic = "births", tablename = "Table_3_8")

#' t3.9 <- create_t3.8_and_t3.9(data = dth_data, est_data = dth_est,
#'                             date_var = dodyr, data_year = 2022, by_var = birth1c,
#'                             topic = "deaths", tablename = "Table_3_9")
#'
create_t3.8_and_t3.9 <- function(data, est_data, date_var, by_var,
                                 topic = NA, tablename = NA,
                                 data_year = NA,
                                 tablename = "Table_3_9", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  # filter data based on topic
  if(topic == 'births'){
    data <- data %>% filter(!!sym(date_var) == data_year, is.na(birth1j))
  } else {
    data <- data %>% filter(!!sym(date_var) == data_year)
  }

  counts <- data |>
   # filter({{date_var}} == data_year & if (topic == "births") is.na(birth1j) else TRUE) |>
   # group_by(!!by_var, birth2a) |>
    group_by(birth1c, birth2a) |>
    summarise(total = n()) |>
    pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
    select(-`not stated`) |>
    adorn_totals("col")

  ests <- est_data |>
    filter(year == data_year) |>
    group_by(birth1c) |>
    summarise(female_est = sum(female), male_est = sum(male)) |>
    adorn_totals("col") |>
    rename(total_est = Total)

  output <- left_join(counts, ests, by = c("birth1c")) |>
    mutate(f_comp = construct_round_excel(female/female_est, 2),
           m_comp = construct_round_excel(male/male_est, 2),
           t_comp = construct_round_excel(Total/total_est, 2)) |>
    mutate(f_adj = ceiling(female/f_comp),
           m_adj = ceiling(male/m_comp),
           t_adj = ceiling(Total/t_comp)) |>
    select(birth1c, male, m_adj, female, f_adj, Total, t_adj)|>
    adorn_totals("row")

  return(handle_table_output(output, output_path, tablename))
}

