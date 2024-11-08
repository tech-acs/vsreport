#' Creates Tables 3.5 or 3.7
#'
#' @description Table 3.5 Birth registration completeness by place of usual residence of mother and sex of newborn
#' @description Table 3.7 Death registration completeness by place of usual residence and sex of decedent
#'
#' @param data dataframe being used
#' @param est_data data frame of estimated data being used
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
#' @param topic whether the data is for births or deaths
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
#' @examples t3.5 <- create_t3.5_and_3.7(bth_data, bth_est, dobyr, 2022, topic = "births", tablename = "Table_3_5")
#'
create_t3.5_and_3.7 <- function(data, est_data, date_var,
                                topic = NA, data_year = NA,
                                tablename = "Table_3_5", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  # filter data based on topic
  if(topic == 'births'){
    data <- data %>% filter(!!sym(date_var) == data_year , tolower(birth2a) %in% c("male", "female") , is.na(birth1j))
  } else {
    data <- data %>% filter(!!sym(date_var) == data_year , tolower(birth2a) %in% c("male", "female") )
  }

  counts <- data |>
    # filter({{date_var}} == data_year & tolower(birth2a) %in% c("male", "female") &
    #          if (topic == "births") is.na(birth1j) else TRUE) |>
    group_by(birth1c, birth2a) |>
    summarise(total = n())

  ests <- est_data |>
    filter(year == data_year) |>
    pivot_longer(cols = c("male", "female"), names_to = "birth2a", values_to = "count" ) |>
    group_by(birth1c, birth2a) |>
    summarise(total_est = sum(count))

  #output <- left_join(counts, ests, by.x = c("birth1c", "birth2a"), by.y = c("birth1c", "birth2a"), all.x = TRUE)
  output <- left_join(counts, ests, by = c("birth1c", "birth2a"))

  output2 <- output |>
    group_by(birth1c) |>
    summarise(total = sum(total), total_est = sum(total_est)) |>
    mutate(birth2a = "total", .after = "birth1c")

  output <- rbind(output, output2) |>
    mutate(completeness := round((total / total_est) * 100, 2)) |>
    pivot_wider(names_from = birth2a, values_from = c(total, total_est, completeness), names_sep = " ") |>
    mutate(birth1c = ifelse(birth1c %in% c("", " ", NA), "not stated", birth1c)) |>
    arrange(birth1c) %>%
    adorn_totals("row")

  return(handle_table_output(output, output_path, tablename))
}
