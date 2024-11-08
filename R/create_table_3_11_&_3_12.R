#' Create Tables 3.11 or 3.12
#'
#' @description
#' Table 3.11 Redistribution of live births with incomplete data on ‘age of mother’
#' @description
#' Table 3.12 Redistribution of deaths with incomplete data on ‘age of decedent’
#'
#' @param data data frame being used
#' @param date_var the occurrence year being used e.g. dobyr or dodyr
#' @param by_var what variable the data is being redistributed by
#' @param topic whether the table is births or deaths data
#' @param data_year The year to report on. Defaults to the last in the data.
#' @param tablename Name of the table to be saved as a csv file. Optional.
#' @param output_path The path to export the generated csv table. Optional.
#'
#' @return data frames for tabulated versions of Table 3.4 and 3.6
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.11 <- create_t3.11_and_3.12(bth_data, dobyr, data_year = 2022, by_var = fert_age_grp, tablename = "Table_3_11", topic = "births")
#' t3.12 <- create_t3.11_and_3.12(dth_data, dodyr, data_year = 2022, by_var = age_grp_80, tablename = "Table_3_12", topic = "deaths")
#'
create_t3.11_and_3.12 <- function(data, date_var, by_var, topic = NA,
                                  data_year = NA,
                                  tablename = "Table_3_11", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  # filter data based on topic
  if(topic == 'births'){
    data <- data %>% filter(!!sym(date_var) == data_year ,  is.na(birth1j))
  } else {
    data <- data %>% filter(!!sym(date_var) == data_year )
  }

  output <- data |>
    #filter({{date_var}} == data_year & if (topic == "births") is.na(birth1j) else TRUE) |>
    group_by({{by_var}}) |>
    summarise(total = n()) |>
    mutate(total = ifelse({{by_var}} == "unknown", 200, total))

  all_out = sum(output$total)

  output <- output |>
    mutate(total_count = all_out) |>
    mutate(proportion = construct_round_excel((total/total_count), 2)) |>
    select(-total_count)

  na_index <- (output[[quo_name(enquo(by_var))]]== "unknown")
  total_na <- sum(output$total[na_index])

  output <- output |>
    mutate(adjusted_total =  ifelse({{by_var}} != "unknown",
                                    floor(total + (proportion * total_na)),0))

  return(handle_table_output(output, output_path, tablename))
}


