#' Creates Tables 3.2 and 3.3
#'
#' @description Table 3.2 Proportion in percentage of live births by year of registration and year of occurrence
#' @description Table 3.3 Proportion in percentage of deaths by year of registration and year of occurrence
#'
#' @param data name of data frame being used
#' @param date_var year variable for births or deaths occurrences
#' @param topic whether births or deahts is being run
#' @param data_year The last year to report on. Defaults to the last in the data.
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
#' @examples t3.2 <-
create_t3.2_t3.3 <- function(data, date_var = "dobyr", topic = "births",
                             data_year = NA,
                             tablename = "Table_3_2", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)
  years <- construct_year_sequence(data_year)

  if(topic == "births"){

    output_vol <- data |>
      filter(is.na(birth1j) & !!sym(date_var) %in% years) |>
      group_by(doryr, !!sym(date_var)) |>
      summarise(Total = n())

    output2 <- output %>%
      group_by(doryr) %>%
      summarise(total = sum(Total))

    # Merge total live births back into the original dataframe
    output <- output %>%
      left_join(output2, by = c("doryr" = "doryr")) %>%
      mutate(Percentage := construct_round_excel((Total/ total) * 100, 2)) %>%
      select(-c(total, Total)) |>
      pivot_wider(names_from = doryr, values_from = Percentage, values_fill = 0) %>%
      adorn_totals("row", name = "Grand total")
  }else if(topic == "deaths"){
    output <- data |>
      filter(!is.na(doryr) & {{occ_var}} %in% construct_year_sequence(max_value)) |>
      group_by(doryr, {{occ_var}}) |>
      summarise(Total = n())

    output2 <- output %>%
      group_by(doryr) %>%
      summarise(total = sum(Total))

    # Merge total deaths back into the original dataframe
    output <- output %>%
      left_join(output2, by = c("doryr" = "doryr")) %>%
      mutate(Percentage := construct_round_excel((Total/ total) * 100, 2)) %>%
      select(-c(total, Total)) |>
      pivot_wider(names_from = doryr, values_from = Percentage, values_fill = 0) %>%
      adorn_totals("row", name = "Grand total")
  }

  return(handle_table_output(output, output_path, tablename))
}
