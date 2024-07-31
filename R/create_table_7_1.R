#' Generate Table 7.1 Summary statistics on marriages and divorces by year of occurrence
#'
#' This function takes marriage and divorce data and generates a table with summary statistics on marriages and divorces by year.
#'
#' @param marriage_data The marriage data.
#' @param divorce_data The divorce data.
#' @param population_data The population data.
#' @param date_var The variable to use the year from
#' @param data_year A particular year to choose from
#' @param tablename name of the table being saved as a csv file
#' @param output_path The path to export the generated table
#'
#' @return A data frame containing the summary statistics on marriages and divorces by year.
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' result_table <- create_t7.1(marriage_data, divorce_data, population_data, tablename = "Table_7_1", output_path = "outputs/")
create_t7.1 <- function(marriage_data, divorce_data, population_data, date_var = "domyr", data_year = NA, tablename = "Table_7_1", output_path = NULL){
  #date_var <- enquo(date_var)

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year <- max(c(marriage_data %>% pull(!!date_var), divorce_data %>% pull(!!date_var)), na.rm = TRUE)
  }
  years <- generate_year_sequence(data_year)

  # Process marriage data
  marriage_summary <- marriage_data %>%
    filter(!!date_var %in% years & marriage2c == "Single" & marriage3c == "Single") %>%
    group_by(!!date_var) %>%
    summarise(
      num_marriages = n(),
      avg_age_first_marriage_male = mean(marriage3b, na.rm = TRUE),
      avg_age_first_marriage_female = mean(marriage2b, na.rm = TRUE)
    ) %>%
    rename("year" = !!date_var)

  final_output <- marriage_summary

  if (is.null(output_path)){
    return(final_output)
  } else {
    write.csv(final_output, paste0(output_path, tablename, ".csv"), row.names = FALSE)
    return(final_output)
  }
}
