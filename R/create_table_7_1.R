#' Generate Table 7.1 Summary statistics on marriages and divorces by year of occurrence
#'
#' This function takes marriage and divorce data and generates a table with summary statistics on marriages and divorces by year.
#'
#' @param marriage_data The marriage data.
#' @param divorce_data The divorce data.
#' @param population_data The population data.
#' @param date_var The variable to use the year from
#' @param data_year A particular year to choose from
#'
#' @return A data frame containing the summary statistics on marriages and divorces by year.
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' create_t7.1(marriage_data, divorce_data, population_data)
create_t7.1 <- function(marriage_data, divorce_data, population_data, date_var = "domyr", data_year = NA) {
  date_var <- enquo(date_var)

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

  # Process divorce data
  divorce_summary <- divorce_data %>%
    filter(!!date_var %in% years) %>%
    group_by(!!date_var) %>%
    summarise(
      num_divorces = n(),
      avg_age_divorce_male = mean(divorce3b, na.rm = TRUE),
      avg_age_divorce_female = mean(divorce2b, na.rm = TRUE)
    ) %>%
    rename("year" = !!date_var)

  # Combine the summaries
  summary_table <- marriage_summary %>%
    full_join(divorce_summary, by = "year") %>%
    arrange(year)

  # Calculate crude rates using population data
  population_data_long <- population_data %>%
    gather(year, population, -birth1c, -birth2a, -age) %>%
    mutate(year = as.integer(gsub("population_", "", year)))

  summary_table <- summary_table %>%
    left_join(population_data_long %>% filter(age == 0) %>% select(year, population), by = c("year" = "year")) %>%
    mutate(
      crude_marriage_rate = (num_marriages / population) * 1000,
      crude_divorce_rate = (num_divorces / population) * 1000
    ) %>%
    select(year, num_marriages, crude_marriage_rate, avg_age_first_marriage_male, avg_age_first_marriage_female, num_divorces, crude_divorce_rate, avg_age_divorce_male, avg_age_divorce_female)

  return(summary_table)
}
