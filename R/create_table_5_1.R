
#' Calculates Table 5.1 Deaths summary table
#'
#' Table 5.1 Summary statistics on mortality by year of occurrence
#'
#' @param data deaths data frame
#' @param est_data estimate data frame
#' @param pops population data frame
#' @param num_yrs number of years to report on
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' death_data <- construct_sample_death_data()
#' estim_death_data <- construct_sample_estim_death_data()
#' sample_pop_estim <- construct_sample_pop_estim()
#' death_data <- construct_year(death_data, date_col = "death1a", year_col = "dodyr")
#' table_5_1 <- create_t5.1(data = death_data, est_data = estim_death_data, pops = sample_pop_estim, tablename = "Table_5_1")
#'
create_t5.1 <- function(data, est_data, pops, num_yrs=5,
                        tablename = "Table_5_1", output_path = NULL){

  max_year <- data %>% pull(dodyr) %>% max(na.rm = TRUE)
  years <- construct_year_sequence(max_year, num_yrs = num_yrs)

  output <- data |>
    filter(dodyr %in% years & death2c != "not stated")|>
    group_by(death2c, dodyr) |>
    rename(Indicator = death2c) |>
    summarise(total = n())

  output_counts <- output |>
    pivot_wider(names_from = Indicator, values_from = total)

  output_comp <- est_data |>
    group_by(year) |>
    summarise(ftotal = sum(female), mtotal = sum(male)) |>
    filter(year %in% years)

  output_comp <- cbind(output_counts, output_comp) |>
    mutate(male_comp = construct_round_excel(male/mtotal*100, 1),
           female_comp = construct_round_excel(female/ftotal*100, 1)) |>
    select(year, male_comp, female_comp) |>
    pivot_longer(cols = c(male_comp, female_comp), names_to = "Indicator", values_to =  "counts") |>
    pivot_wider(names_from = year, values_from = counts)

  output_counts <- output_counts |>
    pivot_longer(cols = c(male, female), names_to = "Indicator", values_to =  "counts") |>
    pivot_wider(names_from = dodyr, values_from = counts)

  population <- pops |>
    select(starts_with("popu"), birth2a) |>
    pivot_longer(cols = starts_with("popu"), names_to = "year", values_to = "count" ) |>
    mutate(year = gsub("population_", "", year)) |>
    group_by(year, birth2a) |>
    summarise(total_pop = sum(count)) |>
    arrange(birth2a, year)

  output_cdr <- output |>
    mutate(dodyr = as.character(dodyr)) |>
    left_join(population, by = c("dodyr" = "year", "Indicator" = "birth2a")) |>
    rename(year = dodyr) |>
    select(year, Indicator, total, total_pop ) |>
    group_by(year) |>
    summarise(total = sum(total), total_pop = sum(total_pop)) |>
    mutate(cdr = construct_round_excel((total/total_pop)*1000,2)) |>
    select(year, cdr) |>
    mutate(Indicator = "CDR") |>
    pivot_wider(names_from = year, values_from = cdr)

  output <- rbind(output_counts, output_comp, output_cdr)

  return(handle_table_output(output, output_path, tablename))
}
