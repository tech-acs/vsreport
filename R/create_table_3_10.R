#' Create Table 3.10
#'
#'Table 3.10 Adjustment of deaths by age group and birth2a of decedent
#'
#' @param data dataframe being used
#' @param dth_est dataframe being used with estimated Death data
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
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
#' @examples t3.10 <- create_t3.10(dth_data, date_var = dodyr, data_year = 2022, tablename = "Table_3_10")
#'
create_t3.10 <- function(data, dth_est, date_var, data_year = NA,
                         tablename = "Table_3_10", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  data_year <- handle_data_year(data_year, data, date_var)

  output <- data |>
    filter(!!sym(date_var) == data_year, birth2a != "not stated") |>
    group_by(age_grp_wide, birth2a) |>
    summarise(reg_deaths = n()) |>
    rename(age_grp = age_grp_wide)

  output_all <- output  |>
     mutate(birth2a = "total") |>
     group_by(age_grp, birth2a) |>
     summarise(reg_deaths = sum(reg_deaths))

  output <- rbind(output_all, output)

  d_est_prop <- dth_est  |>
    filter(year == data_year ) |>
    group_by(age_grp) |>
    summarise(female = sum(female), male = sum(male)) |>
    pivot_longer(cols = c(female, male) , names_to = "birth2a", values_to = "est_count")

  d_est_prop_all <- d_est_prop |>
    mutate(birth2a = "total") |>
    group_by(age_grp, birth2a) |>
    summarise(est_count = sum(est_count))

  d_est_prop <- rbind(d_est_prop_all, d_est_prop)

  output <- left_join(output, d_est_prop, by= c("age_grp", "birth2a")) |>
    mutate(completeness = construct_round_excel((reg_deaths/est_count),2)) |>
    mutate(adjusted = floor((reg_deaths/ completeness)),
           completeness = completeness * 100) |>
    select(-c(est_count)) |>
    pivot_wider(names_from = birth2a, values_from = c(reg_deaths, completeness, adjusted))

  return(handle_table_output(output, output_path, tablename))
}


