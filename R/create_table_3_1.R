#' Create Table 3.1
#'
#' Table 3.1 Number of vital events by registration timeliness, year
#'
#' @param bth_data Birth Registration Data
#' @param dth_data Death Registration Data
#' @param bth_yr_var Date of occurrence variable (Birth Data)
#' @param dth_yr_var Date of occurrence variable (Death Data)
#' @param tablename Name for csv output use _ instead of . for names
#' @param output_path The path to export the generated table
#'
#' @return Data frame with tabulated results (if an output_path is given, it will export a .csv)
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.1 <- create_t3.1(bth_data = bth_data, dth_data = dth_data, bth_yr_var = dobyr, dth_yr_var = dodyr)
#'
create_t3.1 <- function(bth_data, dth_data, bth_yr_var, dth_yr_var, tablename = "Table_4_3", output_path = NULL){
  max_value <- bth_data %>% pull({{bth_yr_var}}) %>% max(na.rm = TRUE)

  outputb <- bth_data |>
    filter(is.na(sbind) & {{bth_yr_var}} %in% c((max_value - 5) : (max_value - 1))) |>
    group_by({{bth_yr_var}}, timeliness) |>
    summarise(total = n()) |>
    mutate(type := "1 Live births") |>
    rename(year = {{bth_yr_var}})
  outputd <- dth_data |>
    filter({{dth_yr_var}} %in% c((max_value - 5) : (max_value - 1))) |>
    group_by({{dth_yr_var}}, timeliness) |>
    summarise(total = n()) |>
    mutate(type := "2 Deaths") |>
    rename(year = {{dth_yr_var}})

  output <- rbind(outputb, outputd) |>
    pivot_wider(names_from = c(year, type), values_from = total, values_fill = 0, names_sep = " ", names_sort =  TRUE) |>
    arrange(match(timeliness, c("Current", "Late", "Delayed"))) |>
    adorn_totals("row", name = "Grand total")

  write.csv(output, "./outputs/Table_3_1.csv", row.names = FALSE)

  return(output)
}
