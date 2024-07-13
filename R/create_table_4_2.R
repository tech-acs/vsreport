#' Creates Table 4.2
#'
#' @description
#' Live births by place of occurrence and sex of newborn, year
#'
#' Variables required:
#' \itemize{
#'   \item Date of occurrence (birth1a)
#'   \item Place of occurrence (birth1c)
#'   \item Type of place of occurrence (hospital, home, etc.) (birth1i)
#'   \item Sex (birth2a)
#' }
#'
#' @param data data frame being used
#' @param date_var name of date variable
#' @param data_year year the data is for
#' @param tablename name of the table being saved as a csv file
#' @param output_path The path to export the generated table
#'
#' @return data frame of tabulated counts
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import janitor
#'
#' @examples t4.2 <- create_t4.2(birth_data, birth1a, data_year = 2022)
create_t4.2 <- function(data, date_var, data_year = 2022, tablename = "Table_4_2", output_path = NULL){
  output <- data |>
    na.omit(sbind) |>
    mutate(birth1a = year(birth1a)) |>
    filter({{date_var}} == data_year) |>
    group_by(birth1i, birth2a) |>
    summarise(total = n()) |>
    pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
    adorn_totals(c("row","col")) |>
    mutate(ratio = round_excel(Male/Female,1)) |>
    mutate(completeness = round_excel(Total/Total*100, 1))
    select("birth1i", "Male", "Female", "not stated", "Total", "ratio", "completeness")
    
  if (is.null(output_path)){
    return(output)
  } else {
    write.csv(output, paste0(output_path, tablename, ".csv"), row.names = FALSE)
    return(output)
  }
}

