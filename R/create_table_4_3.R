#' Create Table 4.3
#'
#' @description
#' Live births by place of occurrence and place of usual residence of mother, year
#'
#' Variables required:
#' \itemize{
#'   \item Date of occurrence (birth1a)
#'   \item Place of occurrence (birth1c)
#'   \item Place of usual residence - Mother (birth3l)
#' }
#'
#' @param data Birth registration data
#' @param date_var Date of occurrence variable
#' @param data_year Chosen year
#' @param tablename Name for csv output use _ instead of . for names
#'
#' @return Data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import janitor
#'
#' @note
#' The Vital Statistics \href{https://www.vitalstrategies.org/resources/production-of-a-vital-statistics-report/}{report resource kit}
#' includes guidance and templates to help complete your CRVS report.
#'
#' @examples table4_3 <- create_t4.3(sample_data1, date_var = birth1a, data_year = 2021, tablename = "Table_4_3")

create_t4.3 <- function(data, date_var, data_year = 2022, tablename = "Table_4_3"){
  output <- data |>
    na.omit(sbind) |>
    mutate(birth1a = year(birth1a)) |>
    filter({{date_var}} == data_year) |>
    group_by(birth1c, birth3l) |>
    summarise(total = n()) |>
    pivot_wider(names_from = birth1c, values_from = total, values_fill = 0) |>
    adorn_totals(c("col", "row"))

  #write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}



