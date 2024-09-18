#' Handle Data Year
#'
#' This function takes the data year, if empty it will select the last one.
#'
#' @param data_year The data year to use for filtering (NULL by default).
#' @param data The dataset
#' @param date_var The variable with the dates, in years
#'
#' @return The data year to use for filtering, last one if none passed in.
#' @export
#'
#' @examples handle_data_year <- function(data_year = NULL, data, date_var)
handle_data_year <- function(data_year = NULL, data, date_var){
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }
  return(data_year)
}
