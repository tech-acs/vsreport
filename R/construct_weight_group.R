#' Construct Age Group Column to Dataset
#'
#' This function takes a dataset and adds a new column that contains age groups.
#'
#' @param dataset A data frame containing at least one column with age values.
#' @param weight_col The name of the column containing age values.
#' @param group_size An integer specifying the size of the age groups. Default is 5.
#' @param new_col The name of the new column to store the age groups. Default is "fert_age_grp".
#' @return A data frame with the new column added, containing age groups.
#'
#' @export
#' @import dplyr
#'
#' @examples
#' df <- data.frame(id = 1:10, weights = c(23, 45, 34, 56, 29, 12, 67, 44, 33, 55))
#' new_df <- construct_age_group(df, age_col = "weights", group_size = 0.25)
#' print(new_df)
construct_weight_group <- function(dataset, weight_col, group_size = 0.5, new_col = "birth2c") {
  max_weight <- max(dataset[[weight_col]], na.rm = TRUE)
  breaks <- seq(0, max_weight + group_size, by = group_size)
  labels <- paste0( tail(breaks, -1) - 1 , "-",head(breaks, -1))
  print(labels)
  
  dataset <- dataset %>%
    mutate(!!new_col := cut(.data[[weight_col]],
                            breaks = breaks,
                            include.lowest = TRUE,
                            right = FALSE,
                            labels = labels))
  
  return(dataset)
}