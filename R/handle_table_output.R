#' Handle Table Output
#'
#' This function takes a table and handles the output creation part.
#'
#' @param table The table to output
#' @param output_path The path to output the table, will be NULL by default
#' @param tablename The name of the .csv file with the table
#'
#' @return The table passed in, if an output path is given it will save the csv.
#' @export
#'
#' @examples handle_table_output <- function(table, output_path, tablename)
handle_table_output <- function(table, output_path = NULL, tablename){
  if (is.null(output_path)){
    return(table)
  } else {
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    write.csv(table, paste0(output_path, tablename, ".csv"), row.names = FALSE)
    return(table)
  }
}
