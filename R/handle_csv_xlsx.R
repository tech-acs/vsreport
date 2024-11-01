#' Convert CSV Files to an Excel Workbook
#'
#' This function reads all CSV files in a specified directory and writes their contents to separate sheets in a single Excel workbook.
#'
#' @param path A character string specifying the directory containing the CSV files. Defaults to the current working directory (".").
#' @param output_path A character string specifying the directory and file name to write the xlsx to.
#' @return An Excel file named "output.xlsx" containing the contents of the CSV files.
#' @examples
#' # Convert CSV files in the current directory to an Excel workbook
#' handle_csv_xlsx(path = ".")
#'
#' # Convert CSV files in a specified directory to an Excel workbook
#' handle_csv_xlsx(input_path = "path/to/directory", output_path = "path/with/file/output.xlsx")
#' @import readr
#' @import fs
#' @import writexl
#' @importFrom purrr map set_names
#'
#' @export
handle_csv_xlsx <- function(input_path = ".", output_path = "output.xlsx") {
  file_list <- list.files(path = input_path, pattern = "*.csv", full.names = T)

  if (length(file_list) == 0) {
    warning("There are no .csv files in this directory")
    return(invisible(NULL))
  }

  table_list <- file_list |>
    purrr::map(~readr::read_csv(.), .id = "id") |>
    purrr::set_names(tools::file_path_sans_ext(fs::path_file(file_list)))

  writexl::write_xlsx(table_list, output_path)
}
