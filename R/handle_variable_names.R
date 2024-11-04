#' Rename and Validate Dataframe Variables Based on Configuration
#'
#' This function takes a dataframe and a configuration dictionary of origin variables, checks
#' if specified variables are present, renames them, validates their types, and returns the modified
#' dataframe and a list of missing variable IDs.
#'
#' @param df A dataframe containing data to be processed.
#' @param origin_variables A list of variable mappings from the configuration file with fields
#'        `variable_id`, `variable_name`, and `variable_type`.
#'
#' @return A list containing:
#'   \item{dataframe}{The modified dataframe with renamed variables.}
#'   \item{missing_vars}{A character vector of variable_ids that were not found in the dataframe.}
#' @export
#'
#' @examples
#' # Assuming 'df' is your dataframe and 'origin_variables' is loaded from config.yml
#' result <- handle_variable_names(df, origin_variables)
#' modified_df <- result$dataframe
#' missing_vars <- result$missing_vars
handle_variable_names <- function(df, origin_variables) {
  # Initialize vector to collect missing variable_ids
  missing_vars <- character()

  # Iterate over each variable in the configuration dictionary
  for (var in origin_variables) {
    var_name <- var$variable_name
    var_id <- var$variable_id
    var_type <- var$variable_type

    # Check if the variable name exists in the dataframe
    if (var_name %in% names(df)) {
      # Rename the variable
      names(df)[names(df) == var_name] <- var_id

      # Check variable type
      actual_type <- class(df[[var_id]])
      if (!actual_type %in% var_type) {
        message(sprintf("Warning: Variable '%s' is expected to be of type '%s' but is '%s'.",
                        var_id, var_type, actual_type))
      }
    } else {
      # If the variable is missing, add to missing_vars
      missing_vars <- c(missing_vars, var_id)
      message(sprintf("Variable '%s' (ID: %s) not found in dataframe.", var_name, var_id))
    }
  }

  # Return modified dataframe and list of missing variable_ids
  return(list(dataframe = df, missing_vars = missing_vars))
}
