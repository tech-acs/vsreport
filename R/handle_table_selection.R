#'@title Handle Table Selection
#'
#'@description A function that takes the user defined selection of tables to run from the config and will generate a list of tables to run.
#'
#'@details Given a section_num number, it will go through the dictionary of all tables and return those that belong to that section by looking at the start of the name.
#'
#'@param section_num A string 
#'
#'@param all_tables A dictionary with all the tables and their information
#'
#'@return A list of table ids that belong to the section_num number
#'
#'@examples sect_tables("3", all_tables)
#'
#'@export

handle_table_selection <- function(section_num, all_tables){
    # Extract all table_ids that start with the prefix
    matching_table_ids <- sapply(all_tables, function(x) {
    if (startsWith(x$table_id, section_num)) {
        return(x$table_id)
    } else {
        return(NULL)
    }
    })
    # Remove NULL values from the result
    matching_table_ids <- matching_table_ids[!sapply(matching_table_ids, is.null)]
    # Return the result
    return(matching_table_ids)
}
