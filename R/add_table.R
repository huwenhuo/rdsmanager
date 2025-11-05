#' Add a variable to the variable table
#'
#' @param varname Name of the variable in the global environment
#' @param filename File name to save the variable as
#' @param var_table Existing variable table (data.frame or file path), default NULL
#' @return Updated variable table (data.frame)
#' @export
var_add_to_table <- function(varname, filename, var_table = NULL) {

    # Load from file if table is a file name
    if (is.character(var_table) && length(var_table) == 1) {
        if (file.exists(var_table)) {
            var_table <- read.csv(var_table, stringsAsFactors = FALSE)
        } else {
            stop("Table file not found: ", var_table)
        }
    }

    # If no table provided, create empty one
    if (is.null(var_table)) {
        var_table <- data.frame(
            var  = character(),
            file = character(),
            stringsAsFactors = FALSE
        )
    }

    # Remove old entry
    var_table <- subset(var_table, var != varname)

    # Add new entry
    var_table <- rbind(var_table, data.frame(var = varname, file = filename))

    return(var_table)
}

