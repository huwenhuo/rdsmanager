#' Save the variable table to CSV
#'
#' @param var_table Variable table data.frame
#' @param fname File name to save as, default "var_table.csv"
#' @param dir Directory to save files in, default "rds_files"
#' @return NULL
#' @export
var_save_table <- function(var_table, fname = "var_table.csv", dir = "rds_files") {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    write.csv(var_table, file.path(dir, fname), row.names = FALSE)
}

