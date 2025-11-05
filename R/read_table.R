#' Read variable table from CSV
#'
#' @param fname File name, default "var_table.csv"
#' @param dir Directory, default "rds_files"
#' @return Variable table data.frame
#' @export
var_read_table <- function(fname = "var_table.csv", dir = "rds_files") {
    read.csv(file.path(dir, fname), stringsAsFactors = FALSE)
}

