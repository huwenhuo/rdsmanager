#' Load variables from RDS based on variable table
#'
#' @param var_table Variable table (data.frame), default NULL
#' @param table_file Default CSV path if var_table is NULL, default "rds_files/var_table.csv"
#' @return TRUE invisibly
#' @export
var_load_rds_by_table <- function(var_table = NULL,
                                  table_file = "rds_files/var_table.csv") {

    if (is.null(var_table)) {
        if (!file.exists(table_file))
            stop("No table provided and default table file not found.")
        var_table <- read.csv(table_file, stringsAsFactors = FALSE)
    }

    for (i in seq_len(nrow(var_table))) {
        nm <- var_table$var[i]
        fn <- var_table$file[i]

        if (file.exists(fn)) {
            assign(nm, readRDS(fn), envir = .GlobalEnv)
        } else {
            warning("Missing RDS file: ", fn)
        }
    }

    invisible(TRUE)
}

