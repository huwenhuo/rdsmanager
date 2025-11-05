#' Save variables listed in variable table as RDS
#'
#' @param var_table Variable table data.frame
#' @return NULL
#' @export
var_save_rds <- function(var_table) {
    for (i in seq_len(nrow(var_table))) {
        nm <- var_table$var[i]
        fn <- var_table$file[i]

        if (exists(nm, envir = .GlobalEnv)) {
            saveRDS(get(nm, envir = .GlobalEnv), fn)
        } else {
            warning("Variable not found: ", nm)
        }
    }
}

