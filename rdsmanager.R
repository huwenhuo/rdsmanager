library(R6)

RDSManager <- R6Class(
  "RDSManager",
  
  private = list(
    var_table = data.frame(var = character(), file = character(), stringsAsFactors = FALSE),
    dir = "rds_files",
    wd  = character(),
    saved = logical(),        # TRUE if already saved, FALSE if new/modified
    table_file = "var_table.csv"
  ),
  
  public = list(
    
    initialize = function(dir = "rds_files", table_file = "var_table.csv") {
      private$dir <- dir
      private$table_file <- table_file
      if (!dir.exists(private$dir)) dir.create(private$dir, recursive = TRUE)
      
      path <- file.path(private$dir, private$table_file)
      if (file.exists(path)) {
        private$var_table <- read.csv(path, stringsAsFactors = FALSE)
        message("Loaded existing table from ", path)
      } else {
        message("Initialized empty table and folder ", private$dir)
      }
    },
    
    add_var = function(...) {
      args <- list(...)
      arg_exprs <- substitute(list(...))[-1]

      for (i in seq_along(args)) {
        # Determine variable name to store in table
        nm <- names(args)[i]

        if (nm == "" || is.null(nm)) {
          # If not named, capture the original variable name
          nm <- deparse(arg_exprs[[i]])
        }

        fn <- file.path(private$dir, paste0(nm, ".rds"))

        # Remove existing entry if present
        private$var_table <- subset(private$var_table, var != nm)

        # Add to var_table
        private$var_table <- rbind(private$var_table, data.frame(var = nm, file = fn, saved = F, wd = getwd()))

        # Assign to global environment under the table name
        assign(nm, args[[i]], envir = .GlobalEnv)
      }
    },
    
    remove_var = function(..., delete_file = FALSE) {
      var_names <- sapply(substitute(list(...))[-1], deparse)
      
      for (nm in var_names) {
        row_idx <- which(private$var_table$var == nm)
        if (length(row_idx) == 0) {
          warning("Variable not found in table: ", nm)
          next
        }
        
        if (delete_file) {
          fn <- private$var_table$file[row_idx]
          if (file.exists(fn)) {
            file.remove(fn)
            message("Deleted RDS file: ", fn)
          }
        }
        
        private$var_table <- private$var_table[-row_idx, ]
        message("Removed variable from table: ", nm)
      }
    },
    
    save_table = function() {
      write.csv(private$var_table, file.path(private$dir, private$table_file), row.names = FALSE)
    },
    
    save_rds = function() {
      for (i in seq_len(nrow(private$var_table))) {
        nm <- private$var_table$var[i]
        fn <- private$var_table$file[i]
        private$var_table$wd[i] = getwd()
        saved <- private$var_table$saved[i]
        if (exists(nm, envir = .GlobalEnv) & saved == F) saveRDS(get(nm, envir = .GlobalEnv), fn)
	var_table$saved[i] <- TRUE
      }
    },
    
    load_rds = function() {
      for (i in seq_len(nrow(private$var_table))) {
        nm <- private$var_table$var[i]
        fn <- private$var_table$file[i]
        if (file.exists(fn)) assign(nm, readRDS(fn), envir = .GlobalEnv)
      }
    },
    
    list_vars = function() {
      if (nrow(private$var_table) == 0) {
        message("No variables tracked.")
      } else {
        print(private$var_table)
      }
    }
  )
)

