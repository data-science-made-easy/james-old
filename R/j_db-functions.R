j_db_put <- function(meta) { # meta$d0 is data
  if (is_yes(meta$j_debug)) print(paste("IN j_db_put"))
  db_writable <- 0 == file.access(meta$j_db_path, mode = 2) 
  if (is_yes(meta$j_debug)) print(paste("db_writable:", db_writable))
  if (is_yes(meta$j_debug)) print(paste("j_db_path:", meta$j_db_path))
  if (!is_yes(meta$j_db) || !is.character(meta$j_db_table) || !is.character(meta$j_db_path) || !db_writable) return()
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = meta$j_db_path) # :dbname:
  if (is_yes(meta$j_debug)) print(paste("dbConnect"))
      
  # Remove data
  this_data <- meta$d0
  meta$d0   <- NULL
  
  # Add extra meta data statistics
  meta$j_db_put_timestamp <- unclass(Sys.time())
  meta$j_db_put_path      <- getwd()
  
  meta_df <- list_as_df(meta)
  meta_df_fields <- colnames(meta_df)
  
  if (!DBI::dbExistsTable(con, meta$j_db_table)) {
    DBI::dbWriteTable(con, meta$j_db_table, meta_df)
  } else {
    if (all(is.element(meta_df_fields, DBI::dbListFields(con, meta$j_db_table)))) {
      # We can just simply append
      DBI::dbWriteTable(con, meta$j_db_table, meta_df, append = TRUE)
    } else {
      db_table        <- DBI::dbReadTable(con, meta$j_db_table)
      db_table_filled <- plyr::rbind.fill(meta_df, db_table)
      DBI::dbWriteTable(con, meta$j_db_table, db_table_filled, overwrite = TRUE)
    }
  }

  DBI::dbDisconnect(con)
  if (is_yes(meta$j_debug)) print(paste("dbDisconnect"))
}

list_as_df <- function(lst) {
  lst_as_df <- as.data.frame(t(unlist(lapply(lst,toString))))
  rownames(lst_as_df) <- NULL
  lst_as_df
}

j_db_get_stats <- function(j_db_path = c('/Volumes/p_jamesgebruiker/stats/j_db.sqlite', 'M:/p_jamesgebruiker/stats/j_db.sqlite')[1], j_db_table = "jls") {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = j_db_path) # :dbname:
  db_table        <- DBI::dbReadTable(con, j_db_table)
  DBI::dbDisconnect(con)
  
  mat <- as.data.frame(unique(cbind(user = db_table$user, timestamp = NA, path = db_table$j_db_put_path)), stringsAsFactors = F)
  if (length(nrow(mat))) {
    for (i in 1:nrow(mat)) {
      index <- which(db_table$user == mat$user[i] & db_table$j_db_put_path == mat$path[i])
      mat$timestamp[i] <- as.numeric(max(db_table$j_db_put_timestamp[index]))
    }
  
    index <- sort(mat$timestamp, decreasing = T, index.return = TRUE)$ix
    mat <- mat[index, ]

    mat$timestamp <- as.POSIXct(as.numeric(mat$timestamp), origin = "1970-01-01")
    
    # Enrich user names with full names
    un_path <- file.path(system.file("extdata", package = "james"), "usernames.RData")
    if (file.exists(un_path)) {
      un <- dget(un_path)
      un_vec <- NULL
      for (i in 1:nrow(mat)) {
        un_vec[i] <- un[which(un[, "username"] == mat$user[i]), 2]
      }
      mat <- cbind(mat[, 1], un_vec, mat[, 2:ncol(mat)])
    }
    
  }  
  
  return(mat)
}





















