j_db_put <- function(meta) { # meta$d0 is data
  if (is_yes(meta$j_debug)) print(paste("IN j_db_put"))
  db_writable <- 0 == file.access(meta$j_db_path, mode = 2) 
  if (is_yes(meta$j_debug)) print(paste("db_writable:", db_writable))
  if (is_yes(meta$j_debug)) print(paste("j_db_path:", meta$j_db_path))
  if (!is_yes(meta$j_db) || !is.character(meta$j_db_table) || !is.character(meta$j_db_path) || !db_writable) return()
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = meta$j_db_path) # :dbname:
  if (is_yes(meta$j_debug)) print(paste("dbConnect"))
  
  this_data <- meta$d0
  meta$d0   <- NULL
  
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
  return(db_table)
}