j_ls_cols <- function(cols = c("index", "name", "title", "dim|class")) {
  df <- j_ls()
  
  for (i in seq_along(df$index)) {
    meta <- j_get(df$index[i], what = "meta")
    
    for (j in seq_along(cols)) {
      if (!is.element(cols[j], c("index", "dim|class")))
        if (!is.null(meta[[cols[j]]]))
          df[i, cols[j]] <- meta[[cols[j]]]
    }
  }
 
  # eliminate non-existent column names
  cols <- intersect(cols, colnames(df))
    
  df[, cols]
}