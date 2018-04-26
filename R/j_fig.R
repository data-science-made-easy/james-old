#' Easy plotting in your standard layout
#'
#' Plot the data at position 'index' in j_ls(), given its meta data
#'
#' @param index id of data you want to plot
#' @param meta settings you want to use for your figure. These settings are not stored and will be used only once.
#'
#' @return None
#'
#' @export j_fig

j_fig <- function(index, meta) {
  x <- j_get(index, what = "data")
  m <- j_get(index, what = "meta")
  
  
  
  # Get x-axis
  #x_at
  #x_at_lab
  
    # 1 str_replace_all(t_vec_labels, "\\\\n", "\n")
    # 2 chartr(".", get_settings()$decimal_sep, as.character(t_vec_labels))    
    # 3 Give x_at_lab same length as x_at
  
  # Remove x-axis
  x <- x[, -1, drop = FALSE]
  
  # Use series_type from Excel or set defaults
  # ?Get color scheme
}
