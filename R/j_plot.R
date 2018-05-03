#' Easy plotting in your standard layout
#'
#' Plot the data at position 'index' in j_ls(), given its meta data
#'
#' @param index id of data you want to plot
#' @param meta settings you want to use for your figure. These settings are not stored and will be used only once.
#'
#' @return None
#'
#' @export j_plot

j_plot <- function(index, meta) {
  d <- as_data_frame(j_get(index, what = "data"))
  m <- j_get(index, what = "meta")
  
  #
  ## X-axis
  #
  x_axis <- extract_x_axis(d)

  # Remove x-axis
  d <- d[, -1, drop = FALSE]
  
  # x,y-lim
  x_lim <- range(x_axis$x_at)
  y_lim <- range(d)
  
  
  
  # Use series_type from Excel or set defaults
  
  
  
  
}



#index=1
#j_plot(1)