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

#' Extract x-axis information from d
#' @param d retrieve x-axis
#' @keywords internal
extract_x_axis <- function(d) {
  # TODO Replace \n by newline, dot by comma, etc...
  if (is_really_character(d[, 1])) {
    x_at <- 1:nrow(d)
    x_at_lab <- d[, 1]
  } else {
    x_at <- as.numeric(d[, 1])
    x_at_lab <- NULL
  }
  
  list(x_at = x_at, x_at_lab = x_at_lab)
}
