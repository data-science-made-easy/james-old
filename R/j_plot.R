#' Easy plotting in your standard layout
#'
#' Plot the data at position 'index' in j_ls(), given its meta data
#'
#' @param index id of data you want to plot
#' @param meta settings you want to use for your figure. These settings are not stored and will be used only once.
#'
#' @return None
#'
#' @importFrom grDevices cm dev.off pdf
#' @importFrom graphics abline axis barplot legend mtext par plot
#' @export j_plot

j_plot <- function(index, meta = list()) {
  # Get data
  d <- as_data_frame(j_get(index, what = "data"))
  # Get meta data
  meta <- combine_lists(high_prio = meta, low_prio = j_get(index, what = "meta"))

  # Get info x,y-axis
  stopifnot(swap_xy(meta)) # TODO Remove this line

  #
  ## Put x,y-axis (as in data) info in meta
  #
  x_axis_data_info <- extract_x_axis(d) # returns x_at, x_at_lab (can be text)

  # Remove x-axis from data
  d <- df_as_matrix(d[, -1, drop = FALSE])

  # Now we are ready for y-data
  y_axis_data_info <- extract_y_axis(d, meta) # returns y_at only
  if (swap_xy(meta)) {
    stopifnot(all(names(x_axis_data_info) == c("x_at", "x_at_lab")))
    names(x_axis_data_info) <- c("y_at", "y_at_lab")
    stopifnot(all(names(y_axis_data_info) == "y_at"))
    names(y_axis_data_info) <- "x_at"
  } else {
    stop("TODO")
  }
  meta <- combine_lists(high_prio = meta, low_prio = c(x_axis_data_info, y_axis_data_info))
  
  # Add series names to meta
  meta <- combine_lists(high_prio = meta, low_prio = list(series_names = colnames(d)))
  
  # x,y-lim
  if (swap_xy(meta)) {
    lims_in_list <- list(x_lim = range(meta$x_at))
  }
  meta <- combine_lists(high_prio = meta, low_prio = lims_in_list)
  
  # Make all variables in meta of proper length, matching number of series
  meta <- one_element_for_each_series(meta)
  
  series_type <- meta[[META$series_type]] # We now have a value for each series
  any_series_beside <- is.element(SERIES_TYPE_BAR_NEXT, series_type)
  
  #
  ## Init PDF?
  #
  if (!is.null(meta$pdf)) pdf(meta$pdf, title = meta$pdf, width = meta$pdf_width / cm(1), height = meta$pdf_height / cm(1), pointsize = meta$pointsize)
  
  set_margins(meta)
  
  # Do the magic
  barplot(t(d), horiz = swap_xy(meta), beside = any_series_beside, names.arg = meta$y_at_lab, las = 1, col = NA, border = NA, axes = FALSE, xlim = meta$x_lim)
  add_axis_and_titles(meta)
  add_help_lines(meta)
  barplot(t(d), horiz = swap_xy(meta), beside = any_series_beside, names.arg = meta$y_at_lab, las = 1, col = meta$ts_col[1:ncol(d)], border = NA, axes = FALSE, add = TRUE, xlim = meta$x_lim)
  
  add_legend(meta)
  #
  ## Close PDF?
  #
  if (!is.null(meta$pdf)) dev.off()
}

swap_xy <- function(meta) is_yes(meta[[META$swap_xy]])

set_margins <- function(meta) {
  this_mai <- par()$mai * c(meta$margin_south, meta$margin_west, meta$margin_north, meta$margin_east)
  par(mai = this_mai)
}

add_help_lines <- function(meta) {
  if (swap_xy(meta))
    abline(v = meta$x_at, lwd = meta$help_lwd)
}

#' @keywords internal
add_axis_and_titles <- function(meta) {
  # title
  mtext(text = meta$title, side = 3, outer = T, adj = 0, at = 0.06, line = -2, font = 2, cex = meta$title_pt)
  # x-axis
  axis(1, at = meta$x_at, labels = meta$x_at_lab, lwd = 0, lwd.ticks = 0, xpd = TRUE, line = meta$v_shift_x_axis)
}

#' @keywords internal
add_legend <- function(meta) {
  # Return if nothing to do
  if (all(is.na(meta$series_names))) return()

  # Set margins
  opar <- par()
  on.exit(suppressWarnings(par(opar)))
  par(fig=c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0:1, 0:1, type="n", bty="n", xaxt="n", yaxt="n")

  # Set series specific legend
  meta <- set_series_specific_legend(meta)

  # Find optimal number of columns
  n_columns <- if (n_series(meta) < 3) 1 else 2
  
  # Do the magic
  #legend(0, .12, legend = meta$series_names, text.font = 3, lwd = meta$ts_lwd, col = meta$ts_col, bty = "n", ncol = n_columns, pch = meta$pch, pt.cex = 2)
  legend(0, .12, legend = meta$series_names, text.font = 3, lwd = meta$ts_lwd, col = meta$ts_col, pch = meta$pch, pt.cex = 2, bty = "n", x.intersp = 0)
}

#' @keywords internal
n_series <- function(meta) length(meta$series_names)

#' Meta may have variables of length 1 or length > n_items. This function makes all lists have length n_items.
#' @keywords internal
one_element_for_each_series <- function(meta) {
  n_items <- n_series(meta)
  if (1 == length(meta$series_type)) meta$series_type <- rep(meta$series_type, n_items) # Fix series_type
  meta$ts_col <- meta$ts_col[1:n_items] # Fix col
  meta$ts_lwd <- rep(meta$ts_lwd, n_items)#rep(meta$ts_lwd, n_items) # Fix lwd
  if (is.null(meta$pch)) meta$pch <- 1
  meta$pch    <- rep(meta$pch, n_items) # TODO Only set pch for BAR
  return(meta)
}

#' @keywords internal
set_series_specific_legend <- function(meta) {
  index_bar <- which(meta$series_type %in% SERIES_TYPE_BAR)
  meta$pch[index_bar] <- 15
  meta$ts_lwd[index_bar] <- NA # Block lines for bar plots

  return(meta)
}

#' Extract x-axis information from d
#' @param d data
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
  
  return(list(x_at = x_at, x_at_lab = x_at_lab))
}

#' Extract pretty y-axis information from d
#' @param d data
#' @keywords internal
extract_y_axis <- function(d, meta) {
  if (swap_xy(meta)) d <- c(0, d) # Include zero if barplot
  y_at <- pretty(d)
  if (max(y_at) < max(d))
    y_at <- c(y_at, tail(y_at, 1) + diff(y_at)[1])
  return(list(y_at = y_at))
}
































