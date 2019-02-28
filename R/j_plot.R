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

j_plot <- function(index, meta = list()) { # TODO Naast index ook via 'tab name'
  # Get meta data
  meta <- combine_lists(high_prio = meta, low_prio = j_get(index, what = "meta"))
  if (is_no(meta$create)) return(invisible("No plot (is_no(meta$create))"))

  # Get data
  # Add data to meta data as 'd0'; strip x-axis and add as 'd'
  meta$d0 <- as_data_frame(j_get(index, what = "data"))

  meta <- pre_process_meta(meta) # d0 = as_data_frame(j_get(index, what = "data"))

  file_base_name <- get_param("name", meta, get_param("tab", meta, "unknown"))
  if (!has_value(meta$pdf)) meta$pdf <- paste0(meta$dir_pdf, file_base_name, ".pdf")
  if (!has_value(meta$png)) meta$png <- paste0(meta$dir_png, file_base_name, ".png")

  # Init PDF?
  if (!is_no(meta$pdf)) { #TODO solve differently
    if (!file.exists(meta$dir_pdf)) dir.create(meta$dir_pdf, showWarnings = FALSE)
    if (!file.exists(meta$dir_png)) dir.create(meta$dir_png, showWarnings = FALSE)
    create_pdf(meta) # Init the PDF
  }

  # Set margins
  set_margins(meta)

  #
  ## Plot (start in background and continue to foreground)
  #
  if (is_class_ppower(meta)) {
    plot_ppower(meta)
  } else {
    plot(NA, axes = FALSE, ann = FALSE, xlim = meta$x_lim, ylim = meta$y_lim) 
  }
  
  # Future
  if (!is.null(meta$future)) {
    rect(meta$future, par("usr")[3], par("usr")[2], par("usr")[4], col = meta$col_future, border = NA)
    text(par("usr")[4], par("usr")[3], "cpb.nl", pos = c(1,4))
  }

  # Axis and titles
  if (!is_class_ppower(meta) && !is_class_pie(meta)) #  && !is_class_heatmap(meta)
    add_axis_gridlines_and_userlines(meta)
  if (!is_class_ppower(meta))
    add_titles(meta)

  # PIE
  if (is_class_pie(meta))
    plot_pie(meta)
  
  if (is_class_heatmap(meta))
    plot_heatmap(meta)

  # HLINE_BOLD
  abline(h = meta$hline_bold, v = meta$vline_bold, lwd = meta$lwd_hline_bold, col = "#000000")

  # FANS
  if (0 < meta$n_fan)
    plot_fans(meta)
  
  # BARS
  for (i_type in meta$bar_air_index) {
    if (0 == which(i_type == meta$bar_air_index) %% 2) next
    plot_bar_next(i_type, meta)
  } 
  for (i_type in meta$bar_next_index) {
    plot_bar_next(i_type, meta)
  }  
  for (i_type in meta$bar_top_index) {
    meta <- plot_bar_top(i_type, meta) # Also update offsets for bar='s
  }
  for (i_type in meta$whisker_index) {
    if (0 == which(i_type == meta$whisker_index) %% 2) next
    plot_bar_next(i_type, meta)
  }
  
  if (0 < meta$n_barv) { # bold ref-line after plotting last bar #TODO Do only once!
    abline(h = meta$hline_bold, v = meta$vline_bold, lwd = meta$lwd_hline_bold, col = "#000000")
  }

  # DOTS (small)
  for (i_type in meta$dot_small_index) {
    plot_line_dot(i_type, meta)
  }
  
  # LINES
  for (i_type in meta$lines_index) {
    plot_line_dot(i_type, meta)
  }

  # DOTS (big)
  for (i_type in meta$dot_index) {
    plot_line_dot(i_type, meta)
  }

  # USER LINES
  add_lines_user(meta)
  
  # MARKS
  for (i_type in meta$mark_index) {
    plot_line_dot(i_type, meta)
  }
  
  # HELP LINES
  add_help_lines(meta)
  
  # MARKS
  add_mark(meta)
  
  # LABELS
  add_text_labels(meta)
  
  # # Now plot consecutive series from last (background) to first (foreground)
  # n_series <- length(meta$series_type)
  # for (i_type in n_series:1) {
  #   this_type <- meta$series_type[i_type]
  #   if (SERIES_TYPE_BAR_NEXT == this_type) {
  #     plot_bar_next(i_type, meta)
  #   } else if (SERIES_TYPE_BAR_TOP == this_type) {
  #     meta <- plot_bar_top(i_type, meta) # Also update offsets for bar='s
  #   } else if (is_line_type(this_type)) {
  #     plot_line_dot(i_type, meta, this_type)
  #   } else if (is.element(this_type, SERIES_TYPE_BARV)) {
  #     plot_barh(i_type, meta)
  #   }
  # }

  # Legend
  if (!is_class_ppower(meta))
    add_legend(meta)
  
  # Add 'draft' if meta$edit has value
  if (has_value(meta$draft)) add_draft()
  
  # Close PDF + create PNG
  if (!is_no(meta$pdf)) {
    dev.off()
    
    # Create PNG
    if (!has_value(meta$png)) meta$png <- paste0(get_param("name", meta, str_sub(meta$pdf, end = -5)), ".png")
    if ("unix" == .Platform$OS.type) { # Create PNG
      system(paste("sips -s format png", meta$pdf, "--out", meta$png))
    } else { # assume CPB/Windows environment
      if (file.exists(meta$ghostscript_executable)) {
        system(paste0(meta$ghostscript_executable, ' -dNOPAUSE -dBATCH -r', meta$ghostscript_resolution, ' -sDEVICE=png16m -sOutputFile="', meta$png, '" "', meta$pdf, '"'))
      } else {
        meta$png <- "NO PNG (please check meta$ghostscript_executable in james-settings.xlsx)"
      }
    }
    
    print(paste("James created", meta$pdf, "and", meta$png))
  }
}

plot_ppower <- function(meta) {
  dat  <- meta$d0
  cats <- unique(dat$category)
  vars <- unique(dat$variables)
  y_labs <- c(cats[1],
              "",
              cats[2],
              vars[2:6],
              "",
              cats[3],
              vars[7:9],
              "",
              cats[4],
              vars[10:12],
              "",
              cats[5],
              vars[13:14]
  )

  make_bold <- which(rev(y_labs) %in% cats)
  
  x_lim <- range(dat[, 5:9])
  x_lim[1] <- floor(x_lim[1])
  x_lim[2] <- ceiling(x_lim[2])
  
  plot(NA, axes = F, xlab = "", ylab = "", xlim = x_lim, ylim = c(1, 22))
  dat_all <- meta$d0
  years <- unique(dat$year)
  year_i = 1
  for (year_i in seq_along(years)) {
    year <- years[year_i]
    dat <- dat_all[which(year == dat_all$year), ]

    plot_dist <- .1
    if (1 == year_i) {
      par(mai = c(0, 0, 1, plot_dist))
    } else {
      par(mai = c(0, plot_dist, 1, 0))    
    }
    plot(NA, axes = F, xlim = x_lim, ylim = c(1, 22), xlab = "", ylab = "")
    rect(x_lim[1], par("usr")[3], x_lim[2], par("usr")[4], col = "#E5F2FA", border = NA)
    if (1 == year_i) {
      axis(2, at = (1:length(y_labs))[-make_bold], rev(y_labs)[-make_bold], las = 2, lwd = 0, font = 3)
      axis(2, at = (1:length(y_labs))[make_bold], rev(y_labs)[make_bold], las = 2, lwd = 0, font = 4)
    }
    axis_3_at <- axis(3)
    mtext(dat$year[1], side = 3, line = 3, font = 4, cex = 1.4)
    mtext("mutaties in %     ", side = 3, adj = 1, line = 2, font = 3)
    for (i in seq_along(axis_3_at)) {
      lines(x = rep(axis_3_at[i], 2), y = c(par("usr")[3:4]), col = "white", lwd = 2)
    }


    y_vars <- c(1:2, 5:7, 10:12, 15:19, 22)
    index_year0 <- 1:nrow(dat)
    dat_rev <- cbind(line = 1:length(index_year0), dat[rev(index_year0), ])
    for (i in seq_along(index_year0)) {
      y_i <- y_vars[i]
      text(x_lim[1], y_i, paste0(round(dat_rev$low[i]), "%"), pos = 4, col = "#688B73", font = 3)
      text(x_lim[2], y_i, paste0(round(100 - dat_rev$low[i]), "%"), pos = 2, col = "#688B73", font = 3)
  
      lines(x = c(dat_rev$q5[i], dat_rev$q95[i]), y = rep(y_i, 2), col = "#66B0DE")
      rect(xleft = dat_rev$q25[i], ybottom = y_i - .25, xright = dat_rev$q75[i], ytop = y_i + .25, col = "#66B0DE", border = NA)
  
      lines(x = rep(dat_rev$q50[i], 2), y = y_i + c(-.3, +.3), lwd = 3, col = "#137ABF")
      median_txt <- gsub("\\.", ",", round(dat_rev$q50[i], 1))
      text( x = dat_rev$q50[i], y = y_i - .5, median_txt, col = "#137ABF", font = 4)
    }
  }
}

create_pdf <- function(meta) {
  if (is_class_ppower(meta)) {
    this_width  <- meta$pdf_ppower_width / cm(1)
    this_height <- meta$pdf_ppower_height / cm(1)      
  } else {
    this_width  <- meta$pdf_width / cm(1)
    this_height <- meta$pdf_height / cm(1)
  }

  pdf(meta$pdf, title = as.character(get_param("name", meta, "")), width = this_width, height = this_height, pointsize = if (is_class_ppower(meta)) 12 else meta$pointsize)
}

plot_pie <- function(meta) {
  # Fix col and pie values (slice area)
  this_col <- meta$col_default[1:nrow(meta$d)]
  slice_value <- if (has_value(meta$slice_value)) meta$slice_value else paste0(round(meta$d[, 1]), meta$pie_unit)
  index_empty <- which(is.na(meta$x_at_lab))
  if (length(index_empty)) {
    this_col[index_empty] <- NA # No colour if no name
    slice_value <- slice_value[-index_empty]
  }
  
  par(new=T)
  pie(meta$d[, 1], labels = slice_value, clockwise = TRUE, border = FALSE, col = this_col)
}

get_heatmap_col <- function(val, meta) {
  rgb(colorRamp(meta$col_heatmap)((val - meta$z_lim[1]) / diff(meta$z_lim)), maxColorValue=255)
}

plot_heatmap <- function(meta) {
  d <- meta$d
  d <- d[nrow(d):1, ] # flip for handy plotting
  delta <- 1/2

  for (i in 1:nrow(d)) for (j in 1:ncol(d)) {
    rect(xleft = j - delta, ybottom = i - delta, xright = j + delta, ytop = i + delta, border = NA, col = get_heatmap_col(d[i, j], meta))
  }
}

plot_fans <- function(meta) {
  index <- meta$fan_index
  for (i in seq_along(index)) {
    if (i %% 2 == 0) next
    lower <- meta$d[, index[i]]
    upper <- meta$d[, index[1 + i]]
    polygon(x = c(meta$x, rev(meta$x)), y = c(lower, rev(upper)), col = meta$col_fan[(1 + i) / 2], border = NA)
  }
}

add_mark <- function(meta) {
  txt <- paste(meta$mark_xy, collapse = ",")
  if (has_value(txt)) {
    vec <- as_char_vec(txt, ";")
    for (i in seq_along(vec)) {
      xy <- as_numeric_vec(vec[i])
      points(xy[1], xy[2], pch = 13, cex = 2, col = meta$col_mark, lwd = meta$mark_lwd) #TODO use vars in Excel
    }
  }
}

add_text_labels <- function(meta) {
  txt    <- paste(meta$txt_lab, collapse = ",")
  xy_pos <- paste(meta$txt_lab_xy_pos, collapse = ",")
  if (has_value(txt) && has_value(xy_pos)) {
    txt_lab_sep <- as_char_vec(txt, ";")
    txt_lab_xy_pos_sep <- as_char_vec(xy_pos, ";")
    for (i in 1:length(txt_lab_sep)) {
      txt     <- txt_lab_sep[i]
      xyp     <- txt_lab_xy_pos_sep[i]
      xy_pos  <- as_numeric_vec(xyp)
      text(xy_pos[1], xy_pos[2], str_replace_all(txt, "\\\\n", "\n"), pos = if (is.na(xy_pos[3])) NULL else xy_pos[3], col = meta$fg_txt_col, xpd = TRUE)
    }    
  }  
}

add_draft <- function() {
  title(main="DRAFT ", outer=T, adj=1, line = -1, col.main = "red", font.main = 4)
}

is_line_type <- function(this_type) {
  "line" == str_sub(this_type, 1, 4)
}

set_class <- function(meta) {
  meta$class <- CLASS_DEFAULT
  if (SERIES_TYPE_PPOWER == meta$series_type[1])
    meta$class <- CLASS_PPOWER
  if (SERIES_TYPE_PIE == meta$series_type[1])
    meta$class <- CLASS_PIE
  if (SERIES_TYPE_HEATMAP == meta$series_type[1])
    meta$class <- CLASS_HEATMAP
  return(meta)
}

is_class_default <- function(meta) CLASS_DEFAULT == meta$class
is_class_ppower  <- function(meta) CLASS_PPOWER  == meta$class
is_class_pie     <- function(meta) CLASS_PIE     == meta$class
is_class_heatmap <- function(meta) CLASS_HEATMAP == meta$class

pre_process_spike_colors <- function(meta) {
  any_fan <- any(meta$series_type %in% SERIES_TYPE_FAN)
  if (meta$n_two_columns) {
    col_new <- rep(NA, ncol(meta$d))
    two_column_series_names <- unique(meta$series_names[meta$two_columns_index])
    # First fill colors for fan from special palette
    for (i_name in seq_along(two_column_series_names)) {
      name <- two_column_series_names[i_name]
      index <- which(name == meta$series_names)
      if (any(meta$series_type[index] %in% SERIES_TYPE_FAN)) { # index == fan
        col_new[index] <- meta$col_fan[i_name]
      }
    }
    # Next fill colors for other types
    index_NA <- which(is.na(col_new))
    if (length(index_NA)) {
      i_col <- 1
      for (i in seq_along(col_new)) {
        if (is.na(col_new[i])) {
          index_same_name <- which(meta$series_names[i] == meta$series_names) # Check i has corresponding column (should have same color)
          if (any(meta$series_type[index_same_name] %in% SERIES_TYPE_FAN_LINE)) {
            col_new[index_same_name] <- meta$col_fan_line
          } else if (any(meta$series_type[index_same_name] %in% SERIES_TYPE_WHISKER)) {
            col_new[index_same_name] <- meta$col_whisker
          } else if (any(index_same_name %in% meta$mark_index)) {
            col_new[index_same_name] <- meta$col_mark
          } else {
            col_new[index_same_name] <- if (any_fan) meta$col_default_with_fan[i_col] else meta$col_default[i_col]         
            i_col <- 1 + i_col 
          }
        }
      }
    }
    meta$col_default <- col_new
  }
  
  return(meta)
}

pre_process_meta <- function(meta) {
  # Make all variables in meta of proper length, matching number of series
  meta <- one_element_for_each_series(meta)
  meta <- count_series_types(meta)
  meta <- set_class(meta)

  # TODO Fix this ugly hack
  if (is_class_heatmap(meta)) {
    if (is.null(colnames(meta$d0))) colnames(meta$d0) <- 1:ncol(meta$d0)
    if (is.null(rownames(meta$d0))) rownames(meta$d0) <- 1:nrow(meta$d0)
    if (is.na(colnames(meta$d0)[1])) {
      rownames(meta$d0) <- meta$d0[, 1]
    } else {
      meta$d0 <- cbind(rownames(meta$d0), meta$d0)
    }
  }

  # Remove x-axis from data
  meta$d <- df_as_matrix(meta$d0[, -1, drop = FALSE])
  colnames(meta$d) <- colnames(meta$d0)[-1] # HACK To fix issue with duplicate names

  if (is_class_heatmap(meta)) {
    rownames(meta$d) <- meta$d0[, 1]
    meta$x_at  <- 1:ncol(meta$d)
    meta$x_at_lab <- colnames(meta$d)
    meta$y_at  <- 1:nrow(meta$d)
    meta$y_at_lab <- rev(rownames(meta$d))
    meta$x_lim <- c(.5, ncol(meta$d) + .5)
    meta$y_lim <- c(.5, nrow(meta$d) + .5)
    if (!has_value(meta$z_lim)) meta$z_lim <- range(meta$d)

    return(meta)
  }

  # Put x,y-axis (as in data) info in meta
  meta <- extract_x_axis(meta) # set x_at, x_at_lab (can be text), original x-values, and x_lim

  # Now we are ready for y-data
  if (!is_class_ppower(meta))
    meta <- extract_y_axis(meta) # set y_at, and y_lim  
  # Add series names to meta
  meta <- combine_lists(high_prio = meta, low_prio = list(series_names = colnames(meta$d)))
  if (is_class_pie(meta))
    meta <- combine_lists(high_prio = meta, low_prio = list(series_names = meta$x_at_lab))

  # n-series
  if (is_class_pie(meta)) {
    meta$n_series <- nrow(meta$d0)
  } else {
    meta$n_series <- length(unique(meta$series_names))
  }  
  
  # Bar plot pre-calculations
  meta <- extract_barplot_info(meta)

  # Fix colours
  meta <- pre_process_spike_colors(meta)
      
  index_bar <- c(meta$bar_next_index, meta$bar_top_index)
  meta$pch[index_bar]            <- 15
  meta$pch[meta$fan_index]       <- 15
  meta$pch[meta$dot_index]       <- 16
  meta$pch[meta$line_dot_index]  <- 16
  meta$pch[meta$dot_small_index] <- 16
  meta$pch[meta$mark_index]      <- as.numeric(str_sub(meta$series_type[meta$mark_index], 1 + nchar(SERIES_TYPE_MARK)))
  meta$pch[meta$line_index]      <- NA 

  return(meta)
}


count_series_types <- function(meta) {
  # Get index for each of series_type
  meta$line_index        <- which(SERIES_TYPE_LINE      == meta$series_type)
  meta$line_dot_index    <- which(SERIES_TYPE_LINE_DOT  == meta$series_type)
  meta$lines_index       <- which(is.element(meta$series_type, SERIES_TYPE_LINES))
	meta$bar_air_index     <- which(SERIES_TYPE_BAR_AIR   == meta$series_type)
	meta$bar_next_index    <- which(SERIES_TYPE_BAR_NEXT  == meta$series_type)
	meta$bar_top_index     <- which(SERIES_TYPE_BAR_TOP   == meta$series_type)
  meta$whisker_index     <- which(SERIES_TYPE_WHISKER   == meta$series_type)
  meta$dot_index         <- which(SERIES_TYPE_DOT       == meta$series_type)
  meta$dot_small_index   <- which(SERIES_TYPE_DOT_SMALL == meta$series_type)
  meta$mark_index        <- which(is_series_type_mark(meta$series_type))
  meta$fan_index         <- which(SERIES_TYPE_FAN       == meta$series_type)#str_sub(meta$series_type, 1, nchar(SERIES_TYPE_FAN)))
  meta$two_columns_index <- which(is.element(meta$series_type, SERIES_TYPE_TWO_COLUMNS))
  
  meta$n_lines        <- length(meta$lines_index)
  meta$n_bar_air      <- length(meta$bar_air_index)
	meta$n_bar_next     <- length(meta$bar_next_index)
	meta$n_bar_top      <- length(meta$bar_top_index)
  meta$n_whisker      <- length(meta$whisker_index)
	meta$n_barv	        <- meta$n_bar_air / 2 + meta$n_bar_next + if (0 == meta$n_bar_top) 0 else 1
  meta$n_dots         <- length(meta$dot_index)
  meta$n_marks        <- length(meta$mark_index)
  meta$n_dot_small    <- length(meta$dot_small_index)
  meta$n_fan          <- length(meta$fan_index)
  meta$n_two_columns  <- length(meta$two_columns_index)
  return(meta)
}

extract_barplot_info <- function(meta) {
  if (0 < meta$n_barv) { # TODO for barh?
    stopifnot(1 < length(meta$x))
    meta$x_delta    <- diff(meta$x)[1] # TODO Can get problems if user wants bars not equidistant :-O, i.e. if x <> x_at
    meta$bar_width  <- meta$x_delta / meta$n_barv
    meta$bar_left   <- meta$x - meta$x_delta / 2
  
    # Initialize offsets for bars in same column
    meta$bars_offset_negative <- rep(0, length(meta$x))
    meta$bars_offset_positive <- rep(0, length(meta$x))    
  }  
  return(meta)
}

plot_line_dot <- function(i, meta) {
  this_type <- meta$series_type[i]
  
  line_type <- 'l'
  if (is.element(this_type, SERIES_TYPE_DOTS) | is_series_type_mark(this_type)) line_type <- 'p'
  if (SERIES_TYPE_LINE_DOT == this_type) line_type <- 'b'

  this_lty  <- if (is.element(this_type, SERIES_TYPE_LINE_DASH)) 2 else 1

  this_cex <- if (SERIES_TYPE_DOT_SMALL == meta$series_type[i]) meta$small_dot_scaling else 1
  
  if (is.element(i, meta$mark_index)) {
    this_col <- meta$col_mark
    if (0 == meta$pch[i]) { # meaning horizontal line as mark
      meta$pch[i] <- NA
      if (0 < meta$n_barv)
        mark_width <- meta$bar_width * meta$n_barv * (1 - meta$bar_gap_fraction)
      else
        mark_width <- mean(diff(meta$x)) * (1 - meta$bar_gap_fraction)
      for (j in seq_along(meta$x)) {
        lines(meta$x[j] + c(-1, 1) * mark_width / 2, rep(as.numeric(meta$d[j, i]), 2), col = this_col, lwd = meta$lwd_ts[i], cex = this_cex)
      }
    } else {
      lines(meta$x, as.numeric(meta$d[, i]), col = this_col, lwd = meta$lwd_ts[i], lty = this_lty, type = line_type, pch = meta$pch[i], cex = this_cex) # TODO type = 'b' and 'p' for points
    }
  } else {
    this_col <- if (SERIES_TYPE_FAN_LINE == meta$series_type[i]) meta$col_fan_line else meta$col_default[i]
    lines(meta$x, as.numeric(meta$d[, i]), col = this_col, lwd = meta$lwd_ts[i], lty = this_lty, type = line_type, pch = meta$pch[i], cex = this_cex) # TODO type = 'b' and 'p' for points
  } #TODO overlap hieruit halen
}

plot_barh <- function(i, meta) {
  stop("USE meta$swap_xy HERE? OF IS DAT OVERKILL?")
}

get_x_bar_left_right <- function(ith_column, meta) {
  x_bar_middle    <- meta$bar_left + meta$x_delta * (1 - (1 - meta$bar_gap_fraction)) / 2 + meta$x_delta / meta$n_barv * (1/2 + ith_column - 1) * (1 - meta$bar_gap_fraction)
	x_bar_left      <- x_bar_middle - meta$bar_width / 2 * (1 - meta$bar_gap_fraction)
	x_bar_right     <- x_bar_middle + meta$bar_width / 2 * (1 - meta$bar_gap_fraction)
  return(list(x_bar_left = x_bar_left, x_bar_right = x_bar_right))
}

plot_bar_next <- function(i, meta) {
	# BAR--
  index_bar_next <- which(meta$series_type %in% SERIES_TYPE_BARS_NEXT)
  ith_column     <- which(i == index_bar_next) # ith column in figure
  ith_column     <- ith_column - length(which(duplicated(meta$series_names)[1:i] & meta$series_type[1:i] %in% SERIES_TYPE_BARS_NEXT)) # Fans also have duplicated columns
  #ith_column      <- ith_column + if (0 == meta$n_bar_top) 0 else 1

  x_bar_left  <- get_x_bar_left_right(ith_column, meta)$x_bar_left
  x_bar_right <- get_x_bar_left_right(ith_column, meta)$x_bar_right
  if (is.element(i, c(meta$bar_air_index, meta$whisker_index))) { # bar^, whisker
    y_low  <- meta$d[, i]
    y_high <- meta$d[, i+1]
  } else { # bar--
    y_low  <- 0
    y_high <- meta$d[, i]
  }
  if (is.element(i, meta$whisker_index)) { # whisker
    # for (index in seq_along(meta$x_at)) {
    #   arrows(x0 = meta$x_at[index], y0 = y_low[index], x1 = meta$x_at[index], y_high[index], code = 3, col = meta$col_whisker)
    # }
    arrows(x0 = meta$x_at, y0 = y_low, x1 = meta$x_at, y_high, code = 3, col = meta$col_whisker, angle = 90, length = meta$whisker_edge_length)
  } else { # bar
    rect(x_bar_left, y_low, x_bar_right, y_high, col = meta$col_default[i], border = NA)
  }
}

plot_bar_top <- function(i, meta) {
	# BAR=
  # first determine horizontal position of the bar
	x_bar_left   <- get_x_bar_left_right(ith_column = meta$n_barv, meta = meta)$x_bar_left
  x_bar_right  <- get_x_bar_left_right(ith_column = meta$n_barv, meta = meta)$x_bar_right
  
  index_negative <- which(meta$d[, i] < 0)
  index_positive <- which(0 < meta$d[, i])
  
  if (length(index_negative)) {
    y_current                                 <- meta$bars_offset_negative[index_negative]
    y_new                                     <- y_current + meta$d[index_negative, i]
    meta$bars_offset_negative[index_negative] <- y_new
    
    rect(x_bar_left[index_negative], y_current, x_bar_right[index_negative], y_new, col = meta$col_default[i], border = NA)
  }

  if (length(index_positive)) {
    y_current                                 <- meta$bars_offset_positive[index_positive]
    y_new                                     <- y_current + meta$d[index_positive, i]
    meta$bars_offset_positive[index_positive] <- y_new
    
    rect(x_bar_left[index_positive], y_current, x_bar_right[index_positive], y_new, col = meta$col_default[i], border = NA)
  }

  return(meta) # with updated offsets
}

set_margins <- function(meta) {
  if (is_class_ppower(meta))
    layout(cbind(c(1,1,1), c(2,2,2), c(2,2,2), c(3,3,3), c(3,3,3)))
  
  col_bg <- meta$col_bg
  if (!has_value(col_bg)) col_bg <- NA
  
  if (is_class_ppower(meta)) {
    par(bg = col_bg, mai = c(meta$margin_ppower_south, meta$margin_ppower_west, meta$margin_ppower_north, meta$margin_ppower_east))
  } else if (is_class_pie(meta)) {
    # par(mai = c(meta$margin_south, meta$margin_east, meta$margin_north, meta$margin_east)) # margin left = margin right
    par(bg = col_bg, mai = c(meta$margin_south, 0, meta$margin_north, 0)) # margin left = margin right
  } else {
    # y2_axis? => different margins
    par(bg = col_bg, mai = c(meta$margin_south, meta$margin_west, meta$margin_north, if (is_yes(meta$y2)) meta$margin_west else meta$margin_east))
  }
}

add_help_lines <- function(meta) {
  # TODO
  # abline(v = meta$x_at, lwd = meta$lwd_help_lines)
}

add_titles <- function(meta) {
  # title
  mtext(text = restore_sep(meta$title), side = 3, outer = T, adj = 0, at = 0.06, line = -2, font = 2, cex = meta$size_title)
  mtext(text = restore_sep(meta$sub_title), side = 3, outer = T, adj = 0, at = 0.06, line = -3, font = 2, cex = meta$size_title/2)
  
  # x-lab
  mtext(text = restore_sep(meta$x_lab), side = 1, adj = 1, font = 3, line = .9, cex = meta$size_labels)
  
  # y-axis label left
  mtext(text = restore_sep(meta$y_lab), side = 3, outer = T, adj = 0, at = 0.06, line = -4, font = 3, cex = meta$size_labels)
}

#' @keywords internal
add_axis_gridlines_and_userlines <- function(meta) {
  # x-axis
  # x-axis,extra ticks:
  if (has_value(meta$x_ticks)) {
    x_ticks_set <- meta$x_ticks
  } else {
    x_at_ticks_tmp <- meta$x_at
    if (has_value(meta$x_at_lab)) if (length(which(is.na(meta$x_at_lab)))) x_at_ticks_tmp <- x_at_ticks_tmp[-which(is.na(meta$x_at_lab))] # Remove thicks that have no user specified x_at_lab
    x_ticks_set <- unique(x_at_ticks_tmp)
  }
  axis(1, at = x_ticks_set, labels = NA, cex.axis = meta$size_axis_x, lwd = 0, lwd.ticks = meta$x_axis_ticks_lwd, line = meta$v_shift_x_axis, tck = meta$x_axis_ticks_length, xpd = T)
  # x-axis, labels
  user_wants_rotated_labels <- has_value(meta$x_axis_rotate_lab)
  if (user_wants_rotated_labels) user_wants_rotated_labels <- 0 != meta$x_axis_rotate_lab
  if (!has_value(meta$x_at_lab)) meta$x_at_lab <- fix_numbers(meta$x_at, meta$x_n_decimals, meta$decimal_sep)
  if (!user_wants_rotated_labels) {
    text(x = meta$x_at, par("usr")[3] + (par("usr")[4] - par("usr")[3]) * meta$v_shift_x_axis_label_fraction, labels = meta$x_at_lab, pos = 1, xpd = TRUE, cex = meta$size_axis_x)
  } else {
    text(x = meta$x_at, par("usr")[3] + (par("usr")[4] - par("usr")[3]) * meta$v_shift_x_axis_label_fraction, labels = meta$x_at_lab, srt = meta$x_axis_rotate_lab, adj = 1, xpd = TRUE, cex = meta$size_axis_x)
  }
  
  # y-axis
  if (!has_value(meta$y_at_lab)) {
    meta$y_at_lab <- fix_numbers(meta$y_at, meta$y_n_decimals, meta$decimal_sep)
  }
  axis(2, at = meta$y_at, labels = meta$y_at_lab, las = 2, lwd = 0, lwd.ticks = 0, xpd = TRUE, cex.axis = meta$size_axis_y)
  
  # y2_axis + label
  if (is_yes(meta$y2)) {
    axis(4, at = meta$y_at, labels = meta$y2_at_lab, las = 2, lwd = 0, lwd.ticks = 0, xpd = TRUE, cex.axis = meta$size_axis_y)
    mtext(text = restore_sep(meta$y2_lab), side = 3, outer = T, adj = 1, at = 0.94, line = -4, font = 3, cex = meta$size_labels)
  }
  
  if (is_class_heatmap(meta)) {
    # Small ticks, like on x-axis
    axis(2, at = meta$y_at, labels = NA, cex.axis = meta$size_axis_x, lwd = 0, lwd.ticks = meta$x_axis_ticks_lwd, line = meta$v_shift_x_axis, tck = meta$x_axis_ticks_length, xpd = T)
  } else {
    # Grid lines
    abline(h = meta$y_at, lwd = meta$lwd_grid_lines, col = meta$col_grid_lines)    
  }
}

add_lines_user <- function(meta) {
  abline(v = meta$vline_bold, lwd = meta$lwd_hline_bold, col = "#000000")          # bold
  abline(h = meta$hline_dash, v = meta$vline_dash, lwd = meta$lwd_hline_dash, col = "#000000", lty = 2) # dash
  abline(v = meta$vline_grid, lwd = meta$lwd_grid_lines, col = meta$col_grid_lines)                     # grid vertical
}

#' @keywords internal
add_legend <- function(meta) {
  # Return if nothing to do
  if (all(is.na(meta$series_names)) && !is_class_heatmap(meta)) return()

  # Set margins
  opar <- par()
  on.exit(suppressWarnings(par(opar)))
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0:1, 0:1, type = "n", bty = "n", xaxt = "n", yaxt = "n")

  if (is_class_heatmap(meta)) {
    # meta$n_series <- 1
    # meta$lwd_ts <- c(0, 0, 0)
    # meta$series_names <- c(get_param("z_lab", meta, ""), meta$z_lim[1], meta$z_lim[2])
    # meta$pch <- c(NA, 15, 15)
    # meta$col_default <- c(NA, meta$col_heatmap)
    
    # Write z_lab label
    text(.03 + meta$legend_x, -.045 + meta$legend_y, get_param("z_lab", meta, ""), font = 3, pos = 4)
    # Gradient
    n_col <- 100
    gradient <- colorRampPalette(meta$col_heatmap)(n_col)
    gradient_width <- .5
    x_left <- .05 + meta$legend_x
    dx = .01
    y  = .008
    dy = .04
    for (i in 1:n_col) {
      x <- x_left + gradient_width * i / n_col
      rect(x, y, x + dx, y + dy, col = gradient[i], border = NA)
    }

    # vertical lines
    # lines(c(x_left, x_left) + gradient_width * 1 / n_col, c(y + dy, y - dy), lwd = meta$x_axis_ticks_lwd, col = "grey")
    # lines(c(x_left, x_left) + gradient_width + dx, c(y + dy, y - dy), lwd = meta$x_axis_ticks_lwd, col = "grey")
    
    # Min/max
    y_z_lim <- y + dy / 2.3
    text(x_left, y_z_lim, meta$z_lim[1], font = 3, pos = 4)
    text(x_left + gradient_width + dx, y_z_lim, meta$z_lim[2], font = 3, pos = 2)
    
    meta$n_series <- 1
    meta$lwd_ts <- c(0, 0, 0)
    meta$series_names <- c(get_param("z_lab", meta, ""), meta$z_lim[1], meta$z_lim[2])
    meta$pch <- c(NA, 15, 15)
    meta$col_default <- c(NA, meta$col_heatmap)
    return()
  }

  # Set series specific legend
  meta <- set_series_specific_legend(meta)

  # Find optimal number of columns if not specified
  legend_n_columns <- meta$legend_n_columns
  if (!has_value(legend_n_columns)) {
    legend_n_columns <- if (meta$n_series < 3) 1 else 2
    if (6 < meta$n_series)
      legend_n_columns <- 3
  }
    
  # Fix distance between columns
  series_names <- meta$series_names
  nseries <- length(series_names)
  if (1 < legend_n_columns) {
    series_names[!is.na(series_names)] <- paste(series_names[!is.na(series_names)], paste0(rep(" ", meta$legend_distance_columns), collapse = ""))
  }
  
  if (has_value(meta$legend_order)) {
    index                         <- meta$legend_order
    series_names                  <- series_names[index]
    meta$lwd_ts                   <- meta$lwd_ts[index]
    meta$col_default              <- meta$col_default[index]
    meta$pch                      <- meta$pch[index]
    meta$legend_symbol_size       <- meta$legend_symbol_size[index]     
  }

  # Do the magic
  legend(meta$legend_x, meta$legend_y, legend = series_names, text.font = 3, lwd = meta$lwd_ts, col = meta$col_default, pch = meta$pch, pt.cex = meta$legend_symbol_size, bty = "n", x.intersp = meta$legend_space_symbol_text, seg.len = meta$legend_line_length, ncol = legend_n_columns)
}

#' Meta may have variables of length 1 or length > meta$n_series. This function makes all lists have length meta$n_series.
#' @keywords internal
one_element_for_each_series <- function(meta) {
  n_columns <- ncol(meta$d0) - 1
  if (0 == length(meta$series_type)) meta$series_type <- rep(SERIES_TYPE_LINE, n_columns) # Fix series_type
  if (1 == length(meta$series_type)) meta$series_type <- rep(meta$series_type, n_columns) # Fix series_type
#  meta$col_default <- meta$col_default[1:n_columns] # Fix col
  if (1 == length(meta$lwd_ts)) meta$lwd_ts <- rep(meta$lwd_ts, n_columns) #rep(meta$lwd_ts, n_columns) # Fix lwd
  if (1 == length(meta$legend_symbol_size)) meta$legend_symbol_size <- rep(meta$legend_symbol_size, n_columns)
  if (1 == length(meta$legend_mark_size)) meta$legend_mark_size <- rep(meta$legend_mark_size, n_columns)
    
  return(meta)
}

#' @keywords internal
set_series_specific_legend <- function(meta) {
  index_bar <- c(meta$bar_next_index, meta$bar_top_index)

  meta$legend_symbol_size[c(meta$dot_small_index, meta$dot_index, meta$line_dot_index)] <- meta$legend_symbol_size[c(meta$dot_small_index, meta$dot_index, meta$line_dot_index)] * meta$legend_scale_dots

  meta$lwd_ts[c(index_bar, meta$dot_index, meta$dot_small_index)] <- NA # Block lines for bar plots

  # Mark specifics
  if (meta$n_marks) for (i in meta$mark_index) {
    if (0 == meta$pch[i]) { # line
      meta$pch[i] <- NA
    } else {
      meta$lwd_ts[i] <- NA
      meta$legend_symbol_size[i] <- meta$legend_mark_size[i]
    }
  }

  # Fan specifics
  if (0 < meta$n_fan) { # Fix for fans
    meta$lwd_ts[meta$fan_index] <- NA
    meta$pch[meta$fan_index]    <- 15
    for (i in seq_along(meta$fan_index)) {
      meta$col_default[meta$fan_index[i]] <- meta$col_fan[trunc((1 + i) / 2)]
    }
  }
  index_fan_line <- which(SERIES_TYPE_FAN_LINE == meta$series_type)
  if (length(index_fan_line)) meta$col_default[index_fan_line] <- meta$col_fan_line
  
  # Bar^ specifics
  if (meta$n_bar_air) {
    meta$pch[meta$bar_air_index] <- 15
  }
  
  # Whisker specifics
  if (meta$n_whisker) {
    meta$pch[meta$whisker_index] <- NA
    meta$col_default[meta$whisker_index] <- meta$col_whisker
  }
  
  # Remove 'fan-' and 'bar^ duplicates'
  index_duplicated  <- which(duplicated(meta$series_names)) # has values for sure
  if (length(index_duplicated)) {
    meta$series_names       <- meta$series_names[-index_duplicated]
    meta$lwd_ts             <- meta$lwd_ts[-index_duplicated]
    meta$col_default             <- meta$col_default[-index_duplicated]
    meta$pch                <- meta$pch[-index_duplicated]
    meta$legend_symbol_size <- meta$legend_symbol_size[-index_duplicated]
  }
  
  # Pie specifics
  if (is_class_pie(meta)) {
    meta$pch <- rep(15, length(meta$series_names))
    index_NA <- which(is.na(meta$series_names))
    if (length(index_NA)) {
      meta$lwd_ts[index_NA] <- NA
      meta$pch[index_NA]    <- NA
    }
  }

  return(meta)
}

#' Extract x-axis information from d
#' @param d data
#' @keywords internal
extract_x_axis <- function(meta) {
  # Extract x_at
  if (is.null(meta[["x_at"]])) {
    if (!is.null(meta$x_lim)) { # Get x_at from x_lim
      meta$x_at <- pretty(meta$x_lim)
    } else {
      if (is_really_character(meta$d0[, 1])) { # If characters present
        meta$x_at <- 1:nrow(meta$d)
      } else { # If just numeric data
        meta$x_at <- pretty(as.numeric(meta$d0[, 1]))
      }
    }      
  }

  # Extract x_at_lab
  if (is.null(meta$x_at_lab) & is_really_character(meta$d0[, 1])) {
    meta$x_at_lab <- meta$d0[, 1]
    # TODO Replace dot by comma, etc...
  }
  meta$x_at_lab <- str_replace_all(meta$x_at_lab, "\\\\n", "\n")

  # Extract x_lim
  if (is.null(meta$x_lim)) {
    meta$x_lim <- range(meta$x_at)
    if (0 < meta$n_barv) {
      meta$x_lim <- meta$x_lim + c(-1, 1) * 0.1 * diff(range(meta$x_lim))
    }
  }
  
  # Set original x-values
  if (is.null(meta$x)) {
    # if (is_really_character(meta$d0[, 1])) meta$x <- meta$d0[, 1] else meta$x <- as.numeric(meta$d0[, 1])
    if (is_really_character(meta$d0[, 1])) meta$x <- meta$x_at else meta$x <- as.numeric(meta$d0[, 1])
  }
  
  return(meta)
}

hide_y2 <- function(x, meta) {
  if (!is_yes(meta$y2))
    return(x)
  else if (is.matrix(x))
    return(x[, -ncol(x), drop = FALSE])
  else
    return(head(x, -1))
}

#' Extract pretty y-axis information from d
#' @param d data
#' @keywords internal
extract_y_axis <- function(meta) {
  # NB Beware of stacked bars!
  # Determine y_min and y_max first
  y_min <- min(hide_y2(meta$d, meta), na.rm = T)
  y_max <- max(hide_y2(meta$d, meta), na.rm = T)
  if (is_yes(meta$y2) && is.element(ncol(meta$d), meta$bar_top_index)) {
    # IF y2 is "bar=", then temporarily replace that by bar--, in this function, for ease of computation
    stop("Please don't use bar= (but bar--) for last time series if you want second y-axis!")
  }
  if (0 < meta$n_bar_top) {
    vec_neg <- NULL
    vec_pos <- NULL
    for (index_x in 1:nrow(meta$d)) {
      index_neg <- which(meta$d[index_x, meta$bar_top_index] <= 0)
      index_pos <- which(0 <= meta$d[index_x, meta$bar_top_index])
      vec_neg[index_x] <- sum(meta$d[index_x, meta$bar_top_index[index_neg]])
      vec_pos[index_x] <- sum(meta$d[index_x, meta$bar_top_index[index_pos]])
    }
    y_min <- min(y_min, vec_neg)
    y_max <- max(y_max, vec_pos)
  }
  
  y_lim_auto <- c(y_min, y_max)
  # Include hline_bold in y_lim
  if (!is.null(meta$hline_bold))
    y_lim_auto <- range(c(meta$hline_bold, y_lim_auto))
  # TODO If user specifies hline_dash, then include this in y_lim too!

  # Overwrite y_lim if manually set
  if (!is.null(meta$y_lim))
    y_lim_auto <- meta$y_lim
  
  # Extract y_at
  if (is.null(meta[["y_at"]])) {
    y_at <- pretty(y_lim_auto)
    if (max(y_at) < y_max) # highest grid line always above data
      y_at <- c(y_at, tail(y_at, 1) + diff(y_at)[1])
    if (y_min < min(y_at)) # lowest grid line always below data
      y_at <- c(y_at[1] - diff(y_at)[1], y_at)
    meta$y_at <- y_at
  }
  
  # Extract y_lim
  if (is.null(meta$y_lim)) {
    meta$y_lim <- range(meta$y_at)
  } else {
    # Make y_lim fit y_at
    if (meta$y_lim[2] < max(meta$y_at)) meta$y_lim[2] <- max(meta$y_at)
    if (min(meta$y_at) < meta$y_lim[1]) meta$y_lim[1] <- min(meta$y_at)
  }

  # Extract y2 (right y-axis)
  if (is_yes(meta$y2)) {
    meta <- extract_y2_axis(meta)
  }

  return(meta)
}

extract_y2_axis <- function(meta) {
  n_col    <- ncol(meta$d)
  if (!has_value(meta$y2_at)) {
    # James sets values
    n_labels <- length(meta$y_at)
    y2_range <- y2_range_original <- range(meta$d[,  n_col], na.rm = T)
    y2_delta <- diff(y2_range)

    valid_y2_axis <- function(y2_labels) {
      if (n_labels != length(y2_labels)) { # Check length
        return(FALSE)
      } else { # Same length! Check labels surround graph
        return(y2_labels[1] <= y2_range_original[1] && y2_range_original[2] <= tail(y2_labels, 1))
      }
    }

    done <- FALSE
    while (!done) {
      meta$y2_at <- pretty(y2_range, n = n_labels)
      y2_labels_delta <- meta$y2_at[2] - meta$y2_at[1]
      while (y2_range[1] < meta$y2_at[1]) meta$y2_at <- c(meta$y2_at[1] - y2_labels_delta, meta$y2_at)
      while (tail(meta$y2_at, 1) < y2_range[2]) meta$y2_at <- c(meta$y2_at, tail(meta$y2_at, 1) + y2_labels_delta)
      done <- valid_y2_axis(meta$y2_at)

      if (!done) {
        # Change scaling for new try
        y2_range[1] <- y2_range[1] - y2_delta * 1e-3
        y2_range[2] <- y2_range[2] + y2_delta * 1e-3
      }
    }
  }
  
  # Scale data in last column so it be plotted like da rest
  y1_range  <- c(tail(meta$y_at, 1), meta$y_at[1])
  y2_range  <- c(tail(meta$y2_at, 1), meta$y2_at[1])
  y2_alpha  <- diff(y1_range) / diff(y2_range)
  y2_beta   <- y1_range[1] - y2_alpha * y2_range[1]
  meta$d[, n_col] <- y2_alpha * meta$d[, n_col] + y2_beta

  # Use auto generated labels if user did not specify
  if (is.null(meta$y2_at_lab))
    meta$y2_at_lab <- fix_numbers(meta$y2_at, meta$y_n_decimals, meta$decimal_sep)

  return(meta)
}






























