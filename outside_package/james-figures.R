# index = 8 # 2a?
# object$fig$specs$series_type <- c(FIG_TYPE$char, FIG_TYPE$char, FIG_TYPE$bar_next)

is_really_character <- function(vec) {
  if ("character" == class(vec)) {
    return("character" == class(type.convert(vec, as.is = TRUE)))
  } else return(FALSE)
}

figure <- function(index, tile = FALSE) {
  object <<- j_get(index = index, what = "object")
  x      <- object$data
  f      <- object$fig$specs
  n_col  <- ncol(x) - !is.ts(x)
  
  #####################################
  ## First get color scheme
  #####################################
  color_scheme <- get_settings()$ts_col
  if (field_has_value(f$col_scheme)) {
    if (COL_SCHEME$dot_median == f$col_scheme)
      color_scheme <- get_settings()$col_dot_median
    if (COL_SCHEME$fan == f$col_scheme)
      color_scheme <- get_settings()$col_fan
  } 
  
  #####################################
  ## Get x-axis
  #####################################
  x_col1_is_char <- is_really_character(x[, 1])
  x_axis_manual_char_labels <- (field_has_value(f$x_labels) && is_really_character(f$x_labels))
  if (is.ts(x)) {
    # Clearly time series that has x-axis
    t_vec <- as.vector(time(x))
    x_at <- if (field_has_value(f$x_at)) as_numeric_vec(f$x_at) else pretty(t_vec)
    t_vec_labels <- if (field_has_value(f$x_labels)) as_char_vec(f$x_labels) else x_at
    t_vec_labels <- str_replace_all(t_vec_labels, "\\\\n", "\n")
  } else {
    # Assume matrix
    if (x_col1_is_char) {
      t_vec <- 1:nrow(x)
      x_at  <- t_vec
      t_vec_labels <- as_char_vec(str_replace_all(x[, 1], "\\\\n", "\n"))
    } else {
      t_vec <- x[, 1]
      x_at <- if (field_has_value(f$x_at)) as_numeric_vec(f$x_at) else pretty(t_vec)
      t_vec_labels <- if (field_has_value(f$x_labels)) as_char_vec(str_replace_all(f$x_labels, "\\\\n", "\n")) else x_at
    }
        
    # Remove x-axis from matrix
    x <- x[, -1, drop = FALSE]
  }
  
  if (x_col1_is_char) {
    # Replace dot by decimal separator
    t_vec_labels <- chartr(".", get_settings()$decimal_sep, as.character(t_vec_labels))    
  }

  # 3. Fix if x_at, t_vec_labels have different lengts
  stopifnot(length(x_at) == length(t_vec_labels))

  #####################################
  # Now continue
  #####################################

  # Use series_type from Excel or set defaults
  series_type <- f$series_type
  if (field_has_value(series_type)) {
    series_type <- as_char_vec(series_type)
  } else {
    series_type <- rep(NA, n_col)
  }

  # Guess default variable type
  index_NA <- which(is.na(series_type))
  if (0 < length(index_NA)) {
    # if (x_axis_is_char) {
    #   # If chars on x-axis, then default is bar
    #   series_type[index_NA] <- FIG_TYPE$bar_next
    # } else {
      # Otherwise default is line
      series_type[index_NA] <- FIG_TYPE$line
    # }
  }

  # Get index for each of FIG_TYPE
  line_index      <- which(FIG_TYPE$line      == series_type)
  line_dot_index  <- which(FIG_TYPE$line_dot  == series_type)
	bar_next_index  <- which(FIG_TYPE$bar_next  == series_type)
	bar_top_index   <- which(FIG_TYPE$bar_top   == series_type)
  dot_index       <- which(FIG_TYPE$dot       == series_type)
  dot_small_index <- which(FIG_TYPE$dot_small == series_type)
  fan_index       <- which(FIG_TYPE$fan       == series_type)
    
  # How much of each type?
  n_lines        <- length(line_index) + length(line_dot_index)
	n_bar_next     <- length(bar_next_index)
	n_bar_top      <- length(bar_top_index)
	n_bars		     <- n_bar_next + if (0 == n_bar_top) 0 else 1
  n_dots         <- length(dot_index)
  n_dot_small    <- length(dot_small_index)
  n_fan          <- length(fan_index)

  # Get x,y-dimensions # Please note, these values may be overwritten by manual values below(!)  
	t_delta	<- mean(diff(t_vec))
  if (field_has_value(f$x_lim)) {
    x_lim <- as_numeric_vec(f$x_lim)
  } else {
    x_lim <- range(t_vec) + if (0 < n_bars) c(-1, 1) * 0.1 * diff(range(t_vec)) else c(0, 0) 
  }

  # Determine y_lim (NB bars on top)
  y2_axis <- field_has_value(f$y2) && is.element(f$y2, Y2_OPTIONS)
  if (field_has_value(f$y_lim)) {
    y_lim <- as_numeric_vec(f$y_lim)
  } else {
    x_stack <- x
    if (y2_axis) x_stack <- x_stack[, -n_col, drop = FALSE]
    x_as_vector <- as.numeric(unlist(x_stack))#as.vector(as.numeric(x_stack))
    
    if (length(bar_top_index)) {
      stacked_min <- stacked_max <- 0
      for (i in 1:nrow(x)) {
        vec <- as.numeric(x[i, bar_top_index])
        stacked_min <- min(stacked_min, sum(vec[which(vec < 0)]))
        stacked_max <- max(stacked_max, sum(vec[which(0 < vec)]))
      }
      x_as_vector <- c(stacked_min, stacked_max, x_as_vector)
    }

    # Include reference lines in y_lim if specified
    y_ref <- f$hline_reference
    if (!field_has_value(y_ref)) y_ref <- NA
    if (!all(is.na(y_ref))) x_as_vector <- c(as_numeric_vec(y_ref), x_as_vector)
    
    y_lim <- range(x_as_vector, na.rm = T)    
    y_lim_withouth_y_ref <- c(min(x_stack[, 1], na.rm = T), y_lim[2])
  }  
  
  # For Barplot with text on x-axis, include 0 in y_lim
  if (x_col1_is_char) y_lim <- range(0, y_lim)
  
  y_lim <- range(y_lim, pretty(y_lim))
  # Add extra space if data exceeds pretty labels?
  y_lim <- range(y_lim, pretty(y_lim))

  if (field_has_value(f$y_at)) {
    y_lim[1] <- head(as_numeric_vec(f$y_at), 1)
    y_lim[2] <- tail(as_numeric_vec(f$y_at), 1)
  }

  # #
  # ## Correct y-lim as we may want empty space above and below lines in graph
  # #
  # f_above   <- get_settings()$y_axis_empty_above # factor, e.g. 1/6
  # f_below   <- get_settings()$y_axis_empty_below # factor, e.g. 1/6
  # delta     <- diff(y_lim) / (1 - f_above - f_below)
  # # Please note, these values may be overwritten by manual values below(!):
  # y_lim[1]  <- y_lim[1] - f_below * delta
  # y_lim[2]  <- f_above * delta + y_lim[2]
  

  # Add extra space if data exceeds pretty labels?
  # print(y_lim)
  # pretty_y_labels <- pretty(y_lim)
  # print(pretty_y_labels)
  # if (min(pretty_y_labels) < y_lim[1]) {
  #   y_delta <- diff(pretty_y_labels)[1]
  #   y_lim[1] <- pretty_y_labels[1] - y_delta
  #   print(y_lim)
  # }
    
  #
  ## INITIALIZATION
  #
  if (tile) {
    par(bg = get_settings()$bg_col, mai = c(0, 0, 0, 0))
  } else {
    this_mai <- par()$mai
    if (y2_axis) this_mai[4] <- this_mai[1]
    par(bg = get_settings()$bg_col, mai = this_mai)
  }
  plot(NA, axes = FALSE, ann = FALSE, xlim = x_lim, ylim = y_lim)
  
  # RAMING (1)
  if (!is.null(f$future)) {
    rect(f$future, par("usr")[3], par("usr")[2], par("usr")[4], col = get_settings()$raming_rect_col, border = NA) #f$future, par("usr")[3], par("usr")[2], y_lim[2]
  }
  
  # RECTANGLES (1): filling
  rect(f$rect_xleft, f$rect_ybottom, f$rect_xright, f$rect_ytop, col = get_settings()$usr_rect_fill_col, border = NA)
 
  # AXIS AND TITLES
	if (!tile) {
    # X-axis
    if (x_col1_is_char) {
      cex_axis <- get_settings()$x_axis_chars_pt
      # axis(1, at = x_at, labels = NA, cex.axis = cex_axis, col = get_settings()$axis_col, col.axis = get_settings()$fg_txt_col, lwd = 0, lwd.ticks = get_settings()$x_axis_ticks_lwd, padj = get_settings()$x_axis_vertical_gap, tck = -0.015, xpd = T)
      # text(x_at, par("usr")[3] - .1, labels = t_vec_labels, xpd = TRUE, pos = 1, col = get_settings()$fg_txt_col, cex = cex_axis)
    } else {
      cex_axis <- get_settings()$x_axis_numbers_pt
    }
    
    if (x_col1_is_char) {
      axis(1, at = x_at, labels = t_vec_labels, cex.axis = cex_axis, col = get_settings()$axis_col, col.axis = get_settings()$fg_txt_col, lwd = 0, lwd.ticks = get_settings()$x_axis_ticks_lwd, padj = -.5, tck = -0.015, xpd = T)
    } else {
      axis(1, at = x_at, labels = t_vec_labels, cex.axis = cex_axis, col = get_settings()$axis_col, col.axis = get_settings()$fg_txt_col, lwd = 0, lwd.ticks = get_settings()$x_axis_ticks_lwd, padj = get_settings()$x_axis_vertical_gap, tck = -0.015, xpd = T)
    }
    

    # X-axis ticks    
    if (field_has_value(f$x_ticks)) {
      axis(1, at = as_numeric_vec(f$x_ticks), labels = NA, cex.axis = cex_axis, col = get_settings()$axis_col, col.axis = get_settings()$fg_txt_col, lwd = 0, lwd.ticks = get_settings()$x_axis_ticks_lwd, padj = get_settings()$x_axis_vertical_gap, tck = -0.015, xpd = T)      
    }

    ################
    ################ Y-axes
    ################
    
    #
    ## Fix labels on y-axis
    #
    
    vec_to_labels <- function(vec) {
      y_labels_left <- as.character(vec)
      # 1. Find number of digits R wants
      n_digits_vec <- nchar(unlist(lapply(stringr::str_split(y_labels_left, "\\."), function(vec) vec[2])))
      if (!all(is.na(n_digits_vec))) {
        y_labels_left <- format(as.numeric(vec), nsmall = max(n_digits_vec, na.rm = T))
      }
      y_labels_left <- chartr(".", get_settings()$decimal_sep, y_labels_left)
    }
    
    if (y2_axis && field_has_value(f$y_at)) {
      y_axis_at <- as_numeric_vec(f$y_at)
    } else {
      y_axis_at <- pretty(y_lim)
    }
    y_labels_left <- vec_to_labels(y_axis_at)
    
    # Plot y-labels
    # axes left
    axis(2, at = y_axis_at, labels = y_labels_left, las = 2, col = get_settings()$axis_col, col.axis = get_settings()$fg_txt_col, lwd = 0)
    
    # Now y-labels right
    # Second y-axis
    if (y2_axis) {
      if (y2_axis && field_has_value(f$y2_at)) {
        y2_labels <- as_numeric_vec(f$y2_at)
      } else {
        n_labels <- length(y_axis_at)
        y2_range <- y2_range_original <- range(x[,  n_col], na.rm = T)
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
          y2_labels <- pretty(y2_range, n = n_labels)
          y2_labels_delta <- y2_labels[2] - y2_labels[1]
          while (y2_range[1] < y2_labels[1]) y2_labels <- c(y2_labels[1] - y2_labels_delta, y2_labels)
          while (tail(y2_labels, 1) < y2_range[2]) y2_labels <- c(y2_labels, tail(y2_labels, 1) + y2_labels_delta)
          done <- valid_y2_axis(y2_labels)

          if (!done) {
            # Change scaling for new try
            y2_range[1] <- y2_range[1] - y2_delta * 1e-3
            y2_range[2] <- y2_range[2] + y2_delta * 1e-3
          }
        }        
      }
      
      y1_range  <- c(tail(y_axis_at, 1), y_axis_at[1])
      y2_range  <- c(tail(y2_labels, 1), y2_labels[1])
      y2_alpha  <- diff(y1_range) / diff(y2_range)
      y2_beta   <- y1_range[1] - y2_alpha * y2_range[1]
      
      # y2_labels <- pretty((y_lim_withouth_y_ref - y2_beta) / y2_alpha)
      axis(4, at = y_axis_at, labels = y2_labels, las = 2, col = get_settings()$axis_col, col.axis = get_settings()$fg_txt_col, lwd = 0)
      
      # Scale data in last column so it be plotted like da rest
      x[, n_col] <- y2_alpha * x[, n_col] + y2_beta
    }    
    
    # Horizontal lines on y-axis
    abline(h = y_axis_at, col = get_settings()$axis_col, lwd = get_settings()$y_axis_hline_lwd)
    # Title, x,y-lab
    # Main title
    #mtext(text = f$main, side = 3, adj = 0, font = 2, line = 2, col = get_settings()$fg_txt_col, cex = get_settings()$title_pt) # adj = -.1 get_settings()$title_shift_left
    mtext(text = f$title, side = 3, outer = T, adj = 0, at = 0.06, line = -2, font = 2, col = get_settings()$fg_txt_col, cex = get_settings()$title_pt) # adj = -.1 get_settings()$title_shift_left
    # Y-lab
    mtext(text = f$ylab, side = 3, outer = T, adj = 0, at = 0.06, line = -4, font = 3, col = get_settings()$fg_txt_col, cex = get_settings()$x_axis_numbers_pt) # adj = -.09 get_settings()$title_shift_left
    # Y-lab2
    if (y2_axis && field_has_value(f$y2_lab)) {
      mtext(text = f$y2_lab, side = 3, outer = T, adj = 1, at = 0.94, line = -4, font = 3, col = get_settings()$fg_txt_col, cex = get_settings()$x_axis_numbers_pt) # color_scheme[n_col] adj = -.09 get_settings()$title_shift_left
    }
    # X-lab
    # title(xlab = f$xlab, font = 3, col.lab = get_settings()$fg_txt_col, cex.axis = get_settings()$x_axis_numbers_pt)
    mtext(text = f$xlab, side = 1, adj = 1, font = 3, line = 1.2, col.lab = get_settings()$fg_txt_col, cex.axis = get_settings()$x_axis_numbers_pt)
#    mtext(text = "what?", side = 1, adj = 1, font = 3, line = 1, col.lab = get_settings()$fg_txt_col, cex.axis = get_settings()$x_axis_numbers_pt)
    # LEGEND
    add_legend(x, line = c(line_index), line_dot = line_dot_index, bar = c(bar_next_index, bar_top_index), dot = dot_index, fan = fan_index, color_scheme = color_scheme)
  }
    
  # REFERENCE LINE
  y_ref <- f$hline_reference
  #if (!field_has_value(y_ref)) y_ref <- 0 # The default
  if (field_has_value(y_ref)) {
    abline(h = as_numeric_vec(y_ref), lty = get_settings()$hline_reference_lty, lwd = get_settings()$hline_reference_lwd, col = get_settings()$hline_reference_col)
  }
 
  #
  ## FAN CHART
  #
  if (length(n_fan)) {
    if (0 < n_fan) for (i in 2 * (1:(n_fan %/% 2) - 1) + 1) {
      fan_index_av <- which(!is.na(x[, fan_index[i]]))

      t_vec_fan <- as.vector(time(object$data))[fan_index_av]    
      y_low     <- x[fan_index_av, fan_index[i]]
      y_high    <- x[fan_index_av, fan_index[i + 1]]
    
      polygon(x = c(t_vec_fan, rev(t_vec_fan)), y = c(y_low, rev(y_high)), col = color_scheme[fan_index[i]], border = NA)      
    }
  }
 
  #
  ## BARS
  #
  bar_fill_horizontal <- get_settings()$bar_fill_horizontal

	# BAR--
  left <- t_vec - t_delta / 2
	x_bar_width <- t_delta / n_bars
	if (0 < n_bar_next) for (i in 1:n_bar_next) {
    # x {left, right} of bars
    x_bar_middle <- left + t_delta * (1 - bar_fill_horizontal) / 2 + t_delta / n_bars * (1/2 + i - 1) * bar_fill_horizontal
		x_bar_left   <- x_bar_middle - x_bar_width / 2 * bar_fill_horizontal
		x_bar_right  <- x_bar_middle + x_bar_width / 2 * bar_fill_horizontal

    # y {bottom, top} of bars
    y_bottom     <- rep(0, length(t_vec))
    y_top        <- x[, bar_next_index[i]]
    rect(x_bar_left, y_bottom, x_bar_right, y_top, col = color_scheme[bar_next_index[i]], border = NA)
	}
	
	# BAR=
  if (0 < n_bar_top) {
    # first determine horizontal position of the par
    x_bar_middle <- left + t_delta * (1 - bar_fill_horizontal) / 2 + t_delta / n_bars * (1/2 + n_bars - 1) * bar_fill_horizontal
  	x_bar_left   <- x_bar_middle - x_bar_width / 2 * bar_fill_horizontal
    x_bar_right  <- x_bar_middle + x_bar_width / 2 * bar_fill_horizontal
    
    for (i in 1:nrow(x)) {
      # for each row (say a given time point)

      # first pick negative
      y_cummulative <- 0
      index_negative <- bar_top_index[which(x[i, bar_top_index] < 0)]
      for (j in seq_along(index_negative)) {
        j_index <- index_negative[j]
        y_cummulative_new <- y_cummulative + x[i, j_index]
        rect(x_bar_left[i], y_cummulative, x_bar_right[i], y_cummulative_new, col = color_scheme[j_index], border = NA)
        y_cummulative <- y_cummulative_new
      }
      
      # now pick positive
      y_cummulative <- 0
      index_positive <- bar_top_index[which(0 < x[i, bar_top_index])]
      for (j in seq_along(index_positive)) {
        j_index <- index_positive[j]
        y_cummulative_new <- y_cummulative + x[i, j_index]
        rect(x_bar_left[i], y_cummulative, x_bar_right[i], y_cummulative_new, col = color_scheme[j_index], border = NA)
        y_cummulative <- y_cummulative_new
      }
    }
  }
  
	# DOT_SMALL  
  for (i in seq_along(dot_small_index)) {
    this_index <- dot_small_index[i]
    points(t_vec, as.numeric(x[, this_index]), col = color_scheme[this_index], pch = 20, cex = .05)
  }
  
	# LINE
  # curves = list()
  index_line_all <- sort(c(line_index, line_dot_index))
  for (i in seq_along(index_line_all)) {
    this_index <- index_line_all[i]
    y <- as.numeric(x[, this_index])
    # type_l <- is.element(this_index, line_index)
    lines(t_vec, y, col = color_scheme[this_index], lwd = get_settings()$ts_lwd, type = "l")#if (type_l) "l" else "b")
    # curves[[1 + length(curves)]] = list(x = t_vec, y = y)
  }

  # HLINE_DASHED
  abline(h = f$hline_dashed, lty = 2, lwd = get_settings()$hline_dotted_lwd, col = get_settings()$hline_dotted_col)

	# DOT
  index_dot_all <- sort(c(dot_index, line_dot_index))
  for (i in seq_along(index_dot_all)) {
    this_index <- index_dot_all[i]
    points(t_vec, as.numeric(x[, this_index]), col = color_scheme[this_index], pch = 19)
  }
  
  # RECTANGLES (2): border
  rect(f$rect_xleft, f$rect_ybottom, f$rect_xright, f$rect_ytop, col = NA, border = get_settings()$usr_rect_border_col, lwd = get_settings()$usr_rect_border_lwd)
  
  # # RAMING (2): place text "Raming" above gray rect + vertical line
  # if (!is.null(f$future)) {
  #   abline(v = f$future, lty = 2)
  #   y_raming <- par("usr")[4] - (par("usr")[4] - y_lim[2]) / 2
  #   text(f$future, y_raming, get_settings()$raming_text, pos = 4, cex = .8)
  # }
  
  # Add labels for y2-axis
  # if (y2_axis) {
  #   # Determine first and last NA index
  #   text(x = t_vec[index_left],  y = x[index_left,  n_col],  labels = round(y2_left), pos = 2, col = color_scheme[n_col])
  #   text(x = t_vec[index_rigth], y = x[index_rigth, n_col],  labels = round(y2_right), pos = 2, col = color_scheme[n_col])
  # }
  
  # Add text labels
  if (field_has_value(f$txt_lab) && field_has_value(f$txt_lab_xy_pos)) {
    txt_lab_sep <- as_char_vec(f$txt_lab, ";")
    txt_lab_xy_pos_sep <- as_char_vec(f$txt_lab_xy_pos, ";")
    for (i in 1:length(txt_lab_sep)) {
      txt <- txt_lab_sep[i]
      xyp  <- txt_lab_xy_pos_sep[i]
      xy_pos <- as_numeric_vec(xyp)
      text(xy_pos[1], xy_pos[2], str_replace_all(txt, "\\\\n", "\n"), pos = if (is.na(xy_pos[3])) NULL else xy_pos[3], col = get_settings()$fg_txt_col, xpd = TRUE)
    }    
  }  
  
  # Add "Please edit text"
  if (field_has_value(f$use) && "cds" == f$use) mtext(text = get_settings()$pleaseEdit, side = 3, line = 2.5, adj = 1, font = 4, col = "red", cex = get_settings()$x_axis_numbers_pt) # adj = -.09
}

add_legend <- function(x, line = NULL, line_dot = NULL, bar = NULL, dot = NULL, fan = NULL, color_scheme = get_settings()$ts_col) {
  opar <- par()
  on.exit(suppressWarnings(par(opar)))
  par(fig=c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0:1, 0:1, type="n", bty="n", xaxt="n", yaxt="n")
    
  # Legend depends on type (line, bar, dot, ...)
  nvars <- ncol(x)
  this_pch <- rep(-1, nvars) # 15 for bars, 19 for dots
  this_lty <- rep( 0, nvars) # 1 for lines, 0 otherwise
  pt_cex   <- rep( 2, nvars) # default 2 for bars, 1 for dots
  this_pch[bar]  <- 15
  this_pch[c(dot, line_dot)]  <- 19
  this_pch[fan]  <- 17
  pt_cex[dot]    <-  1
  pt_cex[line_dot] <- .8
  this_lty[c(line, line_dot)] <-  1
  
  # Remove if no col name available
  var_names <- colnames(x)
  cols      <- color_scheme[1:ncol(x)]
  index_cols_to_ignore <- which(is.na(var_names))
  # If FAN, then remove second of each fan chart column
  if (length(fan))
    index_cols_to_ignore <- c(index_cols_to_ignore, fan[2 * 1:(length(fan) %/% 2)])
  if (length(index_cols_to_ignore)) {
    var_names <- var_names[-index_cols_to_ignore]
    cols      <- cols[-index_cols_to_ignore]
    this_pch  <- this_pch[-index_cols_to_ignore]
    this_lty  <- this_lty[-index_cols_to_ignore]
    pt_cex    <- pt_cex[-index_cols_to_ignore]
  }
  
  if (6 == length(fan)) {
    var_names <- append(var_names, c(NA, NA), 1)
    cols      <- append(cols, c(NA, NA), 1)
    this_pch  <- append(this_pch, c(NA, NA), 1)
    this_lty  <- append(this_lty, c(NA, NA), 1)
    pt_cex    <- append(pt_cex, c(NA, NA), 1)
  }
    

  n_items <- length(var_names)
  if (n_items) {
    n_columns <- if (n_items < 3) 1 else 2
    legend(0, .12, legend = var_names, text.font = 3, lwd = get_settings()$ts_lwd, col = cols, bty = "n", cex = get_settings()$x_axis_numbers_pt, text.col = get_settings()$fg_txt_col, ncol = n_columns, pch = this_pch, lty = this_lty, pt.cex = pt_cex)
  }
}

create_pdf <- function(index, file_name, include_fonts = TRUE) {
  pdf(file_name, title = file_name, width = get_settings()$pdf_width / cm(1), height = get_settings()$pdf_height / cm(1), pointsize = get_settings()$pointsize, family = "RijksoverheidSansText")
    figure(index = index, tile = FALSE)
  dev.off()
  
  # Embed fonts
  if (include_fonts) embed_fonts(file_name, outfile = file_name)
  # INSTALL http://macappstore.org/ghostscript/
}


















