# Global constants
COL_SCHEME  <- list(dot_median = "dot_median", fan = "fan")
Y2_OPTIONS  <- c("y", "Y")
USE_OPTIONS <- c("y", "cds")
TAB_NAME    <- list(meta = "meta", settings = "setting", help = "help")
META_FIELDS <- list(id = "id",	use = "use", "title" = "title",	ylab = "ylab", series_type = "series_type", hline_reference = "hline_reference", x_at = "x_at",	x_labels = "x_labels", x_lim = "x_lim", y_lim = "y_lim", future = "future", xlab = "xlab", email = "email", comment = "comment")
FIG_TYPE    <- list(line = "line", line_dot = "line_dot", bar_next = "bar--", bar_top = "bar=", dot = "dot", dot_small = "dot_small", x_axis = "x_axis", fan = "fan")

# Help function
field_has_value <- function(val) {
  if (0 == length(val)) # NB 0 == length(NULL)
    return(FALSE)
  if (is.na(val))
    return(FALSE)
  if ("" == str_trim(val))
    return(FALSE)
  return(TRUE)
}
as_char_vec    <- function(str, sep = ",") str_trim(unlist(str_split(str, sep)))
as_numeric_vec <- function(str) as.numeric(as_char_vec(str))

# General functions
get_settings <- function(.debug_refresh = FALSE) {

  get_settings_helper <- function() j_get(type = "settings", scenario = "figures", project = "cpb", what = "data")

  if (!.debug_refresh && !is.null(get_settings_helper())) return(get_settings_helper())
  
  figure_settings <- list(
    available_docs      = c("MEV 2018", "kCEP 2018", "CEP 2018", "kMEV 2019"),
    decimal_sep         = ",",
    ts_lwd              = 1.5,
    ts_col				      = c("#01689b", "#8fcae7", "#ca005d", "#ea99be", "#ffb612", "#e17000", "#39870c", "#c3dbb6"),
    col_dot_median      = c("#8fcae7", "#ea99be", "#ca005d", "#ea99be", "#8fcae7", "#ca005d", "#ea99be", "#ffb612", "#e17000", "#39870c", "#c3dbb6"),
    col_fan             = c("#ca005d", "#d6f1ffB0", "#d6f1ffB0", "#8fcae7B0", "#8fcae7B0", "#01689bB0", "#01689bB0", "#ea99be", "#ffb612", "#e17000", "#39870c", "#c3dbb6"),
    #c("#007bc7", "#e17000", "#b2d7ee", "#154273", "#eda966", "#ffffe1", "#d3d3d3", "#d3d3d3", "#d3d3d3"), #c("#1B9E77", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#A6CEE3", "#E6AB02", "gray80", "#E31A1C", "mediumorchid1", "seagreen1"), #,
    title_pt            = 9/7, # scaled to pointsize of 7
    text_pt             = 1, # scaled to pointsize of 7
    # title_shift_left    = -.3, # title + y-lab shift to left
    x_axis_numbers_pt   = 1, # scaled to pointsize of 7
    x_axis_chars_pt     = .7, # scaled to pointsize of 7
    fg_txt_col          = "black",
    bg_col              = NA, #"#fbead9",
  	axis_col			      = "black", #"#999999",#"gray50",
    y_axis_empty_above  = 1/6,
    y_axis_empty_below  = 1/6,
    y_axis_hline_lwd    = 0.25,
  	axis_cex			      = 2,
    x_axis_ticks_lwd    = 0.25,
    x_axis_ticks_length = 0.015,
    x_axis_height       = .5,
    x_axis_vertical_gap = -1.5,
    # mar_single = c(2.5, 6, 0, 7.5 * text_size),
    usr_rect_fill_col   = "#C3FAFF",
    usr_rect_border_col = "#555555",
    usr_rect_border_lwd = 0.5,
    raming_rect_col     = "#e6e6e6", #"#ADD8E6",
    raming_text         = "",#"Raming",
    hline_reference_lwd = 1,
    hline_reference_lty = 1,
    hline_reference_col = "black", #"#555555",
    hline_dotted_col    = "gray50",
    hline_dotted_lwd    = 1,
    bar_fill_horizontal = 0.5,
    pleaseEdit_text     = "Please edit!",
    orange              = "#f39c12",
    green               = "#00a65a",
    blue                = "#3c8dbc",
    gray                = "gray50",
    red                 = "#cc0000",
    help_text_color     = "gray",
    pdf_width	          = 7.5, # cm
    pdf_height	        = 7.5,  # cm # Enkel grafiekimport_xlsx_as_tscm
    pdf_mai             = c(1.02, 1.02, 0, 1.02),
    pointsize           = 7, # pt is the default. Scale title with 9/7 to get 9 pt. # 12 / cm(1)
    iframe_height       = 550 # px
  )

  j_put(x = figure_settings, type = "settings", scenario = "figures", project = "cpb", activate_project_scenario = FALSE)
  j_save()
  
  get_settings_helper()
}

#get_settings(T)