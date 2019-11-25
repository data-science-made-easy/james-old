YES         <- c("y", "Y", "yes", "Yes", "YES")
NO          <- c("n", "N", "no", "No", "NO")
SETTINGS    <- list(sep = ",") # Separator in xlsx-fields
TAB_NAME    <- list(help = "help", meta = "meta", constants = "constants")
META        <- list(settings_file = "settings_file", tab = "tab", dir_pdf = "dir_pdf", dir_png = "dir_png", name = "name", pdf = "pdf", png = "png", id = "id", type = "type", import = "import", series_type = "series_type", swap_xy = "swap_xy", create = "create", publish = "publish", publish_data = "publish_data") # Define columns in Excel
JAMES_SETTINGS_M <- "M:/p_james/settings/james-cepmev.xlsx"
JAMES_SETTINGS_LOCAL <- "james-local-settings.xlsx"
ARGS        <- list(add_if_duplicate = "add_if_duplicate")
CLASS       <- list(jdata = "JData")
SERIES_TYPE_LINE_PATTERN  <- "line_x_y"
SERIES_TYPE_LINE          <- "line"      # Translate these to "line_x_y"
SERIES_TYPE_LINE_DOT      <- "line_dot"
SERIES_TYPE_LINE_DASH     <- "line_dash"
SERIES_TYPE_FAN_LINE      <- "fan_line"
SERIES_TYPE_LINES         <- c(SERIES_TYPE_LINE, SERIES_TYPE_LINE_DOT, SERIES_TYPE_LINE_DASH, SERIES_TYPE_FAN_LINE)
SERIES_TYPE_BAR_AIR       <- "bar^"
SERIES_TYPE_BAR_NEXT      <- "bar--"
SERIES_TYPE_BAR_TOP       <- "bar="
SERIES_TYPE_BARS_NEXT     <- c(SERIES_TYPE_BAR_AIR, SERIES_TYPE_BAR_NEXT)
SERIES_TYPE_BARV          <- c(SERIES_TYPE_BAR_NEXT, SERIES_TYPE_BAR_TOP, SERIES_TYPE_BAR_AIR)
SERIES_TYPE_DOT           <- "dot"
SERIES_TYPE_DOT_SMALL     <- "dot_small"
SERIES_TYPE_MARK          <- "mark"
SERIES_TYPE_DOTS          <- c(SERIES_TYPE_DOT, SERIES_TYPE_DOT_SMALL, SERIES_TYPE_MARK)
SERIES_TYPE_WHISKER       <- "whisker"
SERIES_TYPE_FAN           <- "fan"
SERIES_TYPE_BARH          <- "barh"
SERIES_TYPE_PPOWER        <- "ppower"
SERIES_TYPE_PIE           <- "pie"
SERIES_TYPE_GRAPH         <- "graph"
SERIES_TYPE_HEATMAP       <- "heatmap"
SERIES_TYPE_TWO_COLUMNS   <- c(SERIES_TYPE_BAR_AIR, SERIES_TYPE_FAN, SERIES_TYPE_WHISKER)
CLASS_DEFAULT             <- "class_default"
CLASS_BAR                 <- "class_bar"
CLASS_FAN                 <- "class_fan"
CLASS_PPOWER              <- SERIES_TYPE_PPOWER
CLASS_PIE                 <- SERIES_TYPE_PIE
CLASS_GRAPH               <- SERIES_TYPE_GRAPH
CLASS_HEATMAP             <- SERIES_TYPE_HEATMAP