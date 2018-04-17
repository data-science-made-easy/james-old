# rm(list = ls(all = T))
#
# library(stringr)
# library(extrafont)
#
# source("~/Dropbox/cpb/git/james/global.R")
# source("~/Dropbox/cpb/git/james/james-light.R")
# source("~/Dropbox/cpb/git/james/james-figures.R")

validate_xlsx <- function(file_name) {
  sheet_names <- openxlsx::getSheetNames(file_name)
  
  sheet_names_data <- setdiff(sheet_names, c(TAB_NAME$meta, TAB_NAME$help))

  # get meta data
  sheet_i_meta <- which(TAB_NAME$meta == sheet_names)
  meta_data  <- if (1 == length(sheet_i_meta)) openxlsx::read.xlsx(file_name, sheet = sheet_i_meta) else NULL

  is_valid_id <- setequal(sheet_names_data, meta_data$id)
  
  if (!is_valid_id) {
    print(paste0("No data tab for meta ID: ", paste(setdiff(meta_data$id, sheet_names_data), collapse = ", ")))
    print(paste0("No meta ID for data tab: ", paste(setdiff(sheet_names_data, meta_data$id), collapse = ", ")))
  }
  
  
  id_set <- unique(c(sheet_names_data, meta_data$id))
  is_valid_name <- TRUE
  for (char in c(" ", "/", ":", '"', "\\\\", "'", "\\*", "\\?", "<", ">", "\\|", ",")) { # , "\\"
    index_char <- stringr::str_detect(id_set, char)
    if (" " == char) char <- "space"
    if (any(index_char)) {
      is_valid_name <- FALSE
      print(paste0("IDs with a <", char, "> (please fix): ", paste0(id_set[index_char], collapse = ", ")))
    }
  }
  
  return(is_valid_id && is_valid_name)
}

import_xlsx_as_ts <- function(file_name, scenario = .j_root$active_scenario, project = .j_root$active_project) {
  # workbook <- openxlsx::loadWorkbook(file_name)
  sheet_names <- openxlsx::getSheetNames(file_name)

  # get meta data
  sheet_i_meta <- which(TAB_NAME$meta == sheet_names)
  meta_data  <- if (1 == length(sheet_i_meta)) openxlsx::read.xlsx(file_name, sheet = sheet_i_meta) else NULL

  report <- NULL
  for (sheet_i in 1:length(sheet_names)) {
    print(paste(sheet_i, ";", sheet_names[sheet_i]))

    if (sheet_i == sheet_i_meta || TAB_NAME$help == sheet_names[sheet_i]) # Skip meta data sheet and skip help sheet
      next;
    
    # Get sheet name
		sheet_name <- sheet_names[sheet_i]
    
    this_meta_data <- as.list(meta_data[which(sheet_name == meta_data$id),])
    tab <- openxlsx::read.xlsx(file_name, sheet = sheet_i)

  	# Fix col names
    index_NA_colnames <- grep("(^X)(\\d+)($)", colnames(tab))
    if (length(index_NA_colnames)) colnames(tab)[index_NA_colnames] <- NA
		colnames(tab) <- str_replace_all(colnames(tab), "\\.", " ")
    
		# Convert to ts or matrix object
    if (is_really_character(tab[, 1])) { # in case it is some (bar?)plot with char labels
      # ts_obj    <- as.matrix(tab, nc = ncol(tab), dimnames = list(NULL, colnames(tab)))
      ts_obj <- tab
      rownames(ts_obj) <- NULL
      # ts_obj <- as.data.frame(ts_obj, stringsAsFactors = F) # Fix issue with text in Excel output
      # for (col_i in 2:ncol(ts_obj)) class(ts_obj[, col_i]) <- "numeric"
      #   stop()
		} else {
			# Get time vector
			time_vector <- as.numeric(tab[,1])
			time_start  <- time_vector[1]
	    time_freq   <- 1 / diff(time_vector)[1]
	    time_end    <- tail(time_vector, 1)
      
      # Check time statistics to check we have regular time series?
      if (abs((time_end - time_start) / (length(time_vector) - 1) - diff(time_vector)[1]) < 1e-10) {
        ts_obj <- ts(data = tab[, -1, drop = FALSE], start = time_start, end = time_end, frequency = time_freq)		
      } else {
        ts_obj <- as.matrix(tab, nc = ncol(tab), dimnames = list(NULL, colnames(tab)))
      }      
		}

    # Add if non-existent
    sheet_is_imported <- j_put(ts_obj, type = sheet_name, scenario = scenario, project = project)
    import_text <- paste0("<B><span style='color:", if (sheet_is_imported) "#00a65a" else "#dd4b39", "'>", if (sheet_is_imported) "TRUE" else "FALSE", "</span></B>")
    
    # Gather some information
    object           <- j_get(type = sheet_name, scenario = scenario, project = project, what = "object")
    object$fig$specs <- this_meta_data
    fig_title        <- object$fig$specs[[META_FIELDS$title]]
    ts_names         <- paste(colnames(object$data), collapse = ", ")
    ts_versions      <- paste(j_ls(type = sheet_name, scenario = scenario, project = project, collapse = FALSE)$version, collapse = ", ")
    
    report <- rbind(report, c(Figure = sheet_name, "Version(s)" = ts_versions, "Title" = fig_title, Series = ts_names, "Import date" = object$born, Imported = import_text))
  }

  # Return  
  return(report)
}

# j_init(file_name = "~/Dropbox/cpb/git/james/figures.james", active_scenario = "s", active_project = "p")
#
# file_name = "/Users/mdijkstra/Dropbox/cpb/projects/ramingen_db/data-figuren-kCEP2018.xlsx"
# import_xlsx_as_ts(file_name)


export_xlsx <- function(file_name, doc_name, publication_date, sort_manually = NULL) {
  #
  ## Definitions
  #
  content_tab <- "Overzicht"
  col_pink    <- "#ca005d"
  col_blue    <- "#4F81BD"
  
  sort_fig_name_index <- function(fig_name) {
    # field1_field_2_field3
    fig_name_first  <- str_split(fig_name, ",", simplify = T)[, 1]
    fig_name_fields <- str_split(fig_name_first, "\\.", simplify = T)

    # field1
    fig_name_field_as_number <- function(txt, mult = 1) {
      index <- rep(0, length(txt))
      txt_un <- unique(txt)
      txt_un_index <- mixedorder(txt_un)
      for (i in seq_along(txt_un_index)) {
        index[which(txt == txt_un[txt_un_index[i]])] <- i
      }

      return(mult * index)
    }

    index <- rep(0, length(fig_name))
    if (3 <= ncol(fig_name_fields)) index <- index + fig_name_field_as_number(fig_name_fields[, 3], mult = 1)
    if (2 <= ncol(fig_name_fields)) index <- index + fig_name_field_as_number(fig_name_fields[, 2], mult = 10 * (1+ max(index)))
    index <- index + fig_name_field_as_number(fig_name_fields[, 1], mult = 10 * (1 + max(index)))
    mixedorder(index)  # https://stackoverflow.com/questions/17531403/how-to-sort-a-character-vector-where-elements-contain-letters-and-numbers-in-r
  }
  
  #
  ## Create tableOfContent
  #
  # TODO: please improve code:
  get_field <- function(index, field) j_get(index = index, what = "fig")$specs[[field]]
  james_table <- j_ls()
  toc_index   <- james_table$index
  index_use   <- sapply(toc_index, function(x) field_has_value(get_field(x, "fig_name")))
  james_table <- james_table[which(index_use), ]
  toc_index   <- james_table$index
  fig_name    <- tolower(sapply(toc_index, function(x) get_field(x, "fig_name")))
  index_sorted <- sort_fig_name_index(fig_name) 
  if (!is.null(sort_manually)) index_sorted <- index_sorted[sort_manually]
  toc_index   <- toc_index[index_sorted]
  fig_name    <- tolower(sapply(toc_index, function(x) get_field(x, "fig_name")))
  # Sort da figs  
  toc_title   <- sapply(toc_index, function(x) get_field(x, "title"))
  tableOfContent <- cbind(fig_name, toc_title)
  colnames(tableOfContent) <- c("FIGUUR (h = hoofdstuk, k = kader)", "TITEL")

  #
  ## Get fig specs
  #
  fig_x   <- sapply(toc_index, function(x) get_field(x, "xlab"))
  fig_y   <- sapply(toc_index, function(x) get_field(x, "ylab"))
  fig_y2  <- sapply(toc_index, function(x) get_field(x, "y2_lab"))
  fig_fut <- sapply(toc_index, function(x) get_field(x, "future"))

  #
  ## Create Workbook object and add worksheets
  #
  options("openxlsx.numFmt" = NULL)
  wb <- createWorkbook()

  ########
  ######## CONTENT
  ########
  addWorksheet(wb, content_tab)
  setColWidths(wb, content_tab, cols = 1:2, widths = "auto", hidden = rep(FALSE, 2), ignoreMergedCells = FALSE)
    
  #
  ## Header
  #
  writeData(wb, content_tab, x = doc_name, xy = c("A", 1))
  addStyle(wb, content_tab, rows = 1, cols = 1, style = createStyle(fontSize = 14, fontColour = col_pink, halign = "left", textDecoration = "bold"))
  writeData(wb, content_tab, x = publication_date, xy = c("A", 2))
  addStyle(wb, content_tab, rows = 2, cols = 1, style = createStyle(fontSize = 12, fontColour = col_pink, halign = "left", textDecoration = "italic"))

  #
  ## Content table
  #
  options("openxlsx.borderColour" = col_pink)
  style_tableOfContent <- createStyle(fontSize = 12, fontColour = "#ffffff", fgFill = col_pink, halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight")
  writeData(wb, content_tab, tableOfContent, colNames = TRUE, rowNames = FALSE, startCol="A", startRow = 4, borders = "columns", headerStyle = style_tableOfContent)
  # Overwrite first column with hyperlinks to tabs
  for (i in seq_along(tableOfContent[,1])) {
    writeFormula(wb, content_tab, xy = c(1, 4 + i), x = makeHyperlinkString(sheet = fig_name[i], row = 3, col = 1, text = fig_name[i]))    
  }

  ########
  ######## DATA
  ########
  options("openxlsx.borderColour" = col_blue)
  style_sheet_data <- createStyle(fontColour = "#ffffff", fgFill = col_blue, halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight")
  style_sheet_meta <- createStyle(fontColour = "#ffffff", fgFill = col_blue, halign = "left", textDecoration = "bold", border = "TopBottomLeftRight")
  for (i in seq_along(toc_index)) {
    sheet_name <- as.character(fig_name[i])
    #print(sheet_name)
    addWorksheet(wb, sheet_name)
    #print(">>> done")
    
    # ts to matrix
    data_set <- j_get(index = toc_index[i])
    data_set_colnames <- colnames(data_set)
    if (is.ts(data_set)) {
      data_set <- cbind(as.vector(time(data_set)), head(data_set, n = nrow(data_set)))
    }
    
    # Replace \n newlines with " " blank space
    if (any(0 < str_count(data_set[,1], "\\\\n"))) data_set[, 1] <- str_replace_all(data_set[, 1], "\\\\n", " ")
    
    # Fix header NA
    if (is.null(colnames(data_set))) colnames(data_set) <- c(" ", data_set_colnames)
    # if (is.matrix(data_set)) {
      #print(paste("COLNAMES:", colnames(data_set)))
      index_NA_colnames <- c(which(is.na(colnames(data_set))), grep("(^NA\\.)(\\d+)($)", colnames(data_set)))
      if (length(index_NA_colnames)) colnames(data_set)[index_NA_colnames] <- " "      
    # }
    colnames(data_set)[1] <- " "
    ncol_data_set <- ncol(data_set)
    writeData(wb, sheet_name, x = data_set, xy = c("A", 2), borders = "columns", headerStyle = style_sheet_data)#, xy = c("A"))
    addStyle(wb, sheet_name, rows = 2 + 1:nrow(data_set), cols = 1:ncol_data_set, gridExpand = TRUE, style = createStyle(borderColour = col_blue, border = "LeftRight", numFmt = "0.0"))
    addStyle(wb, sheet_name, rows = 3, cols = 1:ncol_data_set, gridExpand = TRUE, style = createStyle(borderColour = col_blue, border = "TopLeftRight", numFmt = "0.0"))
    addStyle(wb, sheet_name, rows = 2 + nrow(data_set), cols = 1:ncol_data_set, gridExpand = T, style = createStyle(borderColour = col_blue, border = c("bottom", "left", "right"), numFmt = "0.0"))
    # addStyle(wb, sheet_name, style = createStyle(numFmt = "0.0"), rows = 2 + 1:nrow(data_set), cols = 2:ncol_data_set, gridExpand = TRUE)


    # Add fig name to top
    writeData(wb, sheet_name, x = "", xy = c("A", 2))
    mergeCells(wb, sheet_name, cols = 1:ncol_data_set, rows = 1)
    writeData(wb, sheet_name, x = "DATA", xy = c("A", 1))
    addStyle(wb, sheet_name, rows = 1, cols = 1, style = createStyle(fontSize = 12, fgFill = col_pink, fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold"))
    setColWidths(wb, sheet_name, cols = 1:ncol_data_set, widths = "auto", hidden = rep(FALSE, ncol_data_set), ignoreMergedCells = FALSE)
    
    # Add meta data
    col_meta <- 2:3 + ncol_data_set
    mergeCells(wb, sheet_name, cols = col_meta, rows = 1)
    writeData(wb, sheet_name, x = "BESCHRIJVING", xy = c(col_meta[1], 1))
    addStyle(wb, sheet_name, rows = 1, cols = col_meta[1], style = createStyle(fontSize = 12, fgFill = col_pink, fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold"))
    table_description <- c("Titel", toc_title[i])
    if (field_has_value(fig_x[i])) table_description <- rbind(table_description, c("x-as", fig_x[i]))
    if (field_has_value(fig_y[i])) table_description <- rbind(table_description, c("y-as", fig_y[i]))
    if (field_has_value(fig_y2[i])) table_description <- rbind(table_description, c("y-as (r)", fig_y2[i]))
    if (!is.matrix(table_description)) table_description <- matrix(table_description, nrow = 1)
    # table_description <- rbind(table_description, c("Raming", fig_fut[i]))
    writeData(wb, sheet_name, x = table_description, xy = c(col_meta[1], 2), rowNames = FALSE, colNames = FALSE, borders = "columns", headerStyle = style_sheet_meta)
    addStyle(wb, sheet_name, rows = 1 + 1:nrow(table_description), cols = col_meta[1], style = createStyle(fgFill = col_blue, fontColour = "#FFFFFF", halign = "left", textDecoration = "bold"))
    setColWidths(wb, sheet_name, cols = col_meta, widths = "auto", hidden = rep(FALSE, length(col_meta)), ignoreMergedCells = FALSE)
    
    # Link to OVERZICHT
    writeFormula(wb, sheet_name, xy = c(col_meta[1], 3 + nrow(table_description)), x = makeHyperlinkString(sheet = "OVERZICHT", row = 4 + i, col = 1, text = "Link naar overzicht"))
    # writeFormula(wb, sheet_name, startRow = 2, x = makeHyperlinkString(sheet = "OVERZICHT", row = 2 + i, col = 1, text = "Link naar overzicht"))
  }

  # Save
  saveWorkbook(wb, file_name, overwrite = TRUE)
}














# # Remove empty rows/cols
# index_NA_rows <- which(apply(tab, 1, function(vec) all(is.na(vec))))
# index_NA_cols <- which(apply(tab, 2, function(vec) all(is.na(vec))))
# if (0 < length(index_NA_rows)) tab <- tab[-index_NA_rows, ]
# if (0 < length(index_NA_cols)) tab <- tab[, -index_NA_cols]
