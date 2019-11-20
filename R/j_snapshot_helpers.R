guess_doc_specs <- function() {
  month <- as.numeric(format(Sys.time(), "%m"))
  year  <- as.numeric(format(Sys.time(), "%Y"))
  day   <- as.numeric(format(Sys.time(), "%d"))
  
  if (month < 4 & day < 15) {
    doc_name         <- "cCEP"
    file_name        <- paste0("cCEP_", year)
  } else if (month == 3 & 15 < day) {
    doc_name         <- "CEP"
    file_name        <- paste0("CEP_", year)
  } else if (month < 7) {
    doc_name         <- "kMEV"
    file_name        <- paste0("kMEV_", 1 + year)
  } else if (month < 9) {
    doc_name         <- "cMEV"
    file_name        <- paste0("cMEV_", 1 + year)
  } else if (month < 10) {
    doc_name         <- "MEV"
    file_name        <- paste0("MEV_", 1 + year)
  } else if (10 < month) {
    doc_name         <- "kCEP"
    file_name        <- paste0("kCEP_", 1 + year)
  }
  
  file_name <- paste0(file_name, ".xlsx")
  publication_date <- paste0(month, "-", year)

  return(list(file_name = file_name, doc_name = doc_name, publication_date = publication_date))
}

j_export_xlsx <- function(file_name = guess_doc_specs()$file_name, doc_name = guess_doc_specs()$doc_name, publication_date = guess_doc_specs()$publication_date, include_figs = FALSE) {  
  content_tab <- "Overzicht"
  col_pink    <- "#ca005d"
  col_blue    <- "#4F81BD"
  
  sort_fig_name_index <- function(fig_name) {
    # field1_field_2_field3
    fig_name_first  <- stringr::str_split(fig_name, ",", simplify = T)[, 1]
    fig_name_fields <- stringr::str_split(fig_name_first, "\\.", simplify = T)

    # field1
    fig_name_field_as_number <- function(txt, mult = 1) {
      index <- rep(0, length(txt))
      txt_un <- unique(txt)
      txt_un_index <- gtools::mixedorder(txt_un)
      for (i in seq_along(txt_un_index)) {
        index[which(txt == txt_un[txt_un_index[i]])] <- i
      }

      return(mult * index)
    }

    index <- rep(0, length(fig_name))
    if (3 <= ncol(fig_name_fields)) index <- index + fig_name_field_as_number(fig_name_fields[, 3], mult = 1)
    if (2 <= ncol(fig_name_fields)) index <- index + fig_name_field_as_number(fig_name_fields[, 2], mult = 10 * (1+ max(index)))
    index <- index + fig_name_field_as_number(fig_name_fields[, 1], mult = 10 * (1 + max(index)))
    gtools::mixedorder(index)  # https://stackoverflow.com/questions/17531403/how-to-sort-a-character-vector-where-elements-contain-letters-and-numbers-in-r
  }

  get_field <- function(index, field) {
    if (missing(index)) index <- 1:nrow(james_table)
    unlist(sapply(index, function(i) j_get(i, what = "meta")[[field]]))
  }

  james_table     <- james:::j_ls_cols(cols = c("index", "name", "title", "dim|class", "x_lab", "y_lab", "y2_lab", "png", "pdf_width", "pdf_height"))
  index_new_order <- sort_fig_name_index(tolower(james_table$name))
  james_table     <- james_table[index_new_order, ]
  toc             <- cbind("FIGUUR (h = hoofdstuk, k = kader)" = james_table$name, "TITEL" = james_table$title)

  #
  ## Create Workbook object and add worksheets
  #
  options("openxlsx.numFmt" = NULL)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, content_tab)
  openxlsx::setColWidths(wb, content_tab, cols = 1:2, widths = "auto", hidden = rep(FALSE, 2), ignoreMergedCells = FALSE)

  # Header
  openxlsx::writeData(wb, content_tab, x = doc_name, xy = c("A", 1))
  openxlsx::addStyle(wb, content_tab, rows = 1, cols = 1, style = openxlsx::createStyle(fontSize = 14, fontColour = col_pink, halign = "left", textDecoration = "bold"))
  openxlsx::writeData(wb, content_tab, x = publication_date, xy = c("A", 2))
  openxlsx::addStyle(wb, content_tab, rows = 2, cols = 1, style = openxlsx::createStyle(fontSize = 12, fontColour = col_pink, halign = "left", textDecoration = "italic"))

  #
  ## Content table
  #
  options("openxlsx.borderColour" = col_pink)
  style_tableOfContent <- openxlsx::createStyle(fontSize = 12, fontColour = "#ffffff", fgFill = col_pink, halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight")
  openxlsx::writeData(wb, content_tab, toc, colNames = TRUE, rowNames = FALSE, startCol="A", startRow = 4, borders = "columns", headerStyle = style_tableOfContent)
  # Overwrite first column with hyperlinks to tabs
  for (i in 1:nrow(james_table)) {
    openxlsx::writeFormula(wb, content_tab, xy = c(1, 4 + i), x = openxlsx::makeHyperlinkString(sheet = james_table$name[i], row = 3, col = 1, text = james_table$name[i]))
  }

  #
  ## Data
  #
  options("openxlsx.borderColour" = col_blue)
  style_sheet_data <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = col_blue, halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight")
  style_sheet_meta <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = col_blue, halign = "left", textDecoration = "bold", border = "TopBottomLeftRight")

  for (i in 1:nrow(james_table)) {
    sheet_name <- james_table$name[i]
    openxlsx::addWorksheet(wb, sheet_name)

    # ts to matrix
    data_set <- j_get(index = james_table$index[i])

    # Replace \n newlines with " " blank space
    index_newline <- which(0 < stringr::str_count(data_set[,1], "\\\\n"))
    if (length(index_newline)) {
      data_set[index_newline, 1] <- stringr::str_replace_all(data_set[index_newline, 1], "\\\\n", " ")
    }

    # Fix header NA
    if (is.null(colnames(data_set))) colnames(data_set) <- " "#c(" ", colnames(data_set))
    index_NA_colnames <- c(which(is.na(colnames(data_set))), grep("(^NA\\.)(\\d+)($)", colnames(data_set)))
    if (length(index_NA_colnames)) colnames(data_set)[index_NA_colnames] <- " "
    colnames(data_set)[1] <- " "
    ncol_data_set <- ncol(data_set)
    openxlsx::writeData(wb, sheet_name, x = data_set, xy = c("A", 2), borders = "columns", headerStyle = style_sheet_data)#, xy = c("A"))
    openxlsx::addStyle(wb, sheet_name, rows = 2 + 1:nrow(data_set), cols = 1:ncol_data_set, gridExpand = TRUE, style = openxlsx::createStyle(borderColour = col_blue, border = "LeftRight", numFmt = "0.0"))
    openxlsx::addStyle(wb, sheet_name, rows = 3, cols = 1:ncol_data_set, gridExpand = TRUE, style = openxlsx::createStyle(borderColour = col_blue, border = "TopLeftRight", numFmt = "0.0"))
    openxlsx::addStyle(wb, sheet_name, rows = 2 + nrow(data_set), cols = 1:ncol_data_set, gridExpand = T, style = openxlsx::createStyle(borderColour = col_blue, border = c("bottom", "left", "right"), numFmt = "0.0"))
    # addStyle(wb, sheet_name, style = createStyle(numFmt = "0.0"), rows = 2 + 1:nrow(data_set), cols = 2:ncol_data_set, gridExpand = TRUE)


    # Add fig name to top
    openxlsx::writeData(wb, sheet_name, x = "", xy = c("A", 2))
    openxlsx::mergeCells(wb, sheet_name, cols = 1:ncol_data_set, rows = 1)
    openxlsx::writeData(wb, sheet_name, x = "DATA", xy = c("A", 1))
    openxlsx::addStyle(wb, sheet_name, rows = 1, cols = 1, style = openxlsx::createStyle(fontSize = 12, fgFill = col_pink, fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold"))
    openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol_data_set, widths = "auto", hidden = rep(FALSE, ncol_data_set), ignoreMergedCells = FALSE)

    # Add meta data
    col_meta <- 2:3 + ncol_data_set
    openxlsx::mergeCells(wb, sheet_name, cols = col_meta, rows = 1)
    openxlsx::writeData(wb, sheet_name, x = "BESCHRIJVING", xy = c(col_meta[1], 1))
    openxlsx::addStyle(wb, sheet_name, rows = 1, cols = col_meta[1], style = openxlsx::createStyle(fontSize = 12, fgFill = col_pink, fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold"))
    table_description <- c("Titel", james_table$title[i])
    if (james:::has_value(james_table$x_lab[i])) table_description <- rbind(table_description, c("x-as", james_table$x_lab[i]))
    if (james:::has_value(james_table$y_lab[i])) table_description <- rbind(table_description, c("y-as", james_table$y_lab[i]))
    if (james:::has_value(james_table$y2_lab[i])) table_description <- rbind(table_description, c("y-as (r)", james_table$y2_lab[i]))
    if (!is.matrix(table_description)) table_description <- matrix(table_description, nrow = 1)
    # table_description <- rbind(table_description, c("Raming", fig_fut[i]))
    openxlsx::writeData(wb, sheet_name, x = table_description, xy = c(col_meta[1], 2), rowNames = FALSE, colNames = FALSE, borders = "columns", headerStyle = style_sheet_meta)
    openxlsx::addStyle(wb, sheet_name, rows = 1 + 1:nrow(table_description), cols = col_meta[1], style = openxlsx::createStyle(fgFill = col_blue, fontColour = "#FFFFFF", halign = "left", textDecoration = "bold"))
    openxlsx::setColWidths(wb, sheet_name, cols = col_meta, widths = "auto", hidden = rep(FALSE, length(col_meta)), ignoreMergedCells = FALSE)

    # Link to OVERZICHT
    openxlsx::writeFormula(wb, sheet_name, xy = c(col_meta[1], 3 + nrow(table_description)), x = openxlsx::makeHyperlinkString(sheet = "OVERZICHT", row = 1, col = 1, text = "Link naar overzicht")) # row = 4 + i
    # writeFormula(wb, sheet_name, startRow = 2, x = makeHyperlinkString(sheet = "OVERZICHT", row = 2 + i, col = 1, text = "Link naar overzicht"))
    
    # Add PNG
    if (include_figs & file.exists(james_table$png[i])) {
      openxlsx::insertImage(wb, sheet_name, file = james_table$png[i], startRow = 5 + nrow(table_description),  startCol = col_meta[1], width = james_table$pdf_width[i], height = james_table$pdf_height[i], units = "in")
    }
  }

  # Save
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
}
