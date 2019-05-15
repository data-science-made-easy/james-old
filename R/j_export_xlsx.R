# #' @export j_export_xslx
# j_export_xlsx <- function(file_name, doc_name, publication_date, sort_manually = NULL) {
#   #
#   ## Definitions
#   #
#   content_tab <- "Overzicht"
#   col_pink    <- "#ca005d"
#   col_blue    <- "#4F81BD"
#
#   sort_fig_name_index <- function(fig_name) {
#     # field1_field_2_field3
#     fig_name_first  <- str_split(fig_name, ",", simplify = T)[, 1]
#     fig_name_fields <- str_split(fig_name_first, "\\.", simplify = T)
#
#     # field1
#     fig_name_field_as_number <- function(txt, mult = 1) {
#       index <- rep(0, length(txt))
#       txt_un <- unique(txt)
#       txt_un_index <- mixedorder(txt_un)
#       for (i in seq_along(txt_un_index)) {
#         index[which(txt == txt_un[txt_un_index[i]])] <- i
#       }
#
#       return(mult * index)
#     }
#
#     index <- rep(0, length(fig_name))
#     if (3 <= ncol(fig_name_fields)) index <- index + fig_name_field_as_number(fig_name_fields[, 3], mult = 1)
#     if (2 <= ncol(fig_name_fields)) index <- index + fig_name_field_as_number(fig_name_fields[, 2], mult = 10 * (1+ max(index)))
#     index <- index + fig_name_field_as_number(fig_name_fields[, 1], mult = 10 * (1 + max(index)))
#     mixedorder(index)  # https://stackoverflow.com/questions/17531403/how-to-sort-a-character-vector-where-elements-contain-letters-and-numbers-in-r
#   }
#
#   #
#   ## Create tableOfContent
#   #
#   # TODO: please improve code:
#   get_field <- function(index, field) j_get(index = index, what = "fig")$specs[[field]]
#   james_table <- j_ls()
#   toc_index   <- james_table$index
#   index_use   <- sapply(toc_index, function(x) field_has_value(get_field(x, "fig_name")))
#   james_table <- james_table[which(index_use), ]
#   toc_index   <- james_table$index
#   fig_name    <- tolower(sapply(toc_index, function(x) get_field(x, "fig_name")))
#   index_sorted <- sort_fig_name_index(fig_name)
#   if (!is.null(sort_manually)) index_sorted <- index_sorted[sort_manually]
#   toc_index   <- toc_index[index_sorted]
#   fig_name    <- tolower(sapply(toc_index, function(x) get_field(x, "fig_name")))
#   # Sort da figs
#   toc_title   <- sapply(toc_index, function(x) get_field(x, "title"))
#   tableOfContent <- cbind(fig_name, toc_title)
#   colnames(tableOfContent) <- c("FIGUUR (h = hoofdstuk, k = kader)", "TITEL")
#
#   #
#   ## Get fig specs
#   #
#   fig_x   <- sapply(toc_index, function(x) get_field(x, "xlab"))
#   fig_y   <- sapply(toc_index, function(x) get_field(x, "ylab"))
#   fig_y2  <- sapply(toc_index, function(x) get_field(x, "y2_lab"))
#   fig_fut <- sapply(toc_index, function(x) get_field(x, "future"))
#
#   #
#   ## Create Workbook object and add worksheets
#   #
#   options("openxlsx.numFmt" = NULL)
#   wb <- createWorkbook()
#
#   ########
#   ######## CONTENT
#   ########
#   addWorksheet(wb, content_tab)
#   setColWidths(wb, content_tab, cols = 1:2, widths = "auto", hidden = rep(FALSE, 2), ignoreMergedCells = FALSE)
#
#   #
#   ## Header
#   #
#   writeData(wb, content_tab, x = doc_name, xy = c("A", 1))
#   addStyle(wb, content_tab, rows = 1, cols = 1, style = createStyle(fontSize = 14, fontColour = col_pink, halign = "left", textDecoration = "bold"))
#   writeData(wb, content_tab, x = publication_date, xy = c("A", 2))
#   addStyle(wb, content_tab, rows = 2, cols = 1, style = createStyle(fontSize = 12, fontColour = col_pink, halign = "left", textDecoration = "italic"))
#
#   #
#   ## Content table
#   #
#   options("openxlsx.borderColour" = col_pink)
#   style_tableOfContent <- createStyle(fontSize = 12, fontColour = "#ffffff", fgFill = col_pink, halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight")
#   writeData(wb, content_tab, tableOfContent, colNames = TRUE, rowNames = FALSE, startCol="A", startRow = 4, borders = "columns", headerStyle = style_tableOfContent)
#   # Overwrite first column with hyperlinks to tabs
#   for (i in seq_along(tableOfContent[,1])) {
#     writeFormula(wb, content_tab, xy = c(1, 4 + i), x = makeHyperlinkString(sheet = fig_name[i], row = 3, col = 1, text = fig_name[i]))
#   }
#
#   ########
#   ######## DATA
#   ########
#   options("openxlsx.borderColour" = col_blue)
#   style_sheet_data <- createStyle(fontColour = "#ffffff", fgFill = col_blue, halign = "center", valign = "center", textDecoration = "bold", border = "TopBottomLeftRight")
#   style_sheet_meta <- createStyle(fontColour = "#ffffff", fgFill = col_blue, halign = "left", textDecoration = "bold", border = "TopBottomLeftRight")
#   for (i in seq_along(toc_index)) {
#     sheet_name <- as.character(fig_name[i])
#     #print(sheet_name)
#     addWorksheet(wb, sheet_name)
#     #print(">>> done")
#
#     # ts to matrix
#     data_set <- j_get(index = toc_index[i])
#     data_set_colnames <- colnames(data_set)
#     if (is.ts(data_set)) {
#       data_set <- cbind(as.vector(time(data_set)), head(data_set, n = nrow(data_set)))
#     }
#
#     # Replace \n newlines with " " blank space
#     if (any(0 < str_count(data_set[,1], "\\\\n"))) data_set[, 1] <- str_replace_all(data_set[, 1], "\\\\n", " ")
#
#     # Fix header NA
#     if (is.null(colnames(data_set))) colnames(data_set) <- c(" ", data_set_colnames)
#     # if (is.matrix(data_set)) {
#       #print(paste("COLNAMES:", colnames(data_set)))
#       index_NA_colnames <- c(which(is.na(colnames(data_set))), grep("(^NA\\.)(\\d+)($)", colnames(data_set)))
#       if (length(index_NA_colnames)) colnames(data_set)[index_NA_colnames] <- " "
#     # }
#     colnames(data_set)[1] <- " "
#     ncol_data_set <- ncol(data_set)
#     writeData(wb, sheet_name, x = data_set, xy = c("A", 2), borders = "columns", headerStyle = style_sheet_data)#, xy = c("A"))
#     addStyle(wb, sheet_name, rows = 2 + 1:nrow(data_set), cols = 1:ncol_data_set, gridExpand = TRUE, style = createStyle(borderColour = col_blue, border = "LeftRight", numFmt = "0.0"))
#     addStyle(wb, sheet_name, rows = 3, cols = 1:ncol_data_set, gridExpand = TRUE, style = createStyle(borderColour = col_blue, border = "TopLeftRight", numFmt = "0.0"))
#     addStyle(wb, sheet_name, rows = 2 + nrow(data_set), cols = 1:ncol_data_set, gridExpand = T, style = createStyle(borderColour = col_blue, border = c("bottom", "left", "right"), numFmt = "0.0"))
#     # addStyle(wb, sheet_name, style = createStyle(numFmt = "0.0"), rows = 2 + 1:nrow(data_set), cols = 2:ncol_data_set, gridExpand = TRUE)
#
#
#     # Add fig name to top
#     writeData(wb, sheet_name, x = "", xy = c("A", 2))
#     mergeCells(wb, sheet_name, cols = 1:ncol_data_set, rows = 1)
#     writeData(wb, sheet_name, x = "DATA", xy = c("A", 1))
#     addStyle(wb, sheet_name, rows = 1, cols = 1, style = createStyle(fontSize = 12, fgFill = col_pink, fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold"))
#     setColWidths(wb, sheet_name, cols = 1:ncol_data_set, widths = "auto", hidden = rep(FALSE, ncol_data_set), ignoreMergedCells = FALSE)
#
#     # Add meta data
#     col_meta <- 2:3 + ncol_data_set
#     mergeCells(wb, sheet_name, cols = col_meta, rows = 1)
#     writeData(wb, sheet_name, x = "BESCHRIJVING", xy = c(col_meta[1], 1))
#     addStyle(wb, sheet_name, rows = 1, cols = col_meta[1], style = createStyle(fontSize = 12, fgFill = col_pink, fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold"))
#     table_description <- c("Titel", toc_title[i])
#     if (field_has_value(fig_x[i])) table_description <- rbind(table_description, c("x-as", fig_x[i]))
#     if (field_has_value(fig_y[i])) table_description <- rbind(table_description, c("y-as", fig_y[i]))
#     if (field_has_value(fig_y2[i])) table_description <- rbind(table_description, c("y-as (r)", fig_y2[i]))
#     if (!is.matrix(table_description)) table_description <- matrix(table_description, nrow = 1)
#     # table_description <- rbind(table_description, c("Raming", fig_fut[i]))
#     writeData(wb, sheet_name, x = table_description, xy = c(col_meta[1], 2), rowNames = FALSE, colNames = FALSE, borders = "columns", headerStyle = style_sheet_meta)
#     addStyle(wb, sheet_name, rows = 1 + 1:nrow(table_description), cols = col_meta[1], style = createStyle(fgFill = col_blue, fontColour = "#FFFFFF", halign = "left", textDecoration = "bold"))
#     setColWidths(wb, sheet_name, cols = col_meta, widths = "auto", hidden = rep(FALSE, length(col_meta)), ignoreMergedCells = FALSE)
#
#     # Link to OVERZICHT
#     writeFormula(wb, sheet_name, xy = c(col_meta[1], 3 + nrow(table_description)), x = makeHyperlinkString(sheet = "OVERZICHT", row = 4 + i, col = 1, text = "Link naar overzicht"))
#     # writeFormula(wb, sheet_name, startRow = 2, x = makeHyperlinkString(sheet = "OVERZICHT", row = 2 + i, col = 1, text = "Link naar overzicht"))
#   }
#
#   # Save
#   saveWorkbook(wb, file_name, overwrite = TRUE)
# }
