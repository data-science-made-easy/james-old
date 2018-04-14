rm(list = ls(all = T))

library(stringr)
library(extrafont)
library(openxlsx)
library(gtools)

source("global.R")
source("james-light.R")
source("james-figures.R")
source("import-xls-functions.R")

doc_name  <- DEFAULT
scenario  <- DEFAULT
READ_DATA <- T
MAKE_FIGS <- T
DATA_FILE <- F

input_path_prefix <- "data"
j_init(file_name = paste0(input_path_prefix, "/data.james"), active_scenario = scenario, active_project = doc_name)

if (READ_DATA) {
  file_list <- "aad"
  for (user_file in file_list) { # , 
    file_name = paste0(input_path_prefix, "/", user_file, ".xlsx")

    if (!validate_xlsx(file_name)) print(file_name) else import_xlsx_as_ts(file_name)
  }
}

path_base  <- paste0(input_path_prefix, "/output")#tempdir()
system(paste("mkdir -p", path_base))

if (MAKE_FIGS) {
  path_pdf  <- paste0(path_base, "/pdf")
  path_png  <- paste0(path_base, "/png")

  system(paste("mkdir -p", path_pdf))
  system(paste("mkdir -p", path_png))

  # Put overview in that folder
  df <- j_ls()
  system(paste0("echo \"", paste(df$type, collapse = ", "), ".\" > ", path_pdf, "/README-pdf.txt"))
  system(paste0("echo \"", paste(df$type, collapse = ", "), ".\" > ", path_png, "/README-png.txt"))
  # Put all PDFs in /pdf
  if (nrow(df)) for (j in (1:nrow(df))[]) { #[10:11]
    print(paste(j, "of", nrow(df)))
    j_row <- df[j, ]
    if (is.element(j_get(index = j_row$index, what = "fig")$specs$use, USE_OPTIONS)) {
      pdf_file_name <- paste0(j_row$type, ".pdf")
      pdf_path_abs <- paste0(path_pdf, "/", pdf_file_name)
      create_pdf(index = j_row$index, file_name = pdf_path_abs, include_fonts = FALSE)
  
      # Convert to PNG
      png_file_name <- paste0(j_row$type, ".png")
      png_path_abs <- paste0(path_png, "/", png_file_name)
      system(paste("sips -s format png", pdf_path_abs, "--out", png_path_abs))      
    }
  }  
}

if (DATA_FILE) {
  file_name <- paste0(path_base, "/CEP2018-data-figuren.xlsx")#if (REMOTE) paste0(path_base, "/CEP2018-data-figuren.xlsx") else "/private/tmp/CEP2018-data-figuren.xlsx"
  doc_name = "Centraal Economisch Plan 2018"; publication_date = "22 maart 2018"; sort_manually = NULL
  export_xlsx(file_name, doc_name = "Centraal Economisch Plan 2018", publication_date = "22 maart 2018")#, sort_manually = c(2:5,12:20, 1, 6:11))
  system(paste("open", file_name))
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#