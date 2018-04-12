rm(list = ls(all = T))

library(stringr)
library(extrafont)
library(openxlsx)
library(gtools)

source("global.R")
source("james-light.R")
source("james-figures.R")
source("import-xls-functions.R")

doc_name  <- "cep2018"
scenario  <- "2018_03_21_FINAL"
REMOTE    <- T
READ_DATA <- T
MAKE_FIGS <- T
DATA_FILE <- F

input_path_prefix <- if (REMOTE) "/Volumes/James/input/cep2018_" else "~/cpb_data/cpb_figs/input/cep2018/cep2018_"
j_init(file_name = "cep2018_final.james", active_scenario = scenario, active_project = doc_name)

# ##### TODO PBL >>>>>>>>>>>>>>
# input_path_prefix <- "/Users/mdijkstra/Dropbox/cpb/presentation/PBL-SCP/data_pbl/"  # TODO PBL
# doc_name  <- "pbl" # TODO PBL
# scenario  <- "clo" # TODO PBL
#
# j_init(file_name = paste0(input_path_prefix, "pbl.james"), active_scenario = scenario, active_project = doc_name)
# ##### <<<<<<<<<<<< TODO PBL

#j_init(file_name = "cep2018_final.james", active_scenario = scenario, active_project = doc_name)
if (READ_DATA) {
  # file_list <- c("ado", "aek", "fan", "gwm", "jcb", "jhjp", "kst", "mbvk", "mhdv", "mml", "mvk", "pmk", "sptg", "svv", "wbcs")
  
  # ##### ======== TODO PBL
  # file_list <- "james_pbl" # TODO PBL
  # ##### ======== TODO PBL
  
  # file_list <- "svv_laura"
  file_list <- "aek"
  #file_list <- "kst" #kst_20180303
  #file_list <- "gwm_md"
  #file_list <- "AD_mbvk"
  #file_list <- "jhjp"
  # EMAIL paste0(paste0(file_list, "@cpb.nl"), collapse= "; ")
  for (user_file in file_list) { # , 
    print(user_file)
    #file_name = "/Users/mdijkstra/Dropbox/cpb/projects/ramingen_db/cep2018/cep2018_james.xlsx"#"/Users/mdijkstra/Dropbox/cpb/projects/ramingen_db/data-figuren-kCEP2018.xlsx"
    file_name = paste0(input_path_prefix, user_file, ".xlsx")#"/Users/mdijkstra/Dropbox/cpb/projects/ramingen_db/data-figuren-kCEP2018.xlsx"

    if (!validate_xlsx(file_name)) print(file_name) else import_xlsx_as_ts(file_name)
  }
  if (!all(1 == as.numeric(j_ls()$version))) warning("Identieke bestandsnamen (figs)?")
}

if (REMOTE) {
  path_base  <- "/Volumes/James/output"
} else {
  path_base  <- paste0("~/cpb_data/cpb_figs/output/cep2018_", format(Sys.time(), "%Y-%m-%d-%H%M"))#tempdir()
  # ##### ======== TODO PBL
  # path_base <- input_path_prefix # TODO PBL
  # ##### ======== TODO PBL
  system(paste("mkdir -p", path_base))
}

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