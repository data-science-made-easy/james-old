#' Import data from xlsx-file
#'
#' j_import() 
#'
#' @param file_name xlsx-file you want to import
#' @param meta meta parameters used to import, e.g. meta = list(project = "A project", scenario = "Test"). Beware these meta parameters overwrite imported meta parameters if they share the same name.
#'
#' @return None
#' 
#' @export

j_import <- function(file_name, meta) {
  stopifnot(is_valid_xlsx(file_name))
  
  sheet_names <- openxlsx::getSheetNames(file_name)
  
  sheet_i_meta <- which(TAB_NAME$meta == sheet_names)
  meta_data  <- if (1 == length(sheet_i_meta)) openxlsx::read.xlsx(file_name, sheet = sheet_i_meta) else NULL
    
  j_put(x, add_if_duplicate = TRUE, activate_project_scenario = TRUE)
}