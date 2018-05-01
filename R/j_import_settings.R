#' Import settings
#'
#'
#' @param file_name xlsx-file you want to import
#'
#' @return Indices of imported data in j_ls()
#'
#' @import openxlsx
#' @export

j_import_settings <- function(file_name) {
  # Validate xlsx
  stopifnot(is_valid_xlsx(file_name))

}