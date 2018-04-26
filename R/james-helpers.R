is_valid_xlsx <- function(file_name) {
  "xlsx" == tolower(tools::file_ext(file_name))
}