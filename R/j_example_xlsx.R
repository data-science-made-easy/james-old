#' Get example xlsx file
#'
#' You can use this example to import, analyse, and visualise your own data
#'
#' @param file_name file_name where your xlsx-file should be stored (default temporary xlsx-file)
#'
#' @return None
#' 
#' @export

j_example_xlsx <- function(file_name = paste0(tempfile(), ".xlsx")) {
  file.copy(from = system.file("extdata", "james_example.xlsx", package = "james"), to = file_name)
}