#' Create example xlsx file
#'
#' You can use this example to import, analyse, and visualise your own data
#'
#' @param file_name where your xlsx-file should be stored (default temporary xlsx-file)
#' @param multiple_tabs creates an xlsx-file with multiple tabs if TRUE (default), including a meta tab. If FALSE, it creates an xlsx-file with only one tab
#'
#' @return Path to your example xlsx
#'
#' @seealso \code{\link{j_import}} to import the example data
#' 
#' @export

j_example_xlsx <- function(file_name = paste0(tempfile(), ".xlsx"), multiple_tabs = TRUE) {
  if (multiple_tabs) {    
    success <- file.copy(from = system.file("extdata", "james_example_multiple_tabs.xlsx", package = "james"), to = file_name)
  } else {
    success <- file.copy(from = system.file("extdata", "james_example_one_tab.xlsx", package = "james"), to = file_name)
  }
  
  if (success) return(file_name) else return(NULL)
}