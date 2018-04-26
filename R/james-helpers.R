#' Check if the xlsx file is valid
#'
#' @param file_name xlsx file name you want to check
#'
#' @return Boolean indicating whether xlsx is valid

is_valid_xlsx <- function(file_name) {
  # Validate extension
  valid_ext <- "xlsx" == tolower(tools::file_ext(file_name))
  return(valid_ext)
    
  #TODO Validate ALL(meta-tab -> a tab)
  #TODO Validate ALL(No meta-tab, but project/scenario/type are present -> does data exist in James?)
}

#' Get appropriate parameter value
#'
#' @param name name of parameter
#' @param lst list with parameter/value tuples
#' @param default default value that is returned if 'name' not present in 'lst'
#'
#' @return parameter from 'lst' or if non-present 'default'

get_param <- function(name, lst, default) {
  value <- lst[[name]]
  if (!is.null(value)) return(value) else return(default)
}