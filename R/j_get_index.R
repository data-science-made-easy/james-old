#' Get index
#'
#' Get index of data in j_ls()
#'
#' @param type data type (default "")
#' @param version version of data you want index of (default most recent version only)
#' @param scenario scenario filter (defaults to active scenario)
#' @param project project filter (defaults to active project)
#'
#' @return index 
#' 
#' @export

j_get_index <- function(type, version, scenario = james.env$j_root$active_scenario, project = james.env$j_root$active_project) {
  if (missing(type)) type <- ""
  j_table <- j_ls(collapse = FALSE, filter_active = FALSE)
  if (missing(version)) { # take last
    line_number <- tail(which(type == j_table$type & scenario == j_table$scenario & project == j_table$project), 1)
  } else {
    line_number <- which(type == j_table$type & scenario == j_table$scenario & project == j_table$project & version == j_table$version)
  }
  
  # Translate line_number in index
  index <- j_table$index[line_number] 
  
  index
}