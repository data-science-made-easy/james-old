#' Activate a certain project and scenario
#'
#' j_ls() lists only data from the active project and scenario. If you change project and/or scenario, you can use this function to activate it/them. Use unique(j_ls(active_project_scenario_only=F)[c("project", "scenario")]) to list all unique combinations of the two.
#'
#' @param type the data type you want to activate
#' @param scenario the scenario you want to activate
#' @param project the project you want to activate
#'
#' @return None
#' 
#' @export

j_activate <- function(type = james.env$j_root$type, scenario = james.env$j_root$scenario, project = james.env$j_root$project) {
  james.env$j_root$type     <- type
  james.env$j_root$scenario <- scenario
  james.env$j_root$project  <- project
}