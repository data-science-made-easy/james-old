#' Activate a certain project and scenario
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