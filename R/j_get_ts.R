#' Get data as ts object
#'
#' Same as j_get() but with result converted to ts object
#'
#' @param index unique id of your data in j_ls()
#' @param type data type you want to get
#' @param version version you want to get (default is active (c.q. newest) version)
#' @param scenario scenario from which you want to get the data (default is active scenario)
#' @param project project from which you want to get the data (default is active project)
#'
#' @return index
#'
#' @importFrom stats ts
#' @export

j_get_ts <- function(index, type, version, scenario = james.env$j_root$scenario, project = james.env$j_root$project) {
  x <- j_get(index = index, type = type, version = version, scenario = scenario, project = project)
  x_ts <- ts(x[, -1], start = x[1, 1], end = tail(x[,1], 1))
  
  return(x_ts)
}