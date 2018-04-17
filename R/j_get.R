#' Get data from archive
#'
#' @param index unique id for your data
#' @param type data type you want to get
#' @param version version you want to get (default is active (c.q. newest) version)
#' @param scenario scenario from which you want to get the data (default is active scenario)
#' @param project project from which you want to get the data (default is active project)
#' @param what data type you want to get is one from c("data", "fig", "object") (default "data")
#'
#' @return data, JFig or JData object (NULL if not present)
#'
#' @seealso \code{\link{j_put}}, \code{\link{j_ls}}, \code{\link{j_init}}
#'
#' @examples
#' j_put(x = 4:12, type = "age", scenario = "Primary school", project = "Education")
#' j_get(type = "age")
#' j_put(x = 3:13, type = "age")
#' j_ls()
#' j_ls(collapsed = FALSE)
#' j_get(index = 1)
#' j_get(type = "age")
#' j_get(type = "age", version = 1, scenario = "Primary school", project = "Education")
#'
#' @importFrom utils head tail
#' @export

j_get <- function(index, type, version, scenario = james.env$j_root$active_scenario, project = james.env$j_root$active_project, what = c("data", "fig", "object")) {
  james_initialise()
  
  if (missing(index)) {
    j_table <- j_ls(active_project_scenario_only = FALSE, collapsed = FALSE)
    if (missing(version)) { # take last
      index <- tail(which(type == j_table$type & scenario == j_table$scenario & project == j_table$project), 1)
    } else {
      index <- which(type == j_table$type & scenario == j_table$scenario & project == j_table$project & version == j_table$version)
    }      
  }
  
  if (0 == length(index)) {
    return(NULL)
  } else if (1 == length(index)) {
    what   <- what[1]
    object <- james.env$j_root$data_lst[[index]]
    if ("object" == what) return(object)
    if ("data"   == what) return(object$data)
    if ("fig"    == what) return(object$fig)
    stop(paste0("j_get(..., what = '", what, "') is NOT allowed."))
  } else stop(paste0("j_get cannot return > 1 result. Please narrow your search."))
}
