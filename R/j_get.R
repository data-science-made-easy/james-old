#' Get data from archive
#'
#' Get data from archive. This can be (i) data, (ii) meta data or (iii) object holding both.
#'
#' @param index unique id of your data in j_ls()
#' @param type data type you want to get
#' @param version version you want to get (default is active (c.q. newest) version)
#' @param scenario scenario from which you want to get the data (default is active scenario)
#' @param project project from which you want to get the data (default is active project)
#' @param what data type you want to get is one from c("data", "meta", "object") (default "data")
#'
#' @return data, or list with meta data, or JData object (NULL if not present)
#'
#' @seealso \code{\link{j_put}}, \code{\link{j_ls}}, \code{\link{j_init}}
#'
#' @examples
#' index <- j_put("hi", type = "greeting") # Store "hi" as a type of greeting
#' j_ls() # see it is stored (in memory). If you want: j_save() stores it to disc, too.
#' j_get(index)  # get it back; the index is unique and persistent
#' j_put("hello", type = "greeting") # put another greeting
#' j_get(type = "greeting") # And get the last greeting back
#' j_ls() # shows all greetings
#' j_ls(collapse = TRUE) # shows only the most recent one
#' j_get(type = "greeting", version = 1) # gets the first greeting back
#' # Store greeting as part of a project and a scenario:
#' j_put("bye", type = "greeting", scenario = "English words", project = "language courses")
#' # Add another data type to this scenario:
#' j_put("bread", type = "food", scenario = "English words", project = "language courses")
#' # Shows most recent versions of data types for active project and scenario:
#' j_ls(collapse = TRUE, filter_active = TRUE)
#'
#' @importFrom utils head tail
#' @export

j_get <- function(index, type, version, scenario = james.env$j_root$active_scenario, project = james.env$j_root$active_project, what = c("data", "meta", "object")) {
  james_initialise()

  if (missing(index)) { #TODO use j_get_index(type, version, scenario, project)
    if (missing(type)) type <- ""
    j_table <- j_ls(collapse = FALSE, filter_active = FALSE)
    if (missing(version)) { # take last
      line_number <- tail(which(type == j_table$type & scenario == j_table$scenario & project == j_table$project), 1)
    } else {
      line_number <- which(type == j_table$type & scenario == j_table$scenario & project == j_table$project & version == j_table$version)
    }
    
    # Translate line_number in index
    index <- j_table$index[line_number] 
  }
  
  if (0 == length(index)) {
    return(NULL)
  } else if (1 == length(index)) {
    what   <- what[1]
    object <- james.env$j_root$data_lst[[index]]
    if ("data"   == what) return(object$data)
    if ("meta"   == what) return(object$meta)
    if ("object" == what) return(object)
    stop(paste0("j_get(..., what = '", what, "') is NOT allowed."))
  } else stop(paste0("j_get cannot return > 1 result. Please narrow your search."))
}

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

j_get_ts <- function(index, type, version, scenario = james.env$j_root$active_scenario, project = james.env$j_root$active_project) {
  x <- j_get(index = index, type = type, version = version, scenario = scenario, project = project)
  x_ts <- ts(x[, -1], start = x[1, 1], end = tail(x[,1], 1))
  
  return(x_ts)
}




















