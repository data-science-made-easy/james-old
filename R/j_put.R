#' Put data in archive
#'
#' Put data in archive at specified location. TODO: add all meta data with j_put
#'
#' @param x data you want to store
#' @param type (choose something useful, e.g. "input")
#' @param doc document your data with plain text or Markdown
#' @param scenario the scenario (default active scenario)
#' @param project project to which x belongs (default active project)
#' @param add_if_duplicate store if data are already present? Prevents storing duplicates, e.g. after re-running your script (default TRUE)
#'
#' @return index of (added) data. Both invisibly.
#'
#' @seealso \code{\link{j_get}}, \code{\link{j_ls}}, \code{\link{j_init}}
#'
#' @export

j_put <- function(x, type = james.env$j_root$type, scenario = james.env$j_root$scenario, project = james.env$j_root$project, doc = NA, add_if_duplicate = TRUE) {
  james_initialise()
  
  # Fix args
  if (is.null(type)) type = if (!is.null(james.env$j_root$type)) james.env$j_root$type else ""
  if (is.null(scenario)) scenario = if (!is.null(james.env$j_root$scenario)) james.env$j_root$scenario else ""
  if (is.null(project)) project = if (!is.null(james.env$j_root$project)) james.env$j_root$project else ""
    
  # First check if we really want to add x
  x2_object <- j_get(type = type, scenario = scenario, project = project, what = "object")
  add_x     <- add_if_duplicate || is.null(x2_object)
  if (!add_x) add_x <- !identical(x, x2_object$data) || !identical(doc, x2_object$meta$doc)
  
  if (add_x) {
    lst              <- j_ls(collapse = FALSE, filter_active = FALSE)
    index            <- which(type == lst$type & scenario == lst$scenario & project == lst$project)
    version          <- 1 + length(index) # New version
    jdata            <- JData$new(x, version, type, scenario, project, doc) # Create
    # TODO What here? if (!is.null(x2_object)) jdata$meta <- x2_object$meta # Re-use meta
    james.env$j_root$data_lst <- append(james.env$j_root$data_lst, jdata) # Add
    index <- length(james.env$j_root$data_lst)
    
    # Select that project and scenario
    james.env$j_root$type     <- type
    james.env$j_root$scenario <- scenario    
    james.env$j_root$project  <- project
  } else {
    index <- j_get_index(type = type, scenario = scenario, project = project)
  }
  
  return(invisible(index))
}

#type = ""; doc = NA; scenario = james.env$j_root$scenario; project = james.env$j_root$project; add_if_duplicate = TRUE