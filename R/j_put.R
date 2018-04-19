#' Put data in archive
#'
#' Put data in archive at specified location. Beware
#'
#' @param x data you want to store
#' @param type (choose something useful, e.g. "input")
#' @param doc document your data with plain text or Markdown
#' @param scenario the scenario (default active scenario)
#' @param project project to which x belongs (default active project)
#' @param add_if_duplicate (default FALSE). Prevents storing duplicates, e.g. after re-running your script.
#' @param activate_project_scenario the given project and scenario become active after storing x (default TRUE), which sets defaults for the next time you want to get/put data
#'
#' @return index if successfully added, else NULL. Both invisibly.
#'
#' @seealso \code{\link{j_get}}, \code{\link{j_ls}}, \code{\link{j_init}}
#'
#' @export

j_put <- function(x, type = "", doc = NA, scenario = james.env$j_root$active_scenario, project = james.env$j_root$active_project, add_if_duplicate = TRUE, activate_project_scenario = TRUE) {
  james_initialise()
  
  # First check if we really want to add x
  x2_object <- j_get(type = type, scenario = scenario, project = project, what = "object")
  add_x     <- add_if_duplicate || is.null(x2_object)
  if (!add_x) add_x <- !identical(x, x2_object$data) || !identical(doc, x2_object$doc)
  
  if (add_x) {
    lst              <- j_ls(collapse = FALSE, filter_active = FALSE)
    index            <- which(type == lst$type & scenario == lst$scenario & project == lst$project)
    version          <- 1 + length(index) # New version
    jdata            <- JData$new(x, version, type, scenario, project, doc) # Create
    if (!is.null(x2_object)) {
      jdata$fig <- x2_object$fig # Re-use fig settings
      jdata$doc <- x2_object$doc # Re-use doc
    }
    james.env$j_root$data_lst <- append(james.env$j_root$data_lst, jdata) # Add
    index <- length(james.env$j_root$data_lst)
    
    # Select that project and scenario
    if (activate_project_scenario) {
      james.env$j_root$active_project  <- project
      james.env$j_root$active_scenario <- scenario    
    }
  } else {
    index <- NULL
  }
  
  return(invisible(index))
}