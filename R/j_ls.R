#' List James' data
#'
#' Equivalent of R's native ls() function.
#'
#' @param type data type (default "")
#' @param version version of data you want to filter on
#' @param scenario scenario filter (defaults to active scenario if filter_active = TRUE)
#' @param project project filter (defaults to active project if filter_active = TRUE)
#' @param collapse show only most recent version of each data type (default FALSE)
#' @param filter_active if project/scenario are missing, list only data in your active project and active scenario (default FALSE)
#'
#' @return data.frame which describes the data in james.env$j_root, given the parameters
#'
#' @seealso \code{\link{j_activate}} to activate another project/scenario
#'
#' @importFrom utils head tail
#' @export

j_ls <- function(type, version, scenario, project, collapse = FALSE, filter_active = FALSE) {
  james_initialise()
  
  if (filter_active) {
    if (missing(project))  project  <- james.env$j_root$project
    if (missing(scenario)) scenario <- james.env$j_root$scenario
  }
  
  # Filter on type, version, scenario, project
  df <- data.frame(index = integer(), project = character(), scenario = character(), type = character(), version = character(), "dim|class" = character(), doc = character(), stringsAsFactors = FALSE)
  colnames(df)[6] <- "dim|class" # Fixes dim.class
  for (i in seq_along(james.env$j_root$data_lst)) {
    x <- james.env$j_root$data_lst[[i]]
    if (!is.element(CLASS$jdata, class(x))) next; # This entry was deleted with j_del()

    if (!missing(type)     && type != x$type)         next
    if (!missing(version)  && version != x$version)   next
    if (!missing(scenario) && scenario != x$scenario) next
    if (!missing(project)  && project != x$project)   next

    # x_info: dim or class
    x_info <- paste(dim(x$data), collapse = "/")
    if ("" == x_info) x_info <- class(x$data)

    j <- 1 + nrow(df)
    df[j, "index"]       <- i
    df[j, "project"]     <- x$project
    df[j, "scenario"]    <- x$scenario
    df[j, "type"]        <- x$type
    df[j, "version"]     <- x$version
    df[j, "dim|class"]   <- x_info
    df[j, "doc"]         <- x$meta$doc
  }

  # If "collapse", filter most recent version per project/scenario/type
  if (collapse && 0 < nrow(df)) {
    filter_cols <- c("project", "scenario", "type")
    pst <- unique(df[, filter_cols])
    for (i in 1:nrow(pst)) {
      index <- NULL
      for (j in 1:nrow(df)) if (all(df[j, filter_cols] == pst[i, ])) index <- c(index, j)
      if (1 < length(index)) df <- df[-head(index, -1), ]
    }
  }
  
  df
}