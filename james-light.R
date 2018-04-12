#rm(list = ls(all = T))
library(R6)

j_print <- function(txt) cat(txt)

#
## Intended file: JFig.R
#
JFig <- R6Class("JFig",
  public = list(
    initialize = function(specs) {
      if (!missing(specs)) self$specs <- specs
    },
    specs = list()
  )
)

#
## Intended file: JData.R
#
JData <- R6Class("JData",
  public = list(
    initialize = function(data, version, type, scenario, project, doc) {
      self$data     <- data
      self$version  <- version
      self$type     <- type
      self$scenario <- scenario
      self$project  <- project
      self$doc      <- doc
      self$fig      <- JFig$new()
      self$born     <- date()
    },
    data     = NULL,
    version  = NULL,
    type     = NULL,
    scenario = NULL,
    project  = NULL,
    doc      = NULL,
    fig      = NULL,
    born     = NULL  # creation_date
  )
)

#
## Intended file: JRoot.R
#
JRoot <- R6Class("JRoot",
  public = list(
    file_name         = NULL,
    active_project    = "",
    active_scenario   = "",
    data_lst          = list(),
    initialize        = function(file_name, active_scenario, active_project) {
      if (file.exists(file_name)) {
        j_root_file    <- readRDS(file_name)
        self$data_lst  <- j_root_file$data_lst
        self$file_name <- file_name # store file name if successful        
      } else {
        self$file_name <- file_name
        self$save()
      }
      if (!missing(active_scenario)) self$active_scenario <- active_scenario
      if (!missing(active_project))  self$active_project <- active_project
    },
    save = function() {
      saveRDS(self, self$file_name)
    }
  )
)

###
### Data functions
###

#
## Intended file: james-interface.R
#
j_init <- function(file_name = "data.cpb", active_scenario, active_project) {
  .j_root <<- JRoot$new(file_name, active_scenario, active_project)
}

j_ls <- function(type, version, scenario, project, collapsed = TRUE, active_project_scenario_only = TRUE) {
  if (active_project_scenario_only) {
    if (missing(project))  project  <- .j_root$active_project
    if (missing(scenario)) scenario <- .j_root$active_scenario
  }
  
  # Filter on type, version, scenario, project
  df <- data.frame(index = integer(), project = character(), scenario = character(), type = character(), version = character(), "dim|class" = character(), doc = character(), stringsAsFactors = FALSE)
  colnames(df)[6] <- "dim|class" # Fixes dim.class
  for (i in seq_along(.j_root$data_lst)) {
    x <- .j_root$data_lst[[i]]

    if (!missing(type)     && type != x$type)         next
    if (!missing(version)  && version != x$version)   next
    if (!missing(scenario) && scenario != x$scenario) next
    if (!missing(project)  && project != x$project)   next

    # x_info: dim or class
    x_info <- paste(dim(x$data), collapse = "/")
    if ("" == x_info) x_info <- class(x_info)

    j <- 1 + nrow(df)
    df[j, "index"]       <- i
    df[j, "project"]     <- x$project
    df[j, "scenario"]    <- x$scenario
    df[j, "type"]        <- x$type
    df[j, "version"]     <- x$version
    df[j, "dim|class"]   <- x_info
    df[j, "doc"]         <- x$doc
  }

  # If "collapsed", filter most recent version per project/scenario/type
  if (collapsed && 0 < nrow(df)) {
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

j_put <- function(x, type = "", doc = NA, scenario = .j_root$active_scenario, project = .j_root$active_project, ignore_if_duplicate = TRUE, activate_project_scenario = TRUE) {
  # First check if we really want to add x
  x2_object <- j_get(type = type, scenario = scenario, project = project, what = "object")
  add_x     <- !ignore_if_duplicate || is.null(x2_object)
  if (!add_x) add_x <- !identical(x, x2_object$data) || !identical(doc, x2_object$doc)
  
  if (add_x) {
    lst              <- j_ls(active_project_scenario_only = FALSE, collapsed = FALSE)
    index            <- which(type == lst$type & scenario == lst$scenario & project == lst$project)
    version          <- 1 + length(index) # New version
    jdata            <- JData$new(x, version, type, scenario, project, doc) # Create
    if (!is.null(x2_object)) {
      jdata$fig <- x2_object$fig # Re-use fig settings
      jdata$doc <- x2_object$doc # Re-use doc
    }
    .j_root$data_lst <- append(.j_root$data_lst, jdata) # Add    
  }
  
  # Select that project and scenario
  if (activate_project_scenario) {
    .j_root$active_project  <- project
    .j_root$active_scenario <- scenario    
  }
  
  return(invisible(add_x))
}

j_get <- function(index, type, version, scenario = .j_root$active_scenario, project = .j_root$active_project, what = c("data", "fig", "object")) {
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
  } else {
    what   <- what[1]
    object <- .j_root$data_lst[[index]]
    if ("object" == what) return(object)
    if ("data"   == what) return(object$data)
    if ("fig"    == what) return(object$fig)
    stop(paste0("j_get(..., what = '", what, "') is NOT allowed."))
  }
  
  return(if (0 == length(index)) NULL else if (get_fig) .j_root$data_lst[[index]]$fig else .j_root$data_lst[[index]]$data)
}

j_activate <- function(type, version, scenario = .j_root$active_scenario, project = .j_root$active_project, index) {
  if (missing(index)) {
    if (missing(version)) stop("Please specify either 'index' or 'version'.")
    j_table <- j_ls(active_project_scenario_only = FALSE, collapsed = FALSE)
    index   <- which(type == j_table$type & scenario == j_table$scenario & project == j_table$project & version == j_table$version)
  }

  n <- length(.j_root$data_lst)
  .j_root$data_lst <- replace(.j_root$data_lst, c(index, n), c(.j_root$data_lst[n], .j_root$data_lst[index]))
}

j_save <- function() .j_root$save()


# j_init(file_name = "trivial.cpb", active_scenario = DEFAULT_SCENARIO, active_project = "kcep2017")
#
# import_xlsx_as_ts(1, type = "a", project = "pa", scenario = "sa", ignore = F)
# import_xlsx_as_ts(1, type = "a", project = "pa", scenario = "sb", ignore = F)
# import_xlsx_as_ts(2, type = "a")
# import_xlsx_as_ts(2, type = "a", ignore = F)
# import_xlsx_as_ts(1, type = "a", project = "pb", scenario = "sb", ignore = F)
#
# j_ls()































