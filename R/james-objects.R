# R6 objects holding your data
#' @import R6

JData <- R6Class("JData",
  public = list(
    initialize = function(data, version, type, scenario, project, doc) {
      self$data     <- data
      self$version  <- version
      self$type     <- type
      self$scenario <- scenario
      self$project  <- project
      self$meta     <- list(user = Sys.info()[["user"]], born = date(), doc = doc)
    },
    data     = NULL, # The data
    version  = NULL, # Version, type, scenario, project are James-related meta data
    type     = NULL,
    scenario = NULL,
    project  = NULL,
    meta     = NULL  # Data related meta data
  )
)

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