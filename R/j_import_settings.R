#' Import settings
#'
#' Import settings from xlsx-file and add them to meta.
#'
#' @param meta list with meta data
#'
#' @details \code{meta} gets highest prio. Settings in \code{file_name} get lower prio. If you don't specify \code{file_name}, james will use the james-settings.xlsx from its own package.
#'
#' @return list with meta data
#'
#' @import openxlsx stringr
#' @export j_import_settings

j_import_settings <- function(meta = list()) {
  # Load base settings (lowest-level)
  base_settings_file_name <- if (file.exists(JAMES_SETTINGS_M)) JAMES_SETTINGS_M else system.file("extdata", JAMES_SETTINGS_LOCAL, package = "james")
  meta_base <- df_as_list(openxlsx::read.xlsx(base_settings_file_name, sheet = 1, colNames = TRUE))
  
  # check whether user wants to override meta_base with own settings file
  settings_file_name <- get_param(META$settings_file, meta, NULL)
  if (!is.null(settings_file_name)) {
    if (JAMES_SETTINGS_M != settings_file_name) {
      if (!file.exists(settings_file_name)) {
        stop(paste0("settings_file '", settings_file_name, "' does not exist. Please remove or update this parameter in your 'constants' or 'meta' tab."))
      }      
      # file exists, only import if different from JAMES_SETTINGS_M
      stopifnot(is_valid_extension(settings_file_name))

      # user wants to load specific settings
      meta_user <- df_as_list(openxlsx::read.xlsx(settings_file_name, sheet = 1, colNames = TRUE))
      meta_base <- james:::combine_lists(high_prio = meta_user, low_prio = meta_base)
    }
  }

  # meta overwrites import
  meta <- combine_lists(high_prio = meta, low_prio = meta_base)

  return(meta)
}



























