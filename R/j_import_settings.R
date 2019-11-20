#' Import settings
#'
#' Import settings from xlsx-file and add them to meta.
#'
#' @param meta list with meta data
#'
#' @details \code{meta} gets highest prio. Settings in \code{file_name} get lower prio. If you don't specify \code{file_name}, James will use james-settings.xlsx (in current directory). Side effect: if this file is non-existent, James will put a copy of its default version there.
#'
#' @return list with meta data
#'
#' @import openxlsx stringr
#' @export j_import_settings

j_import_settings <- function(meta = list()) {
  settings_file_name <- get_param(META$settings_file, meta, JAMES_SETTINGS_LOCAL)
  
  # Import from which file?
  if (!file.exists(settings_file_name)) {
    # Check writability
    if (-1 == file.access(dirname(settings_file_name), mode = 2)) {
      stop(paste("No write permission for", settings_file_name))
    } else {
      file.copy(from = system.file("extdata", JAMES_SETTINGS_LOCAL, package = "james"), to = settings_file_name)
      print(paste("James created", settings_file_name))      
    }
  }

  # Validate xlsx
  stopifnot(is_valid_extension(settings_file_name))
  meta_base <- df_as_list(openxlsx::read.xlsx(settings_file_name, sheet = 1, colNames = TRUE))
  # meta overwrites import
  meta <- combine_lists(high_prio = meta, low_prio = meta_base)
  return(meta)
}




























