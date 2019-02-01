#' Import settings
#'
#' Import settings from xlsx-file and add them to meta.
#'
#' @param file_name xlsx-file with settings you want to import (if not specified, a file with defaults in package will be imported)
#' @param meta list with meta data
#'
#' @details \code{meta} gets highest prio. Settings in \code{file_name} get lower prio. If you don't specify \code{file_name}, James will use james-settings.xlsx (in current directory). Side effect: if this file is non-existent, James will put a copy of its default version there.
#'
#' @return list with meta data
#'
#' @import openxlsx stringr
#' @export j_import_settings

j_import_settings <- function(file_name, meta = list()) {
  # Validate xlsx
  if (!missing(file_name)) stopifnot(is_valid_xlsx(file_name))

  # Import from which file?
  if (!missing(file_name)) {
    settings_file_name <- file_name
  } else {
    # If non-existent, put copy of james-settings in ${input_dir} so users can easily adapt
    settings_file_name <- paste0(get_param("dir_input", meta, "."), .Platform$file.sep, "james-settings.xlsx")
    if (!file.exists(settings_file_name)) {
      file.copy(from = system.file("extdata", "james-settings.xlsx", package = "james"), to = settings_file_name)
      warning(paste("James: created", settings_file_name, "(#TODO: put such msg in log-file)"))
    }
  }
  

  #
  ## Import
  #
  
  # Import base settings
  meta_base <- df_as_list(openxlsx::read.xlsx(settings_file_name, sheet = 1, colNames = TRUE))
  # Import file_name
    
  # If file contains 'import' field
  
  #
  ## Combine
  #
  
  # nested_import overwrites base settings
  
  # import overwrites nested_import
  
  # meta overwrites import
  meta <- combine_lists(high_prio = meta, low_prio = meta_base)

  #
  ## Return
  #  
  return(meta)
}




























