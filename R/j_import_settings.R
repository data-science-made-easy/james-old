#' Import settings
#'
#' Recursively import settings from xlsx-files and add them to meta.
#'
#' @param file_name xlsx-file with settings you want to import
#' @param meta list with meta data
#'
#' @details Gives a lower prio to a deeper import. \code{meta} gets highest prio. All of these come on top of the base settings, which are available in the package. Currently only first tab is imported for each of the files (may change later on).
#'
#' @return list with meta data
#'
#' @import openxlsx stringr
#' @export j_import_settings

j_import_settings <- function(file_name, meta) {
  # Validate xlsx
  if (!missing(file_name)) stopifnot(is_valid_xlsx(file_name))

  # Initialise meta
  if (missing(meta)) meta <- list()

  #
  ## Import
  #
  
  # Import base settings
  base_file_name <- system.file("extdata", "james-settings.xlsx", package = "james")
  meta_base <- df_as_list(openxlsx::read.xlsx(base_file_name, sheet = 1, colNames = FALSE))
  
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




























