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
  # Validate xlsx

  # Import from which file?
  if (!file.exists(JAMES_SETTINGS)) {
    file.copy(from = system.file("extdata", JAMES_SETTINGS, package = "james"), to = JAMES_SETTINGS)
    print(paste("James created", JAMES_SETTINGS))
  }

  #
  ## Import
  #
  
  # Import base settings
  stopifnot(is_valid_xlsx(JAMES_SETTINGS))
  meta_base <- df_as_list(openxlsx::read.xlsx(JAMES_SETTINGS, sheet = 1, colNames = TRUE))

  #
  ## Combine
  #
  
  # meta overwrites import
  meta <- combine_lists(high_prio = meta, low_prio = meta_base)

  #
  ## Return
  #  
  return(meta)
}




























