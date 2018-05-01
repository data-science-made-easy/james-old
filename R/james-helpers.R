#' Check if the xlsx file is valid
#'
#' @param file_name xlsx file name you want to check
#'
#' @keywords internal
#' @return Boolean indicating whether xlsx is valid

is_valid_xlsx <- function(file_name) {
  # Validate extension
  valid_ext <- "xlsx" == tolower(tools::file_ext(file_name))
  return(valid_ext)
    
  #TODO Validate ALL(meta-tab -> a tab)
  #TODO Validate ALL(No meta-tab, but project/scenario/type are present -> does data exist in James?)
}

#' Get appropriate parameter value
#'
#' @param name name of parameter
#' @param lst list with parameter/value tuples
#' @param default default value that is returned if 'name' not present in 'lst'
#'
#' @keywords internal
#' @return parameter from 'lst' or if non-present 'default'

get_param <- function(name, lst, default) {
  value <- lst[[name]]
  if (!is.null(value)) return(value) else return(default)
}

#' Indicate whether all values in vec are really char
#' @param vec vector
#' @import utils
#' @keywords internal
is_really_character <- function(vec) {
  if ("character" == class(vec)) {
    return("character" == class(type.convert(vec, as.is = TRUE)))
  } else return(FALSE)
}

#' Return data as data.frame
#' @param d data to be converted to data frame
#' @import stats
#' @keywords internal
as_data_frame <- function(d) {
  if (is.data.frame(d)) {
    return(d)
  } else if (is.ts(d)) {
    return(cbind(time = as.vector(time(d)), as.data.frame(d)))
  } else { # same story in other cases...
    return(as.data.frame(d))
  }
}

#' Extract x-axis information from d
#' @param d retrieve x-axis
#' @keywords internal
extract_x_axis <- function(d) {
  # TODO Replace \n by newline, dot by comma, etc...
  if (is_really_character(d[, 1])) {
    x_at <- 1:nrow(d)
    x_at_lab <- d[, 1]
  } else {
    x_at <- as.numeric(d[, 1])
    x_at_lab <- NULL
  }
  
  list(x_at = x_at, x_at_lab = x_at_lab)
}