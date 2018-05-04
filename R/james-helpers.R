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

#' Indicate whether all values in vec are really char
#' @param vec vector
#' @import utils
#' @keywords internal
is_really_character <- function(vec) {
  if ("character" == class(vec)) {
    return("character" == class(type.convert(vec, as.is = TRUE)))
  } else return(FALSE)
}

#' @keywords internal
as_char_vec <- function(str, sep = SETTINGS$sep) str_trim(unlist(str_split(str, sep)))
as_numeric_vec <- function(str) as.numeric(as_char_vec(str))
as_native_vec <- function(str) {
  vec <- as_char_vec(str)
  if (is_really_character(vec))
    return(vec)
  else
    return(as.numeric(vec))
}

#' keywords internal
#' @param meta list with meta data
strings_to_vectors <- function(meta) {
  for (i in seq_along(meta)) {
    val <- meta[[i]]
    if ("character" == class(val)) {
      meta[[i]] <- as_native_vec(val)
    }
  }
  
  return(meta)
}

#' @keywords internal
combine_lists <- function(high_prio, low_prio) {
  lst <- low_prio
  
  for (i in seq_along(high_prio)) {
    var <- names(high_prio)[i]
    value <- high_prio[[i]]
    
    # Only use NA to overwrite if var was non-existent
    if (is.null(value) || !is.na(value) || is.null(lst[[var]]))
      lst[[var]] <- value
  }
  
  return(lst)
}

#' Helper import to list
#' @keywords internal
df_as_list <- function(df) {
  meta <- list()
  for (i in seq_len(nrow(df))) {
    var_name <- df[i, 1]
    var_value <- as_native_vec(df[i, 2])
    if (!all(is.na(var_value))) meta[[var_name]] <- var_value
  }
  return(meta)
}

#' @keywords internal
df_as_matrix <- function(df) {
  mat <- NULL
  for (i in seq_len(ncol(df))) {
    mat <- cbind(mat, df[, i])
  }
  colnames(mat) <- colnames(df)
  return(mat)
}

#' @keywords internal
is_yes <- function(val) {
  if (is.null(val))
    return(FALSE)
  else
    return(is.element(val, YES))
}






















