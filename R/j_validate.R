#' Validate meta data
#'
#' Set meta$warning or meta$error
#'
#' @param index unique id of your data in j_ls()
#'
#' @return non
#'
#' @export
j_validate <- function(index) {
  meta <- j_get_meta(index)
  err <- warn <- NULL
  
  # Check y2
  if (is_yes(meta$y2)) {
    n_left  <- length(meta$y_at)
    n_right <- length(meta$y2_at)
    
    if (!n_left) {
      warn <- c(warn, "Make sure y_at and y2_at have same number of values.")
    } else {
      if (n_right && n_left != n_right) err <- c(err, "y_at and y2_at must have same number of values!")
    }      
  }
  
  j_set_meta(index, fields = list(warning = warn, error = err))
  
  return(invisible(is.null(warn) && is.null(err)))
}

