#' Remove object that was last put
#'
#' Last line in j_ls() will be removed. No undo possible.
#'
#' @return None
#' 
#' @export

j_rm_last <- function() {
  james.env$j_root$data_lst <- james.env$j_root$data_lst[-length(james.env$j_root$data_lst)]
}