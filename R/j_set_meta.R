#' Set meta data
#'
#' Set meta data for figure
#'
#' @param index unique id of your data in j_ls()
#' @param fields list with fields you want to add or edit
#'
#' @return None
#'
#' @export

j_set_meta <- function(index, fields) {
  object = j_get(index, what = "object")
  for (i in 1:length(fields)) {
    object$meta[[names(fields)[i]]] <- fields[[i]]
  }
}