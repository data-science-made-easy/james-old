#' Easy plotting in your standard layout
#'
#' Plot the data at position 'index' in j_ls(), given its meta data
#'
#' @param index id of data you want to plot
#'
#' @return None
#'
#' @export

j_fig <- function(index) {
  x <- j_get(index)
  plot(x)
}