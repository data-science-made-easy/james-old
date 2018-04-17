#' Initialize James
#'
#' Creates or opens a file and a JRoot object holding your data
#'
#' @param file_name File holding your archive. Defaults to <username>.james in your home directory, where <username> is your username
#' @param active_scenario Scenario you want to work on
#' @param active_project Project you want to work on
#'
#' @return JRoot object holding your data. Please assign return value to .j_root, as James assumes all data are globally available in .j_root. See examples.
#'
#' @seealso \code{\link{j_put}}, \code{\link{j_get}}, \code{\link{j_ls}}
#'
#' @examples
#' .j_root <- j_init()
#' j_ls()
#'
#' @export

j_init <- function(file_name = paste0(Sys.getenv("HOME"), .Platform$file.sep, Sys.info()["user"], ".james"), active_scenario, active_project) {
  james.env$j_root <- JRoot$new(file_name, active_scenario, active_project)
}