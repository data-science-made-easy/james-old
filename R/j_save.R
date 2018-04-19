#' Save data to file
#'
#' Save data you have put in archive to file that was specified in j_init()
#'
#' @return None
#'
#' @seealso \code{\link{j_init}} to initialise James. See \code{\link{j_put}} and \code{\link{j_get}} to get/put data
#'
#' @export

j_save <- function() james.env$j_root$save()