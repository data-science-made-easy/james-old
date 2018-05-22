#' James package version
#'
#' @return package version as string
#'
#' @export

j_version <- function() {
  as.character(packageVersion("james"))
}