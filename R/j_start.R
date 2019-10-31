#' @export j_start
j_start <- function(path = getwd()) {
  for (this_file in list.files(system.file("extdata", package = "james"))) {
    dest <- paste0(path, .Platform$file.sep, this_file)
    file.copy(from = system.file("extdata", this_file, package = "james"), to = dest)
    print(paste("Created", dest))
  }  
}
