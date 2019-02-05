#' @export j_start
j_start <- function(path = getwd()) {
  for (this_file in c("james-settings.xlsx", "james-in-your-analysis-pipeline.R", "james-for-xlsx.R", "james-example-multiple-tabs.xlsx")) {
    dest <- paste0(path, .Platform$file.sep, this_file)
    file.copy(from = system.file("extdata", this_file, package = "james"), to = dest)
    print(paste("Created", dest))
  }  
}
