#' @export j_plot_xlsx
j_plot_xlsx <- function(file_name, meta = list()) {
  j_import(file_name)
  for (index in 1:nrow(j_ls())) {
    j_plot(index, meta) # Create PDF & PNG
  }
}