j_plot_xlsx <- function(file_name) {
  j_import(file_name)
  for (index in 1:nrow(j_ls())) {
    j_plot(index) # Create PDF & PNG
  }
}