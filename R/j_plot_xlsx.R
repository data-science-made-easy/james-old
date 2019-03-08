#' @export j_plot_xlsx
j_plot_xlsx <- function(file_name, meta = list(), clean = TRUE) {
  # Clean James db
  if (clean) j_clean()
  
  # Do magic
  j_import(file_name, meta = meta)
  for (index in 1:nrow(j_ls())) {
    j_plot(index, meta) # Create PDF & PNG
  }
}