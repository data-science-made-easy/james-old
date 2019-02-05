#' @export j_plot_data

j_plot_data <- function(data, meta = list()) {
  index <- j_put(data)
  meta <- combine_lists(high_prio = combine_lists(high_prio = meta, low_prio = list(name = class(data))), low_prio = j_import_settings(meta = meta))
  j_plot(index, meta)
}
