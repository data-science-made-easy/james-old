#' Export xlsx-data file
#'
#' Optionally with png's inside.
#'
#' @param is_final (Boolean) prepends user (read: dirnames) if FALSE
#'
#' @return data_file_name
#'
#' @export j_snapshot

j_snapshot <- function(is_final = FALSE, create_figs = TRUE, include_figs = TRUE, open_data_file = FALSE, meta = list(), input_file_name = "cepmev.xlsx", snapshot_dir = file.path("snapshot", format(Sys.time(), "%Y%m%d-%H%M")), data_file_name = file.path(snapshot_dir, guess_doc_specs()$file_name), doc_name = guess_doc_specs()$doc_name, publication_date = guess_doc_specs()$publication_date) {
  # CONSTANTS
  output_formats <- c("pdf", "png")

  # INITIALIZE
  low_prio_meta <- if (file.exists(JAMES_SETTINGS_M)) list(settings_file = META$JAMES_SETTINGS_M) else list(settings_file = META$JAMES_SETTINGS)
  meta <- james:::combine_lists(high_prio = meta, low_prio = low_prio_meta)

  # CLEAN DB
  j_clean()

  # READ FILES AND SET SOME VARIABLES PER FIGURE
  print("Importing figures with publish = 'y'...")
  this_path <- list.dirs(path = ".", full.names = F, recursive = F)
  for (i in seq_along(this_path)) {
    meta$j_snapshot_import_path <- this_path[i]
    this_file <- file.path(this_path[i], input_file_name)
    if (file.exists(this_file)) {
      print(paste0("Importing '", this_file, "'..."))
      index <- j_import(this_file, meta = meta, publish_only = TRUE)
      for (j in seq_along(index)) {
        obj   <- j_get(index[j], what = "object")
        obj$meta$create <- "y"
        obj$meta$j_name_dashed <- stringr::str_replace_all(obj$meta$name, "[;,]", "-") # REMOVE chars that should not be in path (; and ,)
        obj$meta$j_name_dashed <- stringr::str_replace_all(obj$meta$j_name_dashed, "[:blank:]", "") # REMOVE blanks
        if (!is_final) { # PREPEND USER (OR DIR-) NAME if not final
          obj$meta$j_name_dashed <- paste0(this_path[i], "_", obj$meta$j_name_dashed)
          obj$meta$name <- paste0(this_path[i], "_", obj$meta$name)
        }
        # obj$meta$name <- obj$meta$j_name_dashed  # USE ONLY DASHED NAME FROM NOW ON
        for (img_path in output_formats) {
          obj$meta[[img_path]]  <- paste0(file.path(snapshot_dir, img_path, obj$meta$j_name_dashed), ".", img_path)
        }
      }
    }
  }

  # CREATE PATHS
  for (img_path in output_formats)
    dir.create(path = file.path(snapshot_dir, img_path), showWarnings = FALSE, recursive = TRUE)

  # CREATE FIGS
  cat('\n')
  if (create_figs) {
    print("Creating figures...")
    tab <- j_ls()
    for (i in tab$index) {
      j_plot(i)
    }
  } else {
    print("NOT Creating figures... (set create_figs = TRUE if you want to do so)")
    include_figs <- FALSE
  }

  # data_file_name = guess_doc_specs()$file_name; doc_name = guess_doc_specs()$doc_name; publication_date = guess_doc_specs()$publication_date; cols = c("index", "name", "title", "dim|class") # TODO <= REMOVE

  # CREATE DATA FILE
  cat('\n')
  print(paste0("Creating ", data_file_name, "..."))
  j_export_xlsx(file_name = data_file_name, doc_name = doc_name, publication_date = publication_date, include_figs = include_figs)
  cat('\n')
  print("Done!")
  
  if (open_data_file) system(paste(if ("unix" == .Platform$OS.type) "open" else "start", data_file_name))
  
  return(data_file_name)
}
