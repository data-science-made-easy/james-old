#' Import data from xlsx-file
#'
#' Import each data tab as a new data frame (can be multivariate time series). You may add a tab 'meta' with meta parameters describing the corresponding data tab. The meta tab should contain a column called 'tab' which holds the name of the corresponding data tab. 'project', 'scenario' and 'type' are other examples. Moreover, it may contain meta data, which describes your figure. Use 'j_example_xlsx()' to get an example file. Remarks: a tab with name 'help' is ignored.
#'
#' @param file_name xlsx-file you want to import
#' @param meta meta parameters used to import, e.g. meta = list(project = "A project", scenario = "Test"). Beware these meta parameters overwrite imported meta parameters if they share the same name.
#' @param add_if_duplicate add data as new version if they are already present (default TRUE)? Set to FALSE prevents storing duplicates, e.g. after re-running your script. Beware, add_if_duplicate overwrites meta, meta overwrites meta tab in xlsx.
#'
#' @return Indices of imported data in j_ls()
#'
#' @seealso \code{\link{j_example_xlsx}} to generate some example data to import; \code{\link{j_put}} adds imported data to your archive
#'
#' @import stringr openxlsx
#' @export

j_import <- function(file_name, meta = list(), add_if_duplicate, publish_only = FALSE, publish_data_only = FALSE) {
  # Validate xlsx
  stopifnot(is_valid_extension(file_name))
  
  is_csv <- "csv" == tolower(tools::file_ext(file_name))
  
  # Let 'add_if_duplicate' overwrite 'meta'
  if (!missing(add_if_duplicate)) meta[[ARGS$add_if_duplicate]] <- add_if_duplicate
  
  sheet_names       <- if (is_csv) NULL else openxlsx::getSheetNames(file_name)  
  sheet_i_constants <- which(TAB_NAME$constants == sheet_names)
  constants         <- if (length(sheet_i_constants)) df_as_list(openxlsx::read.xlsx(file_name, sheet = sheet_i_constants, colNames = TRUE)) else NULL
  sheet_i_meta      <- which(TAB_NAME$meta == sheet_names)
  meta_data         <- if (length(sheet_i_meta)) openxlsx::read.xlsx(file_name, sheet = sheet_i_meta) else NULL

  #
  ## (1) Import data tabs
  #
  import_index = NULL
  for (sheet_i in seq_along(sheet_names)) {
    # Import data tab
    tab <- openxlsx::read.xlsx(file_name, sheet = sheet_i)
    
    # Get right colnames
    tab_column_names        <- openxlsx::read.xlsx(file_name, sheet = sheet_i, colNames = FALSE)[1, ]
    names(tab_column_names) <- NULL
    tab_column_names        <- unlist(tab_column_names)
    
    # Get sheet name
    sheet_name <- sheet_names[sheet_i]
    
    # Skip meta data sheet and skip help sheet
    if (TAB_NAME$meta == sheet_name || TAB_NAME$help == sheet_name || TAB_NAME$constants == sheet_name)
      next;
    
    # Get meta data
    sheet_meta_data_indices <- which(sheet_name == meta_data[[META$tab]])
    
    ## (1a) Import the data for each meta data entry
    for (meta_i in seq_along(sheet_meta_data_indices)) {
    	# Fix col names
  		colnames(tab) <- tab_column_names
      index_NA_colnames <- grep("(^X)(\\d+)($)", colnames(tab))
      if (length(index_NA_colnames)) colnames(tab)[index_NA_colnames] <- NA
      
      # Get meta data and recursively import 'meta' data in 'import' field
      sheet_meta_data <- combine_lists(high_prio = meta_data[sheet_meta_data_indices[meta_i],], low_prio = constants) # fig meta overwrites constants
      sheet_meta_data <- combine_lists(high_prio = meta, low_prio = sheet_meta_data) # arg meta overwrites previous (need info to settings file path now)
      sheet_meta_data <- j_import_settings(meta = sheet_meta_data) # fill with unset global parameters

      # Parse strings to vectors of native values (i.e. numeric where posible)
      sheet_meta_data <- strings_to_vectors(sheet_meta_data, skip_fields = c("tab", "sub_title", "title", "x_lab", "y_lab", "y2_lab"))
      
      # Remove NA's
      sheet_meta_data <- sheet_meta_data[which(!is.na(sheet_meta_data))]

      # TODO HACK: If sheet_meta_data does not contain 'type', initialize 'type' with 'tab'
      if (is.null(sheet_meta_data[[META$type]])) {
        sheet_meta_data[[META$type]] = get_param(META$tab, sheet_meta_data, "")
      }
      
      # arg meta overwrites rest
      sheet_meta_data <- combine_lists(high_prio = meta, low_prio = sheet_meta_data)
            
      # Get add_if_duplicate from meta if present in 'sheet_meta_data', else TRUE
      add_if_duplicate <- get_param("add_if_duplicate", sheet_meta_data, default = TRUE)
      
      # Get project, scenario, type
      project  <- get_param("project", sheet_meta_data, "")
      scenario <- get_param("scenario", sheet_meta_data, "")
      type     <- get_param("type", sheet_meta_data, "")
    
      # DON'T IMPORT IF publish_only and !publish
      import_this <- TRUE
      if (publish_only) { # Skip if we don't want to publish this figure
        if (!is_yes(sheet_meta_data[[META$publish]])) import_this <- FALSE
      }
      if (publish_data_only) { # Skip if we don't want to include this figure's data in the data file
        if (!is_yes(sheet_meta_data[[META$publish_data]])) import_this <- FALSE
      }
        
      # Add data; append index
      if (import_this) {
        index <- j_put(tab, project = project, scenario = scenario, type = type, add_if_duplicate = add_if_duplicate)
      
        if (is.null(sheet_meta_data[[META$name]])) {
          sheet_meta_data[[META$name]] <- get_param(META$tab, sheet_meta_data, "")
          this_version <- j_get(index, what = "object")$version
          if (1 < this_version) sheet_meta_data[[META$name]] <- paste0(sheet_meta_data[[META$name]], "_", this_version)
        }
        if (is.null(sheet_meta_data[[META$pdf]])) {
          sheet_meta_data[[META$pdf]] <- paste0(sheet_meta_data[[META$dir_pdf]], .Platform$file.sep, sheet_meta_data[[META$name]], ".pdf")
        }
        if (is.null(sheet_meta_data[[META$png]])) {
          sheet_meta_data[[META$png]] <- paste0(sheet_meta_data[[META$dir_png]], .Platform$file.sep, sheet_meta_data[[META$name]], ".png")
        }
      
        # Set meta data
        j_set_meta(index, sheet_meta_data)

        # Record resulting index
        import_index <- c(import_index, index)        
      }
    }
    
    # ## (1b) Import the tab also if we _don't_ have a meta data entry
    # if (0 == length(sheet_meta_data_indices)) {
    #   # Get add_if_duplicate from meta if present in 'sheet_meta_data', else TRUE
    #   add_if_duplicate  <- get_param("add_if_duplicate", meta, default = TRUE)
    #
    #   # Get project, scenario, type
    #   project   <- get_param("project", meta, "")
    #   scenario  <- get_param("scenario", meta, "")
    #   type      <- get_param("type", meta, sheet_name)
    #   meta$name <- type
    #
    #   # Fix col names
    #       colnames(tab) <- tab_column_names #stringr::str_replace_all(colnames(tab), "\\.(?![0-9\\.]|$)", " ")
    #   index_NA_colnames <- grep("(^X)(\\d+)($)", colnames(tab))
    #   if (length(index_NA_colnames)) colnames(tab)[index_NA_colnames] <- NA
    #
    #   index <- j_put(tab, project = project, scenario = scenario, type = type, add_if_duplicate = add_if_duplicate)
    #
    #   # Set meta data
    #   j_set_meta(index, j_import_settings(meta = meta))
    #
    #   # Record resulting index
    #   import_index <- c(import_index, index)
    # }
  }
  
  # #
  # ## (2) Import all meta data which don't refer to a data tab
  # #
  # index_meta_only <- which("" == stringr::str_trim(meta_data[[META$tab]]))
  # for (i in seq_along(index_meta_only)) {
  #   index <- j_ls(type = meta$type, scenario = meta$scenario, project = meta$project, collapse = TRUE, filter_active = FALSE)$index # TODO We now have a function to look up index
  #   j_set_meta(index, meta)
  #
  #   # Record resulting index
  #   import_index <- c(import_index, index)
  # }
  
  if (is_csv) {
    # Get project, scenario, type
    tab <- as.matrix(read.csv(file_name, stringsAsFactors = F))
    
    # Fix col headers
    colnames(tab) <- as.vector(read.csv(file_name, header = F, stringsAsFactors = F)[1,])
    
    project   <- get_param("project", meta, "")
    scenario  <- get_param("scenario", meta, "")
    type      <- get_param("type", meta, stringr::str_sub(basename(file_name), end = -2 -nchar(tools::file_ext(file_name))))
    meta$name <- type
    
    # Get add_if_duplicate from meta if present in 'sheet_meta_data', else TRUE
    add_if_duplicate  <- get_param("add_if_duplicate", meta, default = TRUE)
    
    index <- j_put(tab, project = project, scenario = scenario, type = type, add_if_duplicate = add_if_duplicate)

    # Set meta
    j_set_meta(index, j_import_settings(meta = meta))

    # Record resulting index
    import_index <- c(import_index, index)
  }
  
  return(import_index)
}
































