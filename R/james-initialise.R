james.env <- new.env(parent = emptyenv())

james_initialise <- function() if (is.null(james.env$j_root)) j_init()