% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/j_get.R
\name{j_get}
\alias{j_get}
\title{Get data from archive}
\usage{
j_get(index, type, version, scenario = james.env$j_root$scenario,
  project = james.env$j_root$project, what = c("data", "meta",
  "object"))
}
\arguments{
\item{index}{unique id of your data in j_ls()}

\item{type}{data type you want to get}

\item{version}{version you want to get (default is active (c.q. newest) version)}

\item{scenario}{scenario from which you want to get the data (default is active scenario)}

\item{project}{project from which you want to get the data (default is active project)}

\item{what}{data type you want to get is one from c("data", "meta", "object") (default "data")}
}
\value{
data, or list with meta data, or JData object (NULL if not present)
}
\description{
Get data from archive. This can be (i) data, (ii) meta data or (iii) object holding both.
}
\examples{
index <- j_put("hi", type = "greeting") # Store "hi" as a type of greeting
j_ls() # see it is stored (in memory). If you want: j_save() stores it to disc, too.
j_get(index)  # get it back; the index is unique and persistent
j_put("hello", type = "greeting") # put another greeting
j_get(type = "greeting") # And get the last greeting back
j_ls() # shows all greetings
j_ls(collapse = TRUE) # shows only the most recent one
j_get(type = "greeting", version = 1) # gets the first greeting back
# Store greeting as part of a project and a scenario:
j_put("bye", type = "greeting", scenario = "English words", project = "language courses")
# Add another data type to this scenario:
j_put("bread", type = "food", scenario = "English words", project = "language courses")
# Shows most recent versions of data types for active project and scenario:
j_ls(collapse = TRUE, filter_active = TRUE)

}
\seealso{
\code{\link{j_put}}, \code{\link{j_ls}}, \code{\link{j_init}}
}
