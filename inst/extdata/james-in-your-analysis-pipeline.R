rm(list = ls())
cpblib::use_cpblib()
library(james)

# Create some data
mat <- cbind(2000:2009, random = runif(10), increasing = 1:10)

# Create the plot
j_plot_data(data = mat, meta = list(name = "hello-world", title = "Hello world!", series_type = c("bar--", "line"), hline_bold = 0))

# # Import data
# # You may also first clean James' db with j_clean(). Use j_ls() to list its content.
# index <- j_import("james-example.csv")
#
# # Check import
# j_ls()
# j_get(index)
#
# # Set some meta data
# j_set_meta(index, list(title = "Imported data"))
# j_set_meta(index, list(sub_title = "Just an example", hline_bold = 0))
# j_plot(index)