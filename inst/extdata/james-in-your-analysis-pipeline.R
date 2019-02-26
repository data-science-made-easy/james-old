rm(list = ls())
cpblib::use_cpblib()
library(james)

# Create some data
mat <- cbind(2000:2009, random = runif(10), increasing = 1:10)

# Create the plot
j_clean()
j_plot_data(data = mat, meta = list(name = "hello-world", title = "Hello world!", series_type = c("bar--", "line"), hline_bold = 0))

# Import data
j_clean()
j_import("james-example.csv")

# Check import
j_ls()
j_get(1)

# Set some meta data
j_set_meta(1, list(title = "Imported data"))
j_set_meta(1, list(sub_title = "Just an example", hline_bold = 0))
j_plot(1)