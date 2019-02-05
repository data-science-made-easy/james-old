rm(list = ls())
if (is.element("cpb_lib", installed.packages())) cpb_lib::use_cpb_lib()
library(james)

# Create some data
mat <- cbind(2000:2009, random = runif(10), increasing = 1:10)

# Create the plot
j_plot_data(data = mat, meta = list(name = "hello-world", title = "Hello world!", hline_bold = 0))