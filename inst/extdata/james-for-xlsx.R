rm(list = ls())
if (is.element("cpb_lib", installed.packages())) cpb_lib::use_cpb_lib()
library(james)

j_plot_xlsx("james-example-multiple-tabs.xlsx")