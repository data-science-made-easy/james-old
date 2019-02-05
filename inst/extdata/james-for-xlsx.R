rm(list = ls())
if (is.element("cpblib", installed.packages())) cpb_lib::use_cpblib()
library(james)

j_clean()
j_plot_xlsx("james-example-multiple-tabs.xlsx")