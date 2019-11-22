rm(list = ls())
cpblib::use_cpblib()
library(james)

j_plot_xlsx("james-example.xlsx")
j_plot_xlsx("james-example.xlsx", meta = list(create_svg = "y"))