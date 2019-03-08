rm(list = ls())
cpblib::use_cpblib()
library(james)

j_plot_xlsx("james-example.xlsx", meta = list(settings_file = "james-local-settings.xlsx"))