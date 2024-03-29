# James
Time series data management and visualisation made easy for Excel users and R programmers.

# Geting James in R
First download [RStudio](https://www.rstudio.com) or [R](https://cran.r-project.org/).

Open it, and install the package `devtools` if not yet done before.
``` R
if (!is.element("devtools", installed.packages())) install.packages("devtools", repos = "http://cran.us.r-project.org")
```

Install `james` directly from github and load the package:
``` R
devtools::install_github("data-science-made-easy/james")
```

Congratulations, you now have the cutting-edge development version of `james`!

# Using James

## Getting started
To get an impression of what is possible, you can call `j_start(path = getwd())`:

``` R
setwd('/path/to/your/data') # set working directory
rm(list = ls())             # clean workspace
cpblib::use_cpblib()        # set library path
library(james)              # load James

j_start()                   # generate example files
```

Doing so will generate several files in the specified path (defaults to your working directory):

- `james-example-multiple-tabs.xlsx` with several examples in Excel
- `james-for-xlsx.R` contains example code to make figures of the previous file
- `james-in-your-analysis-pipeline.R` shows how you can connect James to your analysis pipeline in R
- `james-settings.xlsx` has global settings which you may override

> If you don't work in the CPB-environment, you may want to remove `cpblib::use_cpblib()` from the R files (to prevent an error message), or install a mockup of the package:

``` R
devtools::install_github("data-science-made-easy/cpblib")
```

## Details
James has only a few functions, which do most of the work for you:

- `j_ls()` lists the 'working version' of your data (details below)
- `j_put()` puts a data set in the storage
- `j_get()` gets a data set from the storage
- `j_plot()` plots your data (for see examples in `james-in-your-analysis-pipeline.R`, which you may generate with `j_start()`)
- `j_clean()` cleans your in-memory database
- `j_save()` stores your data to disc for later use

> Note: 'j_save()' creates a file `<your user name>.james` in your home directory, which saves all your data in RDS-format to disc. Please see the ‘R Internals’ manual for details.

## Trivial examples:
``` R
index <- j_put("hi", type = "greeting") # Store "hi" as a type of greeting
j_ls() # see it is stored (in memory). If you want: j_save() stores it to disc, too.
j_get(index)  # get it back; the index is unique and persistent
j_put("hello", type = "greeting") # put another greeting
j_get(type = "greeting") # And get the last greeting back
j_ls() # shows all greetings
j_ls(collapse = TRUE) # shows only the most recent one
j_get(type = "greeting", version = 1) # gets the first greeting back
j_put("bye", type = "greeting", scenario = "English words", project = "language courses") # store greeting as part of a project and a scenario
j_put("bread", type = "food", scenario = "English words", project = "language courses") # add another data type to this scenario
j_ls(collapse = TRUE, filter_active = TRUE) # shows most recent versions of data types for active project and scenario
```
