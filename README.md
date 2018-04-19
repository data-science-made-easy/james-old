# James
Time series data management and visualisation made easy for Excel users and R programmers.

# Get James in R
First download [R](https://cran.r-project.org/) or [RStudio](https://www.rstudio.com).

Open `R` (or `RStudio`) and install `devtools`:
``` R
install.packages("devtools")
```

This makes it easy to install `james` directly from github, using devtools:

``` R
devtools::install_github("data-science-made-easy/james")
```

Now you can load the package:
``` R
library(james)
```

Congratulations, you now have the cutting-edge development version!

# Use James
James has only a few functions, which do most of the work for you:

- `j_ls()` lists the 'working version' your data (details below)
- `j_put()` puts a data set in the storage
- `j_get()` gets a data set from the storage
- `j_save()` stores your data to disc for later use

> Note: This creates a file `<your user name>.james` in your home directory, which saves all your data in RDS-format to disc. Please see the ‘R Internals’ manual for details.

## Easy examples:
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