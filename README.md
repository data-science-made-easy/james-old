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

> Note: This creates a file "<your user name>.james" in your home directory, which saves all your data in RDSformat to disc. Please see the ‘R Internals’ manual for details.

## Easy example:
``` R
j_ls()
```

First time, j_ls() shows an empty table.

    [1] index     project   scenario  type      version   dim|class doc      
      <0 rows> (or 0-length row.names)

Now you can add some data:
``` R
j_put(1:10)
```

The data which you've just added is shown in the overview below.

``` R
j_ls()

      index project scenario type version dim|class  doc
    1     1                             1 character <NA>
```

To observe it again:
``` R
j_get(index = 1)
 [1]  1  2  3  4  5  6  7  8  9 10
```

An alternative to get the data back is:
``` R
j_get(version = 1)
 [1]  1  2  3  4  5  6  7  8  9 10
```

Adding a new version goes automatically:
``` R
j_put(101:110)

j_ls()

      index project scenario type version dim|class  doc
    2     2                             2 character <NA>
```

However, the first version stays available, too:
``` R
j_get(index = 1)
 [1]  1  2  3  4  5  6  7  8  9 10
```
