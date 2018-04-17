# James
Time series data management and visualisation made easy for Excel users and R programmers.

# Get James in R

``` R
devtools::install_github("data-science-made-easy/james")
```

# Use James
James has only a few functions, which do most of the work for you.
## Easy example:
```
library(james)
j_ls()
```

This creates a file <your user name>.james in your home directory. First time, j_ls() shows an empty table.

    [1] index     project   scenario  type      version   dim|class doc      
      <0 rows> (or 0-length row.names)

Now you can add some data:
``` R
j_put(1:10)
j_ls()
```

      index project scenario type version dim|class  doc
    1     1                             1 character <NA>