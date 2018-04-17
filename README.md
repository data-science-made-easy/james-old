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
