context("Create and import example xlsx")

index <- j_import(j_example_xlsx())
obj <- cbind(2000:2019, 500:519, 100:119)
colnames(obj) <- c(NA, "bbp", "happiness")
obj <- as.data.frame(obj)

test_that("Create xlsx-file in tmp-dir",
  expect_equal(j_get(index[1]), obj)
)