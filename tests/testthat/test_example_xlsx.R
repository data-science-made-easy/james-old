context("Create, import example xlsx, retrieve data")

index <- j_import(j_example_xlsx())
obj <- cbind(2000:2019, 500:519, 100:119)
colnames(obj) <- c(NA, "bbp", "happiness")
obj <- as.data.frame(obj)
obj_ts <- ts(obj[, -1], start = 2e3, end = 2019)

test_that("Create xlsx-file in tmp-dir", {
  expect_equal(j_get(index[1]), obj)
  expect_equal(j_get_ts(index[1]), obj_ts)
})