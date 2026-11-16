y <- ts(1:36, start = c(2020, 1), frequency = 12)
d <- as.Date(c("2020-07-31", "2021-07-20", "2022-07-10"))

test_that("window c(0, 0) reproduces the monthly dummy", {
  expect_equal(which(holiday_window(d, c(0, 0), y) == 1), c(7, 19, 31))
})

test_that("a fully observed window sums to one and splits by days", {
  W <- holiday_window(d[1], c(-3, 7), y, per_event = TRUE)
  expect_equal(sum(W), 1)
  expect_equal(round(as.numeric(W[7:8]), 3), c(0.364, 0.636))
})

test_that("future periods and quarterly series work", {
  expect_equal(nrow(holiday_window(d, c(0, 0), y, h = 12)), 48)
  yq <- ts(1:12, start = c(2020, 1), frequency = 4)
  expect_equal(which(holiday_window(d, c(0, 0), yq) == 1), c(3, 7, 11))
})

test_that("log-scale remapping keeps 0 and 1 and is monotone", {
  W <- matrix(c(0, 0.25, 0.5, 1), ncol = 1)
  r <- lunarcast:::.remap(W, log(3))
  expect_equal(r[c(1, 4)], c(0, 1))
  expect_true(all(diff(r) > 0))
})
