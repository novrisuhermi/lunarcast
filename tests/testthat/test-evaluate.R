test_that("forecast_accuracy computes the usual measures", {
  a <- forecast_accuracy(c(10, 20), naive = c(12, 18))
  expect_equal(a$RMSE, 2)
  expect_equal(a$MAE, 2)
  expect_equal(a$MAPE, 15)
  expect_error(forecast_accuracy(1:3, c(1, 2, 3)))
})

test_that("rolling_origin flags holiday months and moving years", {
  hol <- holiday_dates("idul_adha")
  r <- rolling_origin(sapi, hol, list(naive = function(y, h) rep(tail(y, 1), h)),
                      first = 2022)
  expect_equal(sort(unique(r$year)), 2022:2025)
  expect_equal(r$month[r$holiday], c(7, 6, 6, 6))
  expect_equal(unique(r$moving[r$year == 2023]), TRUE)
  expect_equal(unique(r$moving[r$year == 2024]), FALSE)
  s <- rolling_summary(r)
  expect_equal(colnames(s), c("all", "holiday", "other", "moving", "stable",
                             "mean_year", "median_year"))
})
