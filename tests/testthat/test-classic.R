test_that("classical regressors match the holiday months", {
  y <- window(sapi, end = c(2010, 12))
  X <- lunarcast:::.classic_X(y, holiday_dates("idul_adha"),
                              "deterministic", "dummy", 5)
  expect_equal(colnames(X)[1], "trend")
  expect_equal(ncol(X), 1 + 11 + 1)
  months <- format(as.Date(paste0(floor(time(y)), "-", cycle(y), "-01"))[X[, "holiday"] == 1], "%Y-%m")
  expect_true(all(c("2006-01", "2006-12", "2010-11") %in% months))
  Xf <- lunarcast:::.classic_X(y, holiday_dates("idul_adha"), "stochastic",
                               "fourier", 5, h = 12)
  expect_equal(dim(Xf), c(12, 11))
  expect_equal(unname(Xf[11, "holiday"]), 1)  # November 2011
})

test_that("calvar_classic fits and forecasts", {
  skip_on_cran()
  m <- calvar_classic(sapi, holiday_dates("idul_adha"), "stochastic", "dummy")
  expect_s3_class(m, "calvar_classic")
  expect_gt(unname(coef(m$fit)["holiday"]), 0)
  fc <- forecast(m, h = 12)
  expect_length(fc$mean, 12)
  expect_gt(fc$mean[6], fc$mean[3])            # May 2026 contains Eid al-Adha
})
