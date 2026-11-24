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

test_that("lead-lag dummies mark the periods around the holiday", {
  y <- window(sapi, end = c(2010, 12))
  X <- lunarcast:::.classic_cal(y, holiday_dates("idul_adha"), "leadlag", NULL)
  ym <- format(as.Date(paste0(floor(time(y)), "-", cycle(y), "-01")), "%Y-%m")
  expect_equal(colnames(X), c("before", "holiday", "after"))
  expect_true(all(c("2005-12", "2006-11", "2010-10") %in% ym[X[, "before"] == 1]))
  expect_true(all(c("2006-01", "2006-12", "2010-11") %in% ym[X[, "holiday"] == 1]))
  expect_true(all(c("2006-02", "2007-01", "2010-12") %in% ym[X[, "after"] == 1]))
})

test_that("proportional windows are the shares of the window days", {
  y <- window(sapi, end = c(2021, 12))
  X <- lunarcast:::.classic_cal(y, holiday_dates("idul_adha"), "window",
                                list(before = c(-14, -1), holiday = c(0, 3)))
  at <- function(yr, mo) (yr - 2004) * 12 + mo
  # Eid al-Adha on 11 August 2019: the 14-day window before it runs from
  # 28 July to 10 August, 4 days in July and 10 in August
  expect_equal(unname(X[at(2019, 7), "before"]), 4 / 14)
  expect_equal(unname(X[at(2019, 8), "before"]), 10 / 14)
  # 31 July 2020: the holiday window (31 July to 3 August) is 1/4 July, 3/4 August
  expect_equal(unname(X[at(2020, 7), "holiday"]), 0.25)
  expect_equal(unname(X[at(2020, 8), "holiday"]), 0.75)
  # every complete occurrence contributes one window in total
  expect_equal(sum(X[at(2010, 1):at(2020, 12), "holiday"]), 11)
})

test_that("calvar_classic supports the three calendar designs", {
  skip_on_cran()
  hol <- holiday_dates("idul_adha")
  y <- window(sapi, end = c(2015, 12))
  m2 <- calvar_classic(y, hol, "stochastic", "dummy", design = "leadlag")
  expect_true(all(c("before", "holiday", "after") %in% names(coef(m2$fit))))
  expect_equal(dim(lunarcast:::.classic_X(y, hol, "stochastic", "dummy", 5, h = 6,
                                          design = "leadlag")), c(6, 14))
  m3 <- calvar_classic(y, hol, "stochastic", "dummy", design = "window",
                       window = list(before = "auto", holiday = c(0, 3)),
                       window_grid = c(7, 14))
  expect_true(m3$window$before[1] %in% c(-7, -14))
  expect_equal(m3$window$before[2], -1)
  expect_equal(nrow(m3$window_aicc), 2)
  expect_equal(m3$fit$aicc, min(m3$window_aicc$AICc))
  fc <- forecast(m3, h = 12)
  expect_length(fc$mean, 12)
  expect_error(calvar_classic(y, hol, design = "window", window = list(c(0, 3))),
               "named list")
})
