test_that("outlier regressors have the right in-sample and future values", {
  info <- data.frame(name = c("AO2020_05", "LS2020_04", "TC2020_06"),
                     type = c("AO", "LS", "TC"), t = c(5, 4, 6), tstat = 4)
  X <- lunarcast:::.outlier_X(info, 8)
  expect_equal(dim(X), c(8, 3))
  expect_equal(unname(X[, "AO2020_05"]), as.numeric(1:8 == 5))
  expect_equal(unname(X[, "LS2020_04"]), as.numeric(1:8 >= 4))
  expect_equal(unname(X[7, "TC2020_06"]), 0.7)
  F <- lunarcast:::.outlier_X(info, 8, h = 2)
  expect_equal(unname(F[, "AO2020_05"]), c(0, 0))
  expect_equal(unname(F[, "LS2020_04"]), c(1, 1))
})

test_that("an injected additive outlier is detected", {
  skip_on_cran()
  hol <- list(adha = holiday_dates("idul_adha"))
  cb  <- lunarcast:::.cal_fit_X(hol, list(adha = c(-3, 7)),
                                list(adha = "two-level"), sapi)
  z <- log(sapi)
  z[136] <- z[136] + log(2)                   # April 2015 doubled
  out <- lunarcast:::.detect_outliers(z, cb$X, c(1, 1, 1), FALSE,
                                      rowSums(cb$X > 0) > 0)
  expect_true("AO2015_04" %in% out$name)
})
