fit_fixed <- function(...) {
  lunarcast(sapi, list(adha = holiday_dates("idul_adha")),
            window = c(-3, 7), effect = "two-level",
            trend = "stochastic", season = "none", ...)
}

test_that("lunarcast fits and forecasts the sapi data", {
  skip_on_cran()
  fit <- fit_fixed()
  expect_s3_class(fit, "lunarcast")
  expect_equal(nrow(fit$effects$adha), 23)
  fc <- forecast(fit, h = 12)
  expect_length(fc$mean, 12)
  expect_gt(fc$mean[6], 1.5 * fc$mean[3])     # May 2026 (Eid al-Adha) vs Feb
  expect_equal(fc$future_effects$adha$event, as.Date("2026-05-27"))
  sc <- forecast(fit, h = 12, level2 = list(adha = log(2)))
  expect_equal(sc$future_effects$adha$ratio, 2)
  eff <- holiday_effects(fit)
  expect_equal(nrow(eff), 23)
  expect_true(all(c("holiday", "type", "event", "alpha", "se", "ratio") %in% names(eff)))
})

test_that("select = 'aicc' with a fixed structure runs", {
  skip_on_cran()
  fit <- fit_fixed(select = "aicc")
  expect_equal(fit$structure$season, "none")
  expect_length(forecast(fit, h = 6)$mean, 6)
})

test_that("a holiday can have several proportional windows", {
  skip_on_cran()
  hol <- list(adha = holiday_dates("idul_adha"))
  fit <- lunarcast(sapi, hol,
                   window = list(adha = list(before = c(-14, -1), holiday = c(0, 3))),
                   effect = "constant", trend = "stochastic", season = "none")
  expect_equal(names(fit$holidays), c("adha_before", "adha_holiday"))
  expect_equal(fit$window$adha_before, c(-14, -1))
  expect_equal(nrow(holiday_effects(fit)), 2)
  fc <- forecast(fit, h = 12)
  expect_length(fc$mean, 12)
  expect_gt(fc$mean[6], fc$mean[3])     # May 2026 contains Eid al-Adha
  expect_error(lunarcast(sapi, hol, window = list(adha = list(c(-14, -1)))),
               "must be named")
})
