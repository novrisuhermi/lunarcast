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
