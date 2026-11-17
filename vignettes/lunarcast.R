## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 3.6)

## ----data, message = FALSE----------------------------------------------------
library(lunarcast)
plot(sapi, xlab = "", ylab = "Google Trends index",
     main = "Search interest in \"sapi\"")

## ----dates--------------------------------------------------------------------
adha <- holiday_dates("idul_adha")
tail(adha, 4)

## ----other-dates--------------------------------------------------------------
easter_dates(2025:2026, offset = -2)   # Good Friday
my_holiday <- as.Date(c("2024-03-11", "2025-03-29", "2026-03-19"))

## ----window-------------------------------------------------------------------
w <- holiday_window(adha, window = c(-3, 7), y = sapi)
window(w, start = c(2020, 6), end = c(2020, 9))

## ----fit----------------------------------------------------------------------
hol <- list(adha  = holiday_dates("idul_adha"),
            fitri = holiday_dates("idul_fitri"))
fit <- lunarcast(sapi, hol)
fit

## ----effects------------------------------------------------------------------
eff <- holiday_effects(fit)
head(eff, 4)
plot(fit)

## ----forecast-----------------------------------------------------------------
fc <- forecast(fit, h = 13)
plot(fc, include = 60)
fc$future_effects
round(cbind(forecast = fc$mean, lower = fc$lower[, 1],
            upper = fc$upper[, 1]), 1)

## ----scenario-----------------------------------------------------------------
sc <- forecast(fit, h = 13, level2 = list(adha = log(2)))
round(cbind(model = fc$mean, scenario = sc$mean)[5:7, ], 1)

## ----fixed--------------------------------------------------------------------
fast <- lunarcast(sapi, hol,
                  window = list(adha = c(-3, 7), fitri = c(-28, 3)),
                  effect = list(adha = "two-level", fitri = "constant"),
                  trend = "stochastic", season = "none")
fast

## ----classic------------------------------------------------------------------
m6 <- calvar_classic(sapi, adha, trend = "stochastic", season = "stochastic")
m6

## ----rolling, eval = FALSE----------------------------------------------------
#  models <- list(
#    classic_m6 = function(y, h)
#      forecast(calvar_classic(y, adha, "stochastic", "stochastic"), h = h)$mean,
#    lunarcast  = function(y, h) forecast(lunarcast(y, hol), h = h)$mean
#  )
#  r <- rolling_origin(sapi, adha, models, first = 2012)
#  rolling_summary(r)

