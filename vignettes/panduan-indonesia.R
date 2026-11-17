## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 3.6)

## ----setup, message = FALSE---------------------------------------------------
library(lunarcast)
hol <- list(adha  = holiday_dates("idul_adha"),
            fitri = holiday_dates("idul_fitri"))

## -----------------------------------------------------------------------------
fit <- lunarcast(sapi, hol,
                 window = list(adha = c(-3, 7), fitri = c(-28, 3)),
                 effect = list(adha = "two-level", fitri = "constant"),
                 trend = "stochastic", season = "none")
fit

## -----------------------------------------------------------------------------
plot(fit)

## -----------------------------------------------------------------------------
fc <- forecast(fit, h = 13)
plot(fc, include = 60)
round(cbind(ramalan = fc$mean, bawah = fc$lower[, 1], atas = fc$upper[, 1]), 1)

## -----------------------------------------------------------------------------
sc <- forecast(fit, h = 13, level2 = list(adha = log(2)))
round(cbind(model = fc$mean, skenario = sc$mean)[5:7, ], 1)

## -----------------------------------------------------------------------------
m6 <- calvar_classic(sapi, hol$adha, trend = "stochastic", season = "stochastic")
m6

