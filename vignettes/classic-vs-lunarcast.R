## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 3.6)

## ----setup, message = FALSE---------------------------------------------------
library(lunarcast)
adha  <- holiday_dates("idul_adha")
fitri <- holiday_dates("idul_fitri")
train <- window(sapi, end = c(2024, 12))
test  <- window(sapi, start = c(2025, 1))

## ----classic------------------------------------------------------------------
classic <- calvar_classic_all(train, adha)
classic

## ----lunarcast----------------------------------------------------------------
lc <- lunarcast(train, list(adha = adha, fitri = fitri))
lc

## ----test---------------------------------------------------------------------
fc_classic <- forecast(classic, h = length(test))
fc_lc      <- forecast(lc, h = length(test))
acc <- do.call(forecast_accuracy,
               c(list(actual = test), fc_classic, list(lunarcast = fc_lc)))
acc[, -1] <- round(acc[, -1], 2)
acc

## ----test-plot----------------------------------------------------------------
plot(test, ylim = c(0, max(test, fc_lc$mean) * 1.15), lwd = 2, col = "grey40",
     xlab = "", ylab = "Index", main = "2025: observed and forecasts")
lines(fc_classic$m6$mean, col = "darkorange", lwd = 1.5)
lines(fc_lc$mean, col = "royalblue", lwd = 1.5)
legend("topright", c("observed", "classical m6", "lunarcast"), bty = "n",
       col = c("grey40", "darkorange", "royalblue"), lwd = c(2, 1.5, 1.5))

## ----rolling-read-------------------------------------------------------------
r <- read.csv(system.file("extdata", "rolling_sapi.csv", package = "lunarcast"))
r$model <- factor(r$model, levels = unique(r$model))
rolling_summary(r)

## ----rolling-plot-------------------------------------------------------------
yearly <- tapply((r$pred - r$actual)^2, list(r$year, r$model),
                 function(v) sqrt(mean(v)))
years <- as.numeric(rownames(yearly))
plot(years, yearly[, "classic_m6"], type = "b", pch = 19, col = "darkorange",
     ylim = c(0, max(yearly[, c("classic_m6", "lunarcast")]) * 1.1),
     xlab = "", ylab = "RMSE", main = "RMSE per test year")
lines(years, yearly[, "lunarcast"], type = "b", pch = 19, col = "royalblue")
legend("topleft", c("classical m6", "lunarcast"), col = c("darkorange", "royalblue"),
       pch = 19, bty = "n")

## ----produce, eval = FALSE----------------------------------------------------
#  hol  <- list(adha = adha, fitri = fitri)
#  spec <- classic$spec
#  classic_models <- lapply(seq_len(6), function(i) {
#    force(i)
#    function(y, h) forecast(calvar_classic(y, adha, spec$trend[i], spec$season[i]),
#                            h = h)$mean
#  })
#  names(classic_models) <- paste0("classic_", spec$model)
#  
#  # the lunarcast variants share the calendar selected at each origin
#  lunarcast_variant <- function(...) {
#    function(y, h) {
#      base <- lunarcast(y, hol, select = "aicc")
#      fit  <- lunarcast(y, hol, window = base$window, effect = base$effect, ...)
#      forecast(fit, h = h)$mean
#    }
#  }
#  lunarcast_models <- list(
#    lunarcast_aicc = function(y, h) forecast(lunarcast(y, hol, select = "aicc"), h = h)$mean,
#    lunarcast_none = lunarcast_variant(season = "none", select = "aicc"),
#    lunarcast      = lunarcast_variant()
#  )
#  r <- rolling_origin(sapi, adha, c(classic_models, lunarcast_models), first = 2012)
#  rolling_summary(r)

