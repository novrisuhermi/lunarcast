## ----include = FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 3.6)
options(width = 100)

## ----setup, message = FALSE-----------------------------------------------------------------------
library(lunarcast)
adha  <- holiday_dates("idul_adha")
fitri <- holiday_dates("idul_fitri")
train <- window(sapi, end = c(2024, 12))
test  <- window(sapi, start = c(2025, 1))

## ----classic--------------------------------------------------------------------------------------
classic <- calvar_classic_all(train, adha, design = c("dummy", "leadlag", "window"))
classic

## ----window-aicc----------------------------------------------------------------------------------
aicc_w <- reshape(classic$window_aicc[, c("model", "w", "AICc")],
                  idvar = "model", timevar = "w", direction = "wide")
names(aicc_w) <- c("model", paste("w =", c(7, 14, 21, 28)))
aicc_w

## ----lunarcast------------------------------------------------------------------------------------
lc <- lunarcast(train, list(adha = adha, fitri = fitri))
lc

## ----test-----------------------------------------------------------------------------------------
fc_classic <- forecast(classic, h = length(test))
fc_lc      <- forecast(lc, h = length(test))
acc <- do.call(forecast_accuracy,
               c(list(actual = test), fc_classic, list(lunarcast = fc_lc)))
acc[, -1] <- round(acc[, -1], 2)
acc[order(acc$RMSE), ]

## ----test-plot------------------------------------------------------------------------------------
plot(test, ylim = c(0, max(test, fc_lc$mean) * 1.15), lwd = 2, col = "grey40",
     xlab = "", ylab = "Index", main = "2025: observed and forecasts")
lines(fc_classic$m6_dummy$mean, col = "darkorange", lwd = 1.5)
lines(fc_classic$m6_window$mean, col = "darkorange", lwd = 1.5, lty = 2)
lines(fc_lc$mean, col = "royalblue", lwd = 1.5)
legend("topright", c("observed", "classical m6, month dummy",
                     "classical m6, proportional windows", "lunarcast"),
       bty = "n", col = c("grey40", "darkorange", "darkorange", "royalblue"),
       lwd = c(2, 1.5, 1.5, 1.5), lty = c(1, 1, 2, 1))

## ----rolling-read---------------------------------------------------------------------------------
r <- read.csv(system.file("extdata", "rolling_sapi.csv", package = "lunarcast"))
r$model <- factor(r$model, levels = unique(r$model))
s <- rolling_summary(r)
s[order(s[, "all"]), ]

## ----rolling-plot---------------------------------------------------------------------------------
yearly <- tapply((r$pred - r$actual)^2, list(r$year, r$model),
                 function(v) sqrt(mean(v)))
years <- as.numeric(rownames(yearly))
show  <- c("classic_m6", "classic_m6_window", "lunarcast")
plot(years, yearly[, "classic_m6"], type = "b", pch = 19, col = "darkorange",
     ylim = c(0, max(yearly[, show]) * 1.1),
     xlab = "", ylab = "RMSE", main = "RMSE per test year")
lines(years, yearly[, "classic_m6_window"], type = "b", pch = 1, lty = 2,
      col = "darkorange")
lines(years, yearly[, "lunarcast"], type = "b", pch = 19, col = "royalblue")
legend("topleft", c("classical m6, month dummy", "classical m6, proportional windows",
                    "lunarcast"), col = c("darkorange", "darkorange", "royalblue"),
       pch = c(19, 1, 19), lty = c(1, 2, 1), bty = "n")

## ----rolling-2019---------------------------------------------------------------------------------
r19 <- r[r$year >= 2019, ]
r19$model <- droplevels(r19$model)
rolling_summary(r19)[c("classic_m6", "classic_m6_leadlag", "classic_m6_window",
                       "lunarcast"), c("all", "mean_year")]

## ----produce, eval = FALSE------------------------------------------------------------------------
#  hol  <- list(adha = adha, fitri = fitri)
#  spec <- classic$spec[classic$spec$design == "dummy", ]
#  classic_design <- function(design, suffix) {
#    m <- lapply(seq_len(6), function(i) {
#      force(i)
#      function(y, h) forecast(calvar_classic(y, adha, spec$trend[i], spec$season[i],
#                                             design = design), h = h)$mean
#    })
#    setNames(m, paste0("classic_m", 1:6, suffix))
#  }
#  classic_models <- c(classic_design("dummy", ""),
#                      classic_design("leadlag", "_leadlag"),
#                      classic_design("window", "_window"))
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
#    lunarcast_aicc  = function(y, h) forecast(lunarcast(y, hol, select = "aicc"), h = h)$mean,
#    lunarcast_none  = lunarcast_variant(season = "none", select = "aicc"),
#    lunarcast       = lunarcast_variant(),
#    lunarcast_split = function(y, h)
#      forecast(lunarcast(y, hol, window = list(adha = list(before = c(-14, -1),
#                                                          holiday = c(0, 3)))),
#               h = h)$mean
#  )
#  r <- rolling_origin(sapi, adha, c(classic_models, lunarcast_models), first = 2012)
#  rolling_summary(r)

