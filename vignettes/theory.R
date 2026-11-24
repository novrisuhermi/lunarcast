## ----include = FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.width = 7, fig.height = 3.4)

## ----setup, message = FALSE-----------------------------------------------------------------------
library(lunarcast)

## ----drift----------------------------------------------------------------------------------------
adha <- holiday_dates("idul_adha")
day  <- as.numeric(format(adha, "%j"))
year <- as.numeric(format(adha, "%Y")) + day / 366
plot(year, day, pch = 19, col = "darkorange", xlab = "", ylab = "Day of the year",
     main = "Eid al-Adha drifts through the Gregorian year")
abline(h = cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)),
       col = "grey85")

## ----ratio----------------------------------------------------------------------------------------
y     <- as.numeric(sapi)
yr    <- floor(time(sapi) + 1e-6)
hmon  <- holiday_window(adha, c(-3, 7), sapi)[, 1] > 0   # months touched by the holiday
ratio <- tapply(seq_along(y), yr, function(i) max(y[i][hmon[i]]) / median(y[i][!hmon[i]]))
plot(as.numeric(names(ratio)), ratio, type = "b", pch = 19, xlab = "",
     ylab = "Holiday month / median month", main = "The holiday effect changes over time")
abline(h = 1, lty = 3)

## ----window---------------------------------------------------------------------------------------
w <- holiday_window(adha, window = c(-3, 7), y = sapi)
window(w, start = c(2020, 6), end = c(2020, 9))

## ----split, eval = FALSE--------------------------------------------------------------------------
#  lunarcast(sapi, list(adha = adha),
#            window = list(adha = list(before = c(-14, -1), holiday = c(0, 3))))

