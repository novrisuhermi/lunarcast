# Rolling-origin evaluation stored in inst/extdata/rolling_sapi.csv.
# Origins at the end of 2011, ..., 2024; test years 2012, ..., 2025.
# The full run takes about an hour on a laptop.
library(lunarcast)
adha  <- holiday_dates("idul_adha")
hol   <- list(adha = adha, fitri = holiday_dates("idul_fitri"))
spec  <- calvar_classic_all(window(sapi, end = c(2010, 12)), adha)$spec

classic_models <- lapply(seq_len(6), function(i) {
  force(i)
  function(y, h) forecast(calvar_classic(y, adha, spec$trend[i], spec$season[i]),
                          h = h)$mean
})
names(classic_models) <- paste0("classic_", spec$model)

# The lunarcast variants share the calendar (windows and effect types)
# selected at each origin and differ only in the trend/seasonal structure.
lunarcast_variant <- function(...) {
  function(y, h) {
    base <- lunarcast(y, hol, select = "aicc")
    fit  <- lunarcast(y, hol, window = base$window, effect = base$effect, ...)
    forecast(fit, h = h)$mean
  }
}
lunarcast_models <- list(
  lunarcast_aicc = function(y, h) forecast(lunarcast(y, hol, select = "aicc"), h = h)$mean,
  lunarcast_none = lunarcast_variant(season = "none", select = "aicc"),
  lunarcast      = lunarcast_variant()
)

r <- rolling_origin(sapi, adha, c(classic_models, lunarcast_models), first = 2012)
r$pred <- round(r$pred, 4)
write.csv(r, "inst/extdata/rolling_sapi.csv", row.names = FALSE)
