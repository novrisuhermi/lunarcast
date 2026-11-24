# Rolling-origin evaluation stored in inst/extdata/rolling_sapi.csv.
# Origins at the end of 2011, ..., 2024; test years 2012, ..., 2025.
# The full run takes about an hour and a half on a laptop.
library(lunarcast)
adha  <- holiday_dates("idul_adha")
hol   <- list(adha = adha, fitri = holiday_dates("idul_fitri"))
spec  <- calvar_classic_all(window(sapi, end = c(2010, 12)), adha)$spec

# Classical models: six structures and three calendar designs. With the
# window design the length of the window before the holiday is chosen by
# AICc at every origin.
classic_design <- function(design, suffix) {
  m <- lapply(seq_len(6), function(i) {
    force(i)
    function(y, h) forecast(calvar_classic(y, adha, spec$trend[i], spec$season[i],
                                           design = design), h = h)$mean
  })
  setNames(m, paste0("classic_", spec$model, suffix))
}
classic_models <- c(classic_design("dummy", ""),
                    classic_design("leadlag", "_leadlag"),
                    classic_design("window", "_window"))

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
  lunarcast_aicc  = function(y, h) forecast(lunarcast(y, hol, select = "aicc"), h = h)$mean,
  lunarcast_none  = lunarcast_variant(season = "none", select = "aicc"),
  lunarcast       = lunarcast_variant(),
  # two proportional windows for Eid al-Adha: 14 days before and the holiday
  lunarcast_split = function(y, h)
    forecast(lunarcast(y, hol, window = list(adha = list(before = c(-14, -1),
                                                        holiday = c(0, 3)))),
             h = h)$mean
)

r <- rolling_origin(sapi, adha, c(classic_models, lunarcast_models), first = 2012)
r$pred <- round(r$pred, 4)
write.csv(r, "inst/extdata/rolling_sapi.csv", row.names = FALSE)
