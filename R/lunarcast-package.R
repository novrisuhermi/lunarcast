#' lunarcast: Calendar Variation Models for Time Series with Moving Holidays
#'
#' Moving holidays such as Eid al-Fitr, Eid al-Adha, the Chinese New Year and
#' Easter fall on a different Gregorian date every year, so their effect on
#' monthly or quarterly data moves between periods. lunarcast models these
#' effects with window regressors, allows them to differ between occurrences
#' (a two-level model) and selects the trend and seasonal structure of a
#' regression with ARIMA errors by calendar-aware cross-validation.
#'
#' Main functions:
#' * [lunarcast()]: automatic two-level calendar variation model;
#'   [forecast.lunarcast()] for forecasts and scenarios.
#' * [calvar_classic()] and [calvar_classic_all()]: the classical model with
#'   a holiday-month dummy and a constant effect (six trend and seasonal
#'   structures).
#' * [holiday_dates()], [easter_dates()] and [holiday_window()]: holiday dates
#'   and regressors.
#' * [rolling_origin()], [rolling_summary()] and [forecast_accuracy()]:
#'   evaluation.
#'
#' Start with `vignette("lunarcast")`; the method is explained in
#' `vignette("theory")`.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom forecast auto.arima Arima arimaorder fourier ndiffs nsdiffs
#'   BoxCox InvBoxCox ses holt seasonaldummy
#' @importFrom stats coef vcov residuals fitted lm qnorm var ts tsp frequency
#'   start end window setNames complete.cases mad time sd median
#' @importFrom utils head tail
#' @importFrom graphics plot segments abline points
NULL

#' @importFrom forecast forecast
#' @export
forecast::forecast
