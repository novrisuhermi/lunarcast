#' Calendar-aware rolling-origin evaluation
#'
#' Refits forecasting models at the end of every year and forecasts the
#' following calendar year (monthly series), keeping track of the periods
#' that contain the holiday and of the years in which the holiday moved to a
#' different month than in the previous year ("moving years").
#'
#' @param y Monthly `ts`.
#' @param dates `Date` vector of the holiday used to flag periods.
#' @param models Named list of functions `f(y_train, h)` that return point
#'   forecasts for the next `h` periods.
#' @param first First test year.
#' @return A data frame with columns `year`, `month`, `actual`, `model`,
#'   `pred`, `holiday` (the month contains the holiday) and `moving` (the
#'   holiday fell in a different month than in the previous year).
#' @seealso [rolling_summary()]
#' @examples
#' \donttest{
#' hol <- holiday_dates("idul_adha")
#' models <- list(
#'   classic_m6 = function(y, h)
#'     forecast(calvar_classic(y, hol, "stochastic", "stochastic"), h = h)$mean,
#'   snaive = function(y, h) forecast::snaive(y, h = h)$mean)
#' r <- rolling_origin(sapi, hol, models, first = 2023)
#' rolling_summary(r)
#' }
#' @export
rolling_origin <- function(y, dates, models, first = start(y)[1] + 8) {
  if (frequency(y) != 12) stop("`y` must be monthly.")
  if (is.null(names(models)) || any(names(models) == ""))
    stop("`models` must be a named list of functions.")
  mon <- as.POSIXlt(dates)$mon + 1
  yr  <- as.POSIXlt(dates)$year + 1900
  out <- list()
  for (yy in first:end(y)[1]) {
    ytr <- window(y, end = c(yy - 1, 12))
    yte <- head(as.numeric(window(y, start = c(yy, 1))), 12)
    h   <- length(yte)
    hol <- mon[yr == yy]
    for (nm in names(models)) {
      p <- tryCatch(as.numeric(models[[nm]](ytr, h)),
                    error = function(e) rep(NA_real_, h))
      out[[length(out) + 1]] <- data.frame(
        year = yy, month = seq_len(h), actual = yte, model = nm, pred = p,
        holiday = seq_len(h) %in% hol,
        moving = !setequal(hol, mon[yr == yy - 1]))
    }
  }
  r <- do.call(rbind, out)
  r$model <- factor(r$model, levels = names(models))
  r
}

#' Summarise a rolling-origin evaluation
#'
#' @param r Output of [rolling_origin()] (columns `year`, `model`, `actual`,
#'   `pred`, `holiday`, `moving`).
#' @return A matrix of RMSEs per model: all periods, holiday months, other
#'   months, moving years, other (stable) years and the median of the yearly
#'   RMSEs.
#' @export
rolling_summary <- function(r) {
  if (!is.factor(r$model)) r$model <- factor(r$model, levels = unique(r$model))
  rmse <- function(i) tapply((r$pred - r$actual)[i], r$model[i],
                             function(e) sqrt(mean(e^2, na.rm = TRUE)))
  med <- vapply(levels(r$model), function(m) {
    s <- r[r$model == m, ]
    median(tapply((s$pred - s$actual)^2, s$year,
                  function(v) sqrt(mean(v, na.rm = TRUE))))
  }, numeric(1))
  round(cbind(all = rmse(TRUE), holiday = rmse(r$holiday),
              other = rmse(!r$holiday), moving = rmse(r$moving),
              stable = rmse(!r$moving), median_year = med), 2)
}

#' Forecast accuracy measures
#'
#' @param actual Observed values.
#' @param ... Named point forecasts: numeric vectors or `forecast` objects
#'   (their `mean` is used), each at least as long as `actual`.
#' @return A data frame with one row per forecast and the RMSE, MAE, MAPE and
#'   sMAPE (the last two in percent).
#' @examples
#' forecast_accuracy(c(10, 20, 30), naive = c(12, 18, 33), mean = c(20, 20, 20))
#' @export
forecast_accuracy <- function(actual, ...) {
  fc <- list(...)
  if (!length(fc) || is.null(names(fc)) || any(names(fc) == ""))
    stop("Name every forecast, e.g. forecast_accuracy(y, m1 = fc1).")
  a <- as.numeric(actual)
  out <- lapply(names(fc), function(nm) {
    p <- fc[[nm]]
    p <- as.numeric(if (inherits(p, "forecast")) p$mean else p)[seq_along(a)]
    e <- a - p
    data.frame(model = nm, RMSE = sqrt(mean(e^2)), MAE = mean(abs(e)),
               MAPE = 100 * mean(abs(e / a)),
               sMAPE = 100 * mean(2 * abs(e) / (abs(a) + abs(p))))
  })
  do.call(rbind, out)
}
