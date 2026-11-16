#' Classical calendar variation model
#'
#' Regression with ARIMA errors in which a moving holiday enters as a dummy
#' for the period that contains it and has the same effect in every year:
#' \deqn{Y_t = \beta_0 + \beta_1 t + S_t + \alpha V_t + N_t,}
#' where \eqn{V_t = 1} if period \eqn{t} contains the holiday, \eqn{S_t} is
#' the seasonal component and \eqn{N_t} an ARIMA process. This is the
#' calendar variation model commonly used in applied work and the benchmark
#' for [lunarcast()], which contains it as a special case.
#'
#' @details The six structures of the classical approach combine a trend and
#' a seasonal component:
#' \tabular{lll}{
#'   model \tab trend \tab seasonality \cr
#'   m1 \tab deterministic \tab dummy \cr
#'   m2 \tab deterministic \tab Fourier \cr
#'   m3 \tab stochastic \tab dummy \cr
#'   m4 \tab stochastic \tab Fourier \cr
#'   m5 \tab deterministic \tab stochastic (seasonal ARIMA) \cr
#'   m6 \tab stochastic \tab stochastic (seasonal ARIMA)
#' }
#' "Deterministic" adds the linear trend regressor \eqn{\beta_1 t};
#' "stochastic" leaves the trend to differencing. In both cases the ARIMA
#' orders, including the order of differencing, are chosen by
#' [forecast::auto.arima()]. Seasonal dummies use the last season (December)
#' as reference.
#'
#' @param y Monthly or quarterly `ts`.
#' @param dates `Date` vector of holiday occurrences.
#' @param trend `"deterministic"` or `"stochastic"`.
#' @param season `"dummy"`, `"fourier"` or `"stochastic"`.
#' @param K Number of Fourier pairs for `season = "fourier"` (default 5 for
#'   monthly data).
#' @param lambda Box-Cox parameter; `NULL` (default) keeps the original scale,
#'   so the holiday effect is additive.
#' @param ... Passed to [forecast::auto.arima()].
#' @return An object of class `"calvar_classic"`.
#' @seealso [calvar_classic_all()] for the six structures at once and
#'   [lunarcast()] for the proposed model.
#' @examples
#' m1 <- calvar_classic(sapi, holiday_dates("idul_adha"),
#'                      trend = "deterministic", season = "dummy")
#' m1
#' forecast(m1, h = 12)
#' @export
calvar_classic <- function(y, dates, trend = c("deterministic", "stochastic"),
                           season = c("dummy", "fourier", "stochastic"),
                           K = frequency(y) / 2 - 1, lambda = NULL, ...) {
  trend  <- match.arg(trend)
  season <- match.arg(season)
  if (!frequency(y) %in% c(4, 12)) stop("`y` must be a monthly or quarterly ts.")
  X   <- .classic_X(y, dates, trend, season, K)
  fit <- auto.arima(y, xreg = X, seasonal = season == "stochastic",
                    lambda = lambda, ...)
  structure(list(fit = fit, y = y, dates = dates, trend = trend,
                 season = season, K = K, lambda = lambda),
            class = "calvar_classic")
}

# Regressors of the classical model: trend, seasonal dummies or Fourier terms
# and the holiday dummy V_t; rows n + 1, ..., n + h when h > 0
.classic_X <- function(y, dates, trend, season, K, h = 0) {
  n   <- length(y)
  idx <- if (h == 0) seq_len(n) else n + seq_len(h)
  V   <- .mat(holiday_window(dates, c(0, 0), y, h = h))[idx, 1]
  X   <- NULL
  if (trend == "deterministic") X <- cbind(X, trend = idx)
  if (season == "dummy")
    X <- cbind(X, .mat(if (h == 0) seasonaldummy(y) else seasonaldummy(y, h)))
  if (season == "fourier")
    X <- cbind(X, .mat(if (h == 0) fourier(y, K = K) else fourier(y, K = K, h = h)))
  cbind(X, holiday = as.numeric(V > 0))
}

.classic_label <- function(trend, season, K) {
  paste0(trend, " trend + ", switch(season,
    dummy      = "dummy seasonality",
    fourier    = paste0("Fourier seasonality (K = ", K, ")"),
    stochastic = "stochastic seasonality"))
}

.classic_effect <- function(fit) {
  c(coef(fit)["holiday"], sqrt(diag(vcov(fit)))["holiday"])
}

#' @export
print.calvar_classic <- function(x, ...) {
  e <- .classic_effect(x$fit)
  cat("Classical calendar variation model:", .classic_label(x$trend, x$season, x$K), "\n")
  cat(as.character(x$fit), "| AICc =", round(x$fit$aicc, 1), "\n")
  cat("Holiday effect (same every year): ", format(e[1], digits = 4), " (s.e. ",
      format(e[2], digits = 3), ")",
      if (is.null(x$lambda)) ", original scale" else ", transformed scale", "\n",
      sep = "")
  invisible(x)
}

#' The six classical calendar variation models
#'
#' Fits the six combinations of trend (deterministic or stochastic) and
#' seasonality (dummy, Fourier or stochastic) of [calvar_classic()], named
#' `m1` to `m6` as in its table.
#'
#' @inheritParams calvar_classic
#' @return An object of class `"calvar_classic_set"`: a list with the fitted
#'   models (`models`, named `m1` to `m6`) and their specification (`spec`).
#' @examples
#' \donttest{
#' set <- calvar_classic_all(sapi, holiday_dates("idul_adha"))
#' set
#' fc <- forecast(set, h = 12)
#' names(fc)
#' }
#' @export
calvar_classic_all <- function(y, dates, K = frequency(y) / 2 - 1,
                               lambda = NULL, ...) {
  spec <- data.frame(
    model  = paste0("m", 1:6),
    trend  = c("deterministic", "deterministic", "stochastic", "stochastic",
               "deterministic", "stochastic"),
    season = c("dummy", "fourier", "dummy", "fourier", "stochastic",
               "stochastic"))
  models <- lapply(seq_len(6), function(i)
    calvar_classic(y, dates, spec$trend[i], spec$season[i], K = K,
                   lambda = lambda, ...))
  names(models) <- spec$model
  structure(list(models = models, spec = spec), class = "calvar_classic_set")
}

#' @export
print.calvar_classic_set <- function(x, ...) {
  tab <- do.call(rbind, lapply(names(x$models), function(nm) {
    m <- x$models[[nm]]
    e <- .classic_effect(m$fit)
    data.frame(model = nm, structure = .classic_label(m$trend, m$season, m$K),
               ARIMA = sub(" errors", "", sub("Regression with ", "",
                                               as.character(m$fit))),
               effect = unname(e[1]), se = unname(e[2]), AICc = m$fit$aicc)
  }))
  cat("Six classical calendar variation models\n")
  print(tab, digits = 4, row.names = FALSE)
  cat("Note: AICc is comparable only between models with the same order of",
      "differencing.\n")
  invisible(x)
}

#' Forecasts from the classical calendar variation model
#'
#' Builds the future trend, seasonal and holiday regressors and forecasts
#' with the fitted regression with ARIMA errors.
#'
#' @param object A `calvar_classic` or `calvar_classic_set` object.
#' @param h Forecast horizon (periods).
#' @param level Confidence levels (percent).
#' @param ... Passed to [forecast::forecast()].
#' @return A `forecast` object, or a named list of `forecast` objects for a
#'   `calvar_classic_set`.
#' @examples
#' m6 <- calvar_classic(sapi, holiday_dates("idul_adha"),
#'                      trend = "stochastic", season = "stochastic")
#' forecast(m6, h = 12)
#' @method forecast calvar_classic
#' @export
forecast.calvar_classic <- function(object, h = 2 * frequency(object$y),
                                    level = c(80, 95), ...) {
  Xf <- .classic_X(object$y, object$dates, object$trend, object$season,
                   object$K, h)
  forecast(object$fit, xreg = Xf, level = level, ...)
}

#' @rdname forecast.calvar_classic
#' @method forecast calvar_classic_set
#' @export
forecast.calvar_classic_set <- function(object,
                                        h = 2 * frequency(object$models[[1]]$y),
                                        level = c(80, 95), ...) {
  lapply(object$models, forecast, h = h, level = level, ...)
}
