#' Classical calendar variation model
#'
#' Regression with ARIMA errors in which a moving holiday has the same effect
#' in every year:
#' \deqn{Y_t = \beta_0 + \beta_1 t + S_t + C_t + N_t,}
#' where \eqn{S_t} is the seasonal component, \eqn{N_t} an ARIMA process and
#' \eqn{C_t} the calendar component given by `design`:
#' \describe{
#'   \item{`"dummy"` (D1)}{\eqn{C_t = \alpha V_t}, with \eqn{V_t = 1} if period
#'     \eqn{t} contains the holiday. This is the calendar variation model
#'     commonly used in applied work.}
#'   \item{`"leadlag"` (D2)}{\eqn{C_t = \alpha_{-1} V_{t+1} + \alpha_0 V_t +
#'     \alpha_{+1} V_{t-1}}: separate dummies for the period before, the period
#'     of and the period after the holiday.}
#'   \item{`"window"` (D3)}{proportional window regressors (Bell and Hillmer,
#'     1983): for every window \eqn{[D + a, D + b]} in `window`, the share of
#'     its days that falls in period \eqn{t}, e.g.
#'     \eqn{C_t = \alpha_{\mathrm{before}}\,\mathrm{PRE}_t(w) +
#'     \alpha_{\mathrm{holiday}}\,\mathrm{HOL}_t}, where \eqn{\mathrm{PRE}_t(w)}
#'     is the share of the \eqn{w} days before the holiday and
#'     \eqn{\mathrm{HOL}_t} the share of the holiday window (by default the
#'     holiday and the three following days, the tasyrik days of Eid
#'     al-Adha).}
#' }
#' [lunarcast()] generalises the window design: its window is chosen
#' automatically and its effect may change from one occurrence to the next.
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
#' With `design = "window"`, a window given as `"auto"` (by default the
#' `before` window) is chosen by AICc among \eqn{[D - w, D - 1]} for \eqn{w}
#' in `window_grid`, with all other choices fixed.
#'
#' @param y Monthly or quarterly `ts`.
#' @param dates `Date` vector of holiday occurrences.
#' @param trend `"deterministic"` or `"stochastic"`.
#' @param season `"dummy"`, `"fourier"` or `"stochastic"`.
#' @param K Number of Fourier pairs for `season = "fourier"` (default 5 for
#'   monthly data).
#' @param lambda Box-Cox parameter; `NULL` (default) keeps the original scale,
#'   so the holiday effect is additive.
#' @param design Calendar design: `"dummy"` (default), `"leadlag"` or
#'   `"window"`.
#' @param window For `design = "window"`: a named list of windows `c(a, b)` in
#'   days relative to the holiday, one regressor (and one effect) per window.
#'   The default, `list(before = "auto", holiday = c(0, 3))`, chooses the
#'   length of the window before the holiday by AICc.
#' @param window_grid Candidate lengths \eqn{w} (days) for a window given as
#'   `"auto"`, which then covers the \eqn{w} days before the holiday.
#' @param ... Passed to [forecast::auto.arima()].
#' @return An object of class `"calvar_classic"`.
#' @references Bell, W. R. and Hillmer, S. C. (1983). Modeling time series
#'   with calendar variation. *Journal of the American Statistical
#'   Association*, 78, 526-534.
#' @seealso [calvar_classic_all()] for the six structures at once and
#'   [lunarcast()] for the proposed model.
#' @examples
#' hol <- holiday_dates("idul_adha")
#' m1 <- calvar_classic(sapi, hol, trend = "deterministic", season = "dummy")
#' m1
#' forecast(m1, h = 12)
#' # lead-lag dummies and proportional windows (14 days before, holiday + 3)
#' calvar_classic(sapi, hol, "stochastic", "dummy", design = "leadlag")
#' calvar_classic(sapi, hol, "stochastic", "dummy", design = "window",
#'                window = list(before = c(-14, -1), holiday = c(0, 3)))
#' @export
calvar_classic <- function(y, dates, trend = c("deterministic", "stochastic"),
                           season = c("dummy", "fourier", "stochastic"),
                           K = frequency(y) / 2 - 1, lambda = NULL,
                           design = c("dummy", "leadlag", "window"),
                           window = list(before = "auto", holiday = c(0, 3)),
                           window_grid = c(7, 14, 21, 28), ...) {
  trend  <- match.arg(trend)
  season <- match.arg(season)
  design <- match.arg(design)
  if (!frequency(y) %in% c(4, 12)) stop("`y` must be a monthly or quarterly ts.")
  window <- .classic_windows(design, window)
  fit_w <- function(win) {
    X <- .classic_X(y, dates, trend, season, K, design = design, window = win)
    auto.arima(y, xreg = X, seasonal = season == "stochastic", lambda = lambda, ...)
  }
  auto <- names(window)[vapply(window, identical, logical(1), "auto")]
  wtab <- NULL
  if (length(auto)) {
    # coordinate-wise choice of every "auto" window among [D - w, D - 1]
    for (nm in auto) window[[nm]] <- c(-window_grid[1], -1)
    for (nm in auto) {
      aicc <- vapply(window_grid, function(w) {
        win <- window
        win[[nm]] <- c(-w, -1)
        tryCatch(fit_w(win)$aicc, error = function(e) NA_real_)
      }, numeric(1))
      window[[nm]] <- c(-window_grid[which.min(aicc)], -1)
      wtab <- rbind(wtab, data.frame(window = nm, w = window_grid, AICc = aicc))
    }
  }
  fit <- fit_w(window)
  structure(list(fit = fit, y = y, dates = dates, trend = trend,
                 season = season, K = K, lambda = lambda, design = design,
                 window = if (design == "window") window else NULL,
                 window_aicc = wtab),
            class = "calvar_classic")
}

# Windows of the "window" design: a named list of c(a, b) or "auto"
.classic_windows <- function(design, window) {
  if (design != "window") return(NULL)
  if (!is.list(window) || is.null(names(window)) || any(names(window) == ""))
    stop("`window` must be a named list, e.g. list(before = c(-14, -1), holiday = c(0, 3)).")
  for (nm in names(window)) {
    w <- window[[nm]]
    if (!identical(w, "auto") && !(is.numeric(w) && length(w) == 2 && w[1] <= w[2]))
      stop("Every window must be \"auto\" or c(a, b) with a <= b (days).")
  }
  window
}

# Period of every holiday occurrence, counted from the start of y (1 = first)
.hol_pos <- function(dates, y) {
  f  <- frequency(y)
  s  <- start(y)
  lt <- as.POSIXlt(dates)
  (lt$year + 1900 - s[1]) * f + (lt$mon %/% (12 / f) + 1 - s[2]) + 1
}

# Calendar regressors of the classical model for rows 1..n + h
.classic_cal <- function(y, dates, design, window, h = 0) {
  N   <- length(y) + h
  ind <- function(p) {
    v <- numeric(N)
    p <- p[p >= 1 & p <= N]
    v[p] <- 1
    v
  }
  switch(design,
    dummy   = cbind(holiday = ind(.hol_pos(dates, y))),
    leadlag = {
      p <- .hol_pos(dates, y)
      cbind(before = ind(p - 1), holiday = ind(p), after = ind(p + 1))
    },
    window  = {
      X <- vapply(names(window), function(nm)
        .mat(holiday_window(dates, window[[nm]], y, h = h))[, 1], numeric(N))
      matrix(X, nrow = N, dimnames = list(NULL, names(window)))
    })
}

# Regressors of the classical model: trend, seasonal dummies or Fourier terms
# and the calendar regressors; rows n + 1, ..., n + h when h > 0
.classic_X <- function(y, dates, trend, season, K, h = 0, design = "dummy",
                       window = NULL) {
  n   <- length(y)
  idx <- if (h == 0) seq_len(n) else n + seq_len(h)
  X   <- NULL
  if (trend == "deterministic") X <- cbind(X, trend = idx)
  if (season == "dummy")
    X <- cbind(X, .mat(if (h == 0) seasonaldummy(y) else seasonaldummy(y, h)))
  if (season == "fourier")
    X <- cbind(X, .mat(if (h == 0) fourier(y, K = K) else fourier(y, K = K, h = h)))
  cbind(X, .classic_cal(y, dates, design, window, h)[idx, , drop = FALSE])
}

.classic_label <- function(trend, season, K) {
  paste0(trend, " trend + ", switch(season,
    dummy      = "dummy seasonality",
    fourier    = paste0("Fourier seasonality (K = ", K, ")"),
    stochastic = "stochastic seasonality"))
}

# "[-14, -1] days", "[0, +3] days"
.fmt_day <- function(d) ifelse(d > 0, paste0("+", d), as.character(d))
.fmt_win <- function(w) paste0("[", .fmt_day(w[1]), ", ", .fmt_day(w[2]), "] days")

.design_label <- function(x) {
  switch(x$design %||% "dummy",
    dummy   = "holiday dummy",
    leadlag = "lead-lag dummies (period before, of and after the holiday)",
    window  = paste0("proportional windows: ", paste(names(x$window),
              vapply(x$window, .fmt_win, ""), collapse = ", ")))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Names of the calendar regressors of a fitted classical model
.classic_terms <- function(x) {
  switch(x$design %||% "dummy",
         dummy = "holiday", leadlag = c("before", "holiday", "after"),
         window = names(x$window))
}

# Calendar coefficients and standard errors
.classic_effect <- function(x) {
  k  <- .classic_terms(x)
  cf <- coef(x$fit)
  v  <- diag(vcov(x$fit))
  data.frame(term = k, estimate = unname(cf[k]),
             se = unname(sqrt(ifelse(v[k] > 0, v[k], NA_real_))))
}

#' @export
print.calvar_classic <- function(x, ...) {
  e <- .classic_effect(x)
  cat("Classical calendar variation model:", .classic_label(x$trend, x$season, x$K), "\n")
  cat("Calendar design:", .design_label(x), "\n")
  cat(as.character(x$fit), "| AICc =", round(x$fit$aicc, 1), "\n")
  cat("Holiday effects (same every year, ",
      if (is.null(x$lambda)) "original scale" else "transformed scale", "):\n",
      sep = "")
  print(e, digits = 4, row.names = FALSE)
  if (!is.null(x$window_aicc)) {
    cat("AICc of the candidate windows:\n")
    print(x$window_aicc, digits = 6, row.names = FALSE)
  }
  invisible(x)
}

#' The six classical calendar variation models
#'
#' Fits the six combinations of trend (deterministic or stochastic) and
#' seasonality (dummy, Fourier or stochastic) of [calvar_classic()], named
#' `m1` to `m6` as in its table, for one or several calendar designs.
#'
#' @inheritParams calvar_classic
#' @param design One or more of `"dummy"`, `"leadlag"` and `"window"`. With
#'   several designs the models are named `m1_dummy`, `m1_leadlag`, ...
#' @details With `design = "window"` and a window given as `"auto"`, every
#'   structure ranks the candidate windows by AICc, and the window chosen by
#'   most structures (the shorter one in a tie) is used for all six, so that
#'   their calendar regressors are the same.
#' @return An object of class `"calvar_classic_set"`: a list with the fitted
#'   models (`models`), their specification (`spec`) and, for an automatic
#'   window, the AICc of every structure and candidate (`window_aicc`).
#' @examples
#' \donttest{
#' set <- calvar_classic_all(sapi, holiday_dates("idul_adha"))
#' set
#' fc <- forecast(set, h = 12)
#' names(fc)
#' # the 18 models of the three calendar designs
#' all18 <- calvar_classic_all(sapi, holiday_dates("idul_adha"),
#'                             design = c("dummy", "leadlag", "window"))
#' }
#' @export
calvar_classic_all <- function(y, dates, K = frequency(y) / 2 - 1,
                               lambda = NULL, design = "dummy",
                               window = list(before = "auto", holiday = c(0, 3)),
                               window_grid = c(7, 14, 21, 28), ...) {
  design <- match.arg(design, c("dummy", "leadlag", "window"), several.ok = TRUE)
  st <- data.frame(
    model  = paste0("m", 1:6),
    trend  = c("deterministic", "deterministic", "stochastic", "stochastic",
               "deterministic", "stochastic"),
    season = c("dummy", "fourier", "dummy", "fourier", "stochastic",
               "stochastic"))
  models <- list()
  wtab <- NULL
  for (d in design) {
    win <- .classic_windows(d, window)
    auto <- names(win)[vapply(win, identical, logical(1), "auto")]
    if (length(auto)) {
      # every structure ranks the candidates; the majority choice is used
      for (nm in auto) win[[nm]] <- c(-window_grid[1], -1)
      for (nm in auto) {
        tab <- do.call(rbind, lapply(seq_len(6), function(i) {
          aicc <- vapply(window_grid, function(w) {
            ww <- win
            ww[[nm]] <- c(-w, -1)
            tryCatch(calvar_classic(y, dates, st$trend[i], st$season[i], K = K,
                                    lambda = lambda, design = "window",
                                    window = ww, ...)$fit$aicc,
                     error = function(e) NA_real_)
          }, numeric(1))
          data.frame(model = st$model[i], window = nm, w = window_grid, AICc = aicc)
        }))
        best <- vapply(split(tab, tab$model), function(t) t$w[which.min(t$AICc)], numeric(1))
        votes <- table(factor(best, levels = window_grid))
        win[[nm]] <- c(-window_grid[which.max(votes)], -1)
        wtab <- rbind(wtab, tab)
      }
    }
    for (i in seq_len(6)) {
      nm <- if (length(design) > 1) paste0(st$model[i], "_", d) else st$model[i]
      models[[nm]] <- calvar_classic(y, dates, st$trend[i], st$season[i], K = K,
                                     lambda = lambda, design = d, window = win, ...)
    }
  }
  spec <- data.frame(model = names(models),
                     trend = vapply(models, `[[`, "", "trend"),
                     season = vapply(models, `[[`, "", "season"),
                     design = vapply(models, `[[`, "", "design"), row.names = NULL)
  structure(list(models = models, spec = spec, window_aicc = wtab),
            class = "calvar_classic_set")
}

#' @export
print.calvar_classic_set <- function(x, ...) {
  eff <- lapply(x$models, .classic_effect)
  terms <- unique(unlist(lapply(eff, `[[`, "term")))
  terms <- c(intersect(c("before", "holiday", "after"), terms),
             setdiff(terms, c("before", "holiday", "after")))
  tab <- do.call(rbind, lapply(names(x$models), function(nm) {
    m <- x$models[[nm]]
    e <- eff[[nm]]
    cell <- vapply(terms, function(k) {
      i <- match(k, e$term)
      if (is.na(i)) "" else sprintf("%.2f (%.2f)", e$estimate[i], e$se[i])
    }, "")
    data.frame(model = nm,
               trend = c(deterministic = "det.", stochastic = "stoch.")[[m$trend]],
               season = c(dummy = "dummy", fourier = "Fourier",
                          stochastic = "stoch.")[[m$season]],
               ARIMA = sub(" errors", "", sub("Regression with ARIMA", "",
                                               as.character(m$fit))),
               AICc = round(m$fit$aicc, 1), t(cell), check.names = FALSE)
  }))
  designs <- unique(vapply(x$models, `[[`, "", "design"))
  cat(nrow(tab), "classical calendar variation models")
  if (length(designs) == 1) cat(";", .design_label(x$models[[1]]))
  cat("\n")
  print(tab, row.names = FALSE, right = FALSE)
  if (any(designs == "window")) {
    w <- x$models[[which(vapply(x$models, `[[`, "", "design") == "window")[1]]]$window
    cat("Windows:", paste(names(w), vapply(w, .fmt_win, ""), collapse = ", "), "\n")
  }
  cat("Trend: det. = deterministic, stoch. = stochastic; seasonality: stoch. =",
      "seasonal ARIMA.\nHoliday effects: estimate (s.e.); with the window design",
      "they are effects of a full window.\n")
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
                   object$K, h, design = object$design %||% "dummy",
                   window = object$window)
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
