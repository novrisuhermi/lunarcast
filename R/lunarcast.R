#' Automatic two-level calendar variation model
#'
#' Fits a regression with ARIMA errors in which moving holidays enter through
#' window regressors and may have a different effect at every occurrence, and
#' selects the whole specification automatically.
#'
#' @section Model:
#' On the log scale (`lambda = 0`), with \eqn{z_t = \log y_t},
#' \deqn{z_t = \sum_h \sum_j \alpha_{hj}\,\tilde w_{hj,t} +
#'   \mathbf{x}_t^\top \boldsymbol\beta + N_t,}
#' where \eqn{w_{hj,t}} is the share of the window
#' \eqn{[D_{hj} + a_h, D_{hj} + b_h]} around occurrence \eqn{j} of holiday
#' \eqn{h} that falls in period \eqn{t}, mapped to the log scale by
#' \eqn{\tilde w = \log\{1 + (e^{\alpha} - 1) w\} / \alpha}; \eqn{\mathbf{x}_t}
#' holds the trend, seasonal and outlier regressors and \eqn{N_t} is an ARIMA
#' process. With a *constant* effect \eqn{\alpha_{hj} = \alpha_h}; with a
#' *two-level* effect every occurrence has its own \eqn{\alpha_{hj}} (level 1)
#' and the effect of the next occurrence is forecast from the sequence of past
#' effects (level 2). See `vignette("theory", package = "lunarcast")`.
#'
#' @section Automatic specification:
#' (1) a preliminary trend type by KPSS tests; (2) constant versus two-level
#' effect and (3) the window of every holiday by AICc on a grid
#' (coordinate-wise, linear window shares, non-seasonal ARIMA errors);
#' (4) optionally outliers outside two-level holiday periods; (5) the trend
#' and seasonal structure: the trend type follows KPSS tests (a deterministic
#' trend is considered only if they indicate trend stationarity) and the
#' seasonality is chosen by calendar-aware cross-validation with a
#' one-standard-error rule (`select = "cv"`), or by AICc
#' (`select = "aicc"`); (6) the final ARIMA order by [forecast::auto.arima()]
#' (for stochastic seasonality the seasonal order comes from a small grid);
#' (7) the level-2 rule by one-step-ahead cross-validation on the sequence of
#' occurrence effects.
#'
#' @param y Monthly or quarterly `ts`.
#' @param holidays Named list of `Date` vectors, e.g.
#'   `list(adha = holiday_dates("idul_adha"))`.
#' @param effect `"auto"`, `"constant"` or `"two-level"`, or a named list.
#' @param window `"auto"` or `c(pre, post)` in days, or a named list.
#' @param grid List with candidate `pre` and `post` offsets (days, >= 0).
#' @param level2 `"auto"`, `"mean"`, `"wmean"` (precision-weighted mean),
#'   `"rw"`, `"ses"`, `"holt"` (damped) or a fixed log effect, or a named list.
#' @param k Number of recent occurrences used by `"mean"` and `"wmean"`.
#' @param lambda Box-Cox parameter; `0` (default) gives multiplicative effects,
#'   `NULL` none.
#' @param share `"level"` maps window shares to the log scale (only for
#'   `lambda = 0`); `"linear"` uses them as they are.
#' @param trend `"auto"` (default): KPSS tests decide whether the series is
#'   trend-stationary; a deterministic trend is a candidate only if they do,
#'   a stochastic trend (differencing) always. `"cv"` compares deterministic
#'   and stochastic trends by cross-validation. `"none"`, `"deterministic"`
#'   or `"stochastic"` fix the trend.
#' @param season `"auto"`, `"none"`, `"dummy"`, `"fourier"` or `"stochastic"`.
#' @param select `"cv"` compares the candidate structures (the trends allowed
#'   by `trend`, each with no, Fourier, dummy or stochastic seasonality) by
#'   one-year-ahead forecasts from `cv_years` origins one year apart at the
#'   end of the sample; `"aicc"` chooses the trend by KPSS tests and the
#'   seasonality by AICc.
#' @param cv_years Number of forecast origins for `select = "cv"`.
#' @param cv_rule `"1se"` (default) takes the simplest structure whose
#'   CV-RMSE is within one standard error (across forecast origins) of the
#'   smallest; `"min"` takes the structure with the smallest CV-RMSE.
#' @param outliers If `TRUE`, additive outliers, level shifts and temporary
#'   changes are detected outside the periods of two-level holidays.
#' @param cval Critical |t| value for outliers.
#' @param xreg Optional extra regressors with at least `length(y) + h` rows
#'   (rows beyond `length(y)` are used for forecasting).
#' @param min_events Minimum number of occurrences for a two-level effect.
#' @param ... Passed to [forecast::auto.arima()].
#' @return An object of class `"lunarcast"` with, among others, the fitted
#'   ARIMA model (`fit`), the selected windows (`window`), effect types
#'   (`effect`) and structure (`structure`), the occurrence effects
#'   (`effects`, see [holiday_effects()]), the level-2 rules (`level2`) and the
#'   cross-validation table (`cv`).
#' @seealso [forecast.lunarcast()], [holiday_effects()], [calvar_classic()]
#'   for the classical model and [rolling_origin()] for evaluation.
#' @examples
#' hol <- list(adha = holiday_dates("idul_adha"),
#'             fitri = holiday_dates("idul_fitri"))
#' # fixed windows and structure keep the example fast
#' fit <- lunarcast(sapi, hol,
#'                  window = list(adha = c(-3, 7), fitri = c(-28, 3)),
#'                  effect = list(adha = "two-level", fitri = "constant"),
#'                  trend = "stochastic", season = "none")
#' fit
#' plot(fit)
#' forecast(fit, h = 12)
#' \donttest{
#' # fully automatic specification (about a minute)
#' auto <- lunarcast(sapi, hol)
#' }
#' @export
lunarcast <- function(y, holidays, effect = "auto", window = "auto",
                   grid = list(pre = c(0, 3, 7, 14, 28), post = c(0, 3, 7, 14)),
                   level2 = "auto", k = 3, lambda = 0,
                   share = c("level", "linear"),
                   trend = c("auto", "cv", "none", "deterministic", "stochastic"),
                   season = c("auto", "none", "dummy", "fourier", "stochastic"),
                   select = c("cv", "aicc"), cv_years = 5,
                   cv_rule = c("1se", "min"),
                   outliers = FALSE, cval = 3.5,
                   xreg = NULL, min_events = 6, ...) {
  share  <- match.arg(share)
  trend  <- match.arg(trend)
  season <- match.arg(season)
  select <- match.arg(select)
  cv_rule <- match.arg(cv_rule)
  if (!frequency(y) %in% c(4, 12)) stop("`y` must be a monthly or quarterly ts.")
  if (inherits(holidays, "Date")) holidays <- list(holiday = holidays)
  hn <- names(holidays)
  if (is.null(hn) || any(hn == ""))
    stop("`holidays` must be a named list of Date vectors.")
  if (!.is_log(lambda)) share <- "linear"
  n <- length(y)
  z <- .trans(y, lambda)
  xin <- NULL
  if (!is.null(xreg)) {
    xreg <- .mat(as.matrix(xreg))
    if (is.null(colnames(xreg))) colnames(xreg) <- paste0("x", seq_len(ncol(xreg)))
    xin <- xreg[seq_len(n), , drop = FALSE]
  }
  effect <- .per(effect, hn, "auto")
  window <- .per(window, hn, "auto")
  level2 <- .per(level2, hn, "auto")
  win  <- lapply(window, function(w) if (identical(w, "auto")) c(0, 0) else w)
  eff  <- lapply(effect, function(e) if (identical(e, "auto")) "constant" else e)
  calX <- function(win, eff) .cal_fit_X(holidays, win, eff, y)

  # (1) preliminary trend, given dummy windows and constant effects
  st <- .select_trend(z, cbind(calX(win, eff)$X, xin),
                      if (trend == "cv") "auto" else trend)
  Xt <- .struct_X(st, y)
  if (any(effect == "auto") || any(window == "auto") || outliers) {
    base <- auto.arima(z, xreg = cbind(calX(win, eff)$X, Xt, xin), d = st$d,
                       seasonal = FALSE, ...)
    ord <- arimaorder(base)
    cst <- .const(base)
  }
  crit <- function(win, eff) tryCatch(
    Arima(z, order = ord[1:3], xreg = cbind(calX(win, eff)$X, Xt, xin),
          include.constant = cst, method = "ML")$aicc,
    error = function(e) NA_real_)

  # (2) constant vs two-level effect
  for (nm in hn) if (identical(effect[[nm]], "auto")) {
    nev <- sum(colSums(.cal_block(holidays[[nm]], win[[nm]], y, nm)) > 0.5)
    if (nev >= min_events) {
      e2 <- eff
      e2[[nm]] <- "two-level"
      if (isTRUE(crit(win, e2) < crit(win, eff))) eff <- e2
    }
  }

  # (3) window of every holiday (coordinate-wise grid search)
  wtab <- list()
  for (nm in hn) if (identical(window[[nm]], "auto")) {
    g <- expand.grid(pre = -grid$pre, post = grid$post)
    g$AICc <- vapply(seq_len(nrow(g)), function(i) {
      w2 <- win
      w2[[nm]] <- c(g$pre[i], g$post[i])
      crit(w2, eff)
    }, numeric(1))
    b <- which.min(g$AICc)
    win[[nm]]  <- c(g$pre[b], g$post[b])
    wtab[[nm]] <- g[order(g$AICc), ]
  }
  cb <- calX(win, eff)

  # (4) outliers outside the periods of two-level occurrences
  oinfo <- NULL
  if (outliers) {
    tl   <- unlist(cb$cols[unlist(eff) == "two-level"], use.names = FALSE)
    prot <- if (length(tl)) rowSums(cb$X[, tl, drop = FALSE] > 0) > 0
            else rep(FALSE, n)
    oinfo <- .detect_outliers(z, cbind(cb$X, Xt, xin), ord[1:3], cst, prot, cval)
  }
  Xo <- .outlier_X(oinfo, n)

  # (5) trend and seasonal structure
  spec <- list(y = y, holidays = holidays, win = win, eff = eff, xreg = xreg,
               lambda = lambda, share = share, k = k, level2 = level2,
               outliers = oinfo)
  cvtab <- NULL
  if (select == "aicc") {
    st <- .select_season(z, cbind(cb$X, Xt, xin, Xo), st, season, ...)
  } else {
    trends <- switch(trend,
      auto = unique(c(st$trend, "stochastic")),   # deterministic only if KPSS
      cv   = c("deterministic", "stochastic"),    # supports trend stationarity
      st$trend)
    cands <- .structure_candidates(z, cbind(cb$X, xin, Xo), trends, season, ...)
    st <- cands[[1]]
    if (length(cands) > 1) {
      cvtab <- .cv_structure(y, cands, spec, cv_years)
      st    <- cands[[.cv_pick(cvtab, cv_rule)]]
    }
  }

  # (6)-(7) final fit (ML), occurrence effects and level-2 rules
  obj <- do.call(.fit_spec, c(spec, list(st = st, method = "ML", search = TRUE),
                              list(...)))
  obj$window_aicc <- wtab
  obj$cv <- cvtab
  obj$select <- if (select == "aicc") "aicc" else
    if (is.null(cvtab)) "fixed" else paste0("cv, ", cv_rule)
  obj
}

# Fit with every choice fixed (calendar, structure, outliers); `search = TRUE`
# re-selects the non-seasonal ARIMA order with auto.arima. Window shares are
# mapped to the log scale by iteration when share = "level".
.fit_spec <- function(y, holidays, win, eff, st, xreg = NULL, lambda = 0,
                      share = "level", k = 3, level2 = NULL, outliers = NULL,
                      method = "ML", search = FALSE, ...) {
  if (is.null(level2)) level2 <- .per("auto", names(holidays), "auto")
  n   <- length(y)
  z   <- .trans(y, lambda)
  xin <- if (is.null(xreg)) NULL else xreg[seq_len(n), , drop = FALSE]
  cb  <- .cal_fit_X(holidays, win, eff, y)
  X   <- cbind(cb$X, .struct_X(st, y), xin, .outlier_X(outliers, n))
  arma <- function(X, ord, sord, cst) {
    # try the requested method first; a fit that fails or is not stationary
    # and invertible is re-estimated with full maximum likelihood
    best <- NULL
    for (m in unique(c(method, "CSS-ML", "ML", "CSS"))) {
      fit <- tryCatch(suppressWarnings(Arima(z, order = ord, seasonal = sord,
                                             xreg = X, include.constant = cst,
                                             method = m)),
                      error = function(e) NULL)
      if (is.null(fit)) next
      if (.arma_ok(fit)) return(fit)
      if (is.null(best)) best <- fit
    }
    if (is.null(best)) stop("ARIMA estimation failed.")
    best
  }
  fit <- if (search && st$season != "stochastic")
    auto.arima(z, xreg = X, d = st$d, seasonal = FALSE, ...)
  else arma(X, st$order, if (st$season == "stochastic") st$sord else c(0, 0, 0),
            st$const)
  cc <- unlist(cb$cols, use.names = FALSE)
  if (share == "level") {
    ord <- arimaorder(fit)
    cst <- .const(fit)
    for (i in 1:2) {
      X2 <- X
      X2[, cc] <- .remap(cb$X, coef(fit)[cc])
      f2 <- tryCatch(arma(X2, ord[1:3], .sord(ord), cst), error = function(e) NULL)
      if (is.null(f2)) break
      X   <- X2
      fit <- f2
    }
  }
  v  <- diag(vcov(fit))
  se <- sqrt(ifelse(is.finite(v) & v > 0, v, NA_real_))
  effects <- l2 <- list()
  for (nm in names(holidays)) {
    cn <- cb$cols[[nm]]
    e  <- data.frame(
      event = if (eff[[nm]] == "two-level") .col_date(cn) else as.Date(NA),
      alpha = unname(coef(fit)[cn]), se = unname(se[cn]))
    if (.is_log(lambda)) e$ratio <- exp(e$alpha)
    effects[[nm]] <- e
    if (eff[[nm]] == "two-level")
      l2[[nm]] <- .level2_select(e$alpha, e$se, level2[[nm]], k)
  }
  structure(list(fit = fit, y = y, X = X, holidays = holidays, effect = eff,
                 window = win, structure = st, effects = effects, level2 = l2,
                 cols = cb$cols, k = k, lambda = lambda, share = share,
                 xreg = xreg, outliers = outliers), class = "lunarcast")
}

#' @export
print.lunarcast <- function(x, ...) {
  st <- x$structure
  cat("lunarcast:", as.character(x$fit), "| AICc =", round(x$fit$aicc, 1), "\n")
  cat("Structure (", x$select, "): ", .st_label(st), ", d = ", st$d, "\n",
      sep = "")
  if (!is.null(x$cv)) {
    cv <- x$cv[order(x$cv$RMSE), c("structure", "RMSE", "SE")]
    cat("Cross-validated RMSE of the candidate structures:\n")
    print(cv, digits = 3, row.names = FALSE)
  }
  if (!is.null(x$outliers) && nrow(x$outliers))
    cat("Outliers:", paste0(x$outliers$name, " (t = ",
                            format(x$outliers$tstat, digits = 2), ")",
                            collapse = ", "), "\n")
  for (nm in names(x$holidays)) {
    w <- x$window[[nm]]
    e <- x$effects[[nm]]
    cat("\n", nm, ": ", x$effect[[nm]], " effect, window [", w[1], ", +", w[2],
        "] days", sep = "")
    if (x$effect[[nm]] == "two-level") {
      l <- x$level2[[nm]]
      cat(", level 2 = ", l$method,
          if (l$method != "fixed" && is.finite(l$mse))
            paste0(" (CV-RMSE ", format(sqrt(l$mse), digits = 2), ")"),
          "\n", sep = "")
      print(e, digits = 3, row.names = FALSE)
    } else {
      cat("\n")
      print(e[, -1], digits = 3, row.names = FALSE)
    }
  }
  invisible(x)
}

#' @export
plot.lunarcast <- function(x, holiday = NULL, ...) {
  tl <- names(x$effect)[unlist(x$effect) == "two-level"]
  if (!length(tl)) stop("No two-level holiday effect to plot.")
  nm <- if (is.null(holiday)) tl[1] else holiday
  e  <- x$effects[[nm]]
  rt <- "ratio" %in% names(e)
  g  <- if (rt) exp else identity
  lo <- e$alpha - 1.96 * e$se
  hi <- e$alpha + 1.96 * e$se
  nx <- x$holidays[[nm]][x$holidays[[nm]] > max(e$event)][1]   # next occurrence
  af <- .l2_forecast(e$alpha, e$se, x$level2[[nm]], x$k, 1)
  sf <- sqrt(x$level2[[nm]]$mse)
  plot(e$event, g(e$alpha), type = "b", pch = 19,
       xlim = range(c(e$event, nx), na.rm = TRUE),
       ylim = g(range(lo, hi, af - 1.96 * sf, af + 1.96 * sf, na.rm = TRUE)),
       xlab = "", ylab = if (rt) "Effect (ratio)" else "Effect",
       main = paste("Holiday effect per occurrence:", nm), ...)
  segments(e$event, g(lo), e$event, g(hi))
  if (!is.na(nx)) {
    points(nx, g(af), pch = 1, cex = 1.4)
    segments(nx, g(af - 1.96 * sf), nx, g(af + 1.96 * sf), lty = 2)
  }
  abline(h = g(0), lty = 3)
  invisible(x)
}


#' Holiday effects of a fitted model
#'
#' Collects the estimated holiday effects of a [lunarcast()] fit in one data
#' frame: one row per occurrence for two-level effects and one row per
#' holiday for constant effects.
#'
#' @param object A `lunarcast` object.
#' @return A data frame with columns `holiday`, `type` (`"two-level"` or
#'   `"constant"`), `event` (occurrence date), `alpha` (effect on the
#'   transformed scale), `se` and, when `lambda = 0`, `ratio` (multiplicative
#'   effect \eqn{e^{\alpha}}).
#' @examples
#' fit <- lunarcast(sapi, list(adha = holiday_dates("idul_adha")),
#'                  window = c(-3, 7), effect = "two-level",
#'                  trend = "stochastic", season = "none")
#' head(holiday_effects(fit))
#' @export
holiday_effects <- function(object) {
  if (!inherits(object, "lunarcast")) stop("`object` must be a lunarcast fit.")
  out <- lapply(names(object$effects), function(nm)
    data.frame(holiday = nm, type = object$effect[[nm]], object$effects[[nm]]))
  do.call(rbind, out)
}
