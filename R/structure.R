# Deterministic trend and Fourier seasonality regressors (dummy seasonality is
# Fourier with K = frequency / 2, which spans the same space as s - 1 dummies)
.struct_X <- function(st, y, h = 0) {
  n <- length(y)
  X <- NULL
  if (st$trend == "deterministic")
    X <- cbind(X, trend = if (h == 0) seq_len(n) else n + seq_len(h))
  if (st$K > 0)
    X <- cbind(X, .mat(if (h == 0) fourier(y, K = st$K)
                       else fourier(y, K = st$K, h = h)))
  X
}

.adjust <- function(z, X) {
  ts(residuals(lm(z ~ X)), start = start(z), frequency = frequency(z))
}

# Trend: KPSS tests on the calendar-adjusted series. Level-stationary -> "none";
# trend-stationary -> "deterministic" (d = 0 plus a linear trend);
# otherwise "stochastic" (d >= 1).
.select_trend <- function(z, X0, trend) {
  zadj <- .adjust(z, X0)
  if (trend == "auto") {
    trend <- if (ndiffs(zadj, type = "level") == 0) "none"
             else if (ndiffs(zadj, type = "trend") == 0) "deterministic"
             else "stochastic"
  }
  d <- if (trend == "stochastic") max(1, ndiffs(zadj)) else 0
  list(trend = trend, d = d, D = 0, season = "none", K = 0, table = NULL)
}

# Seasonality given the calendar regressors: AICc among models with the same
# differencing, so the values are comparable. Candidates: none, Fourier with
# K = 1, ..., s/2 - 1, dummy (K = s/2) and stochastic (seasonal ARMA; the
# seasonal order is chosen from a small grid on top of the non-seasonal order
# found by auto.arima).
.select_season <- function(z, X, st, season, ...) {
  f <- frequency(z)
  if (season %in% c("auto", "stochastic"))
    st$D <- tryCatch(nsdiffs(.adjust(z, X)), error = function(e) 0)
  if (season == "auto" && st$D > 0) season <- "stochastic"
  cand <- switch(season, auto = c(0:(f / 2), -1), fourier = seq_len(f / 2 - 1),
                 dummy = f / 2, none = 0, stochastic = -1)
  lab <- function(K) ifelse(K == -1, "stochastic", ifelse(K == 0, "none",
                     ifelse(K == f / 2, "dummy", paste0("fourier(K=", K, ")"))))
  crit <- rep(NA_real_, length(cand))
  if (length(cand) > 1 || -1 %in% cand) {
    pre <- auto.arima(z, xreg = X, d = st$d, seasonal = FALSE, ...)
    st$order <- arimaorder(pre)[1:3]
    st$const <- .const(pre)
    aicc <- function(X, sord = c(0, 0, 0)) tryCatch(
      Arima(z, order = st$order, seasonal = sord, xreg = X,
            include.constant = st$const, method = "ML")$aicc,
      error = function(e) NA_real_)
    if (-1 %in% cand) {
      sords <- lapply(list(c(1, 0), c(0, 1), c(1, 1), c(2, 0), c(2, 1)),
                      function(o) c(o[1], st$D, o[2]))
      sa <- vapply(sords, function(s) aicc(X, s), numeric(1))
      st$sord <- if (all(is.na(sa))) sords[[1]] else sords[[which.min(sa)]]
      crit[cand == -1] <- if (all(is.na(sa))) NA_real_ else min(sa, na.rm = TRUE)
    }
    if (length(cand) > 1) {
      crit[cand != -1] <- vapply(cand[cand != -1], function(K)
        aicc(cbind(X, if (K > 0) .mat(fourier(z, K = K)))), numeric(1))
      st$table <- data.frame(season = lab(cand), AICc = crit)
    }
  }
  pick <- if (length(cand) > 1) cand[which.min(crit)] else cand
  st$season <- sub("\\(.*", "", lab(pick))
  st$K <- max(pick, 0)
  st
}

# Approximate AICc from a CSS fit (as in auto.arima's approximation); used to
# rank seasonal ARMA orders quickly
.aicc_css <- function(z, ord, sord, X, const) {
  fit <- tryCatch(suppressWarnings(
    Arima(z, order = ord, seasonal = sord, xreg = X, include.constant = const,
          method = "CSS")), error = function(e) NULL)
  if (is.null(fit) || !is.finite(fit$sigma2) || fit$sigma2 <= 0) return(NA_real_)
  nstar <- length(z) - ord[2] - sord[2] * frequency(z)
  npar  <- length(coef(fit)) + 1
  nstar * log(fit$sigma2) + 2 * npar + 2 * npar * (npar + 1) / (nstar - npar - 1)
}

.st_label <- function(st) {
  s <- switch(st$season,
    fourier    = paste0("fourier(K=", st$K, ")"),
    stochastic = paste0("SARMA(", paste(st$sord, collapse = ","), ")"),
    st$season)
  paste(st$trend, "+", s)
}

# Candidate structures for cross-validation: for every trend type, no
# seasonality, Fourier (K by AICc), dummy and seasonal ARMA (order by
# approximate AICc)
.structure_candidates <- function(z, Xc, trends, season, ...) {
  f <- frequency(z)
  types <- if (season == "auto") c("none", "fourier", "dummy", "stochastic") else season
  dsto  <- max(1, ndiffs(.adjust(z, Xc)))
  out <- list()
  for (tr in trends) {
    st <- list(trend = tr, d = if (tr == "stochastic") dsto else 0, D = 0,
               season = "none", K = 0, table = NULL)
    Xt  <- cbind(Xc, .struct_X(st, z))
    pre <- auto.arima(z, xreg = Xt, d = st$d, seasonal = FALSE, ...)
    st$order <- arimaorder(pre)[1:3]
    st$const <- .const(pre)
    for (ty in types) {
      s2 <- st
      s2$season <- ty
      if (ty == "fourier") {
        Ks <- seq_len(f / 2 - 1)
        a  <- vapply(Ks, function(K) tryCatch(
          Arima(z, order = st$order, xreg = cbind(Xt, .mat(fourier(z, K = K))),
                include.constant = st$const, method = "ML")$aicc,
          error = function(e) NA_real_), numeric(1))
        if (all(is.na(a))) next
        s2$K <- Ks[which.min(a)]
      } else if (ty == "dummy") {
        s2$K <- f / 2
      } else if (ty == "stochastic") {
        s2$D  <- tryCatch(nsdiffs(.adjust(z, Xt)), error = function(e) 0)
        sords <- lapply(list(c(1, 0), c(0, 1), c(1, 1), c(2, 0), c(2, 1)),
                        function(o) c(o[1], s2$D, o[2]))
        a <- vapply(sords, function(s) .aicc_css(z, st$order, s, Xt, st$const),
                    numeric(1))
        if (all(is.na(a))) next
        s2$sord <- sords[[which.min(a)]]
      }
      out[[length(out) + 1]] <- s2
    }
  }
  out
}

# Structural complexity used by the one-standard-error rule: slope of a
# deterministic trend plus Fourier/dummy columns or seasonal ARMA terms
.st_complexity <- function(st, f) {
  (st$trend == "deterministic") + switch(st$season, none = 0,
    fourier = 2 * st$K, dummy = f - 1, stochastic = sum(st$sord[c(1, 3)]))
}

# Calendar-aware cross-validation of candidate structures: forecast origins one
# year apart over the last `cv_years` years, one-year horizon. Every candidate
# is refitted by CSS with its ARIMA orders fixed and forecast with the calendar
# machinery (new occurrences through the level-2 rule). Returns the pooled
# RMSE, its standard error across origins and the structural complexity.
.cv_structure <- function(y, cands, spec, cv_years) {
  n <- length(y)
  f <- frequency(y)
  origins <- n - f * rev(seq_len(cv_years))
  origins <- origins[origins >= 4 * f]
  tab <- data.frame(structure = vapply(cands, .st_label, ""), RMSE = NA_real_,
                    SE = NA_real_,
                    complexity = vapply(cands, .st_complexity, numeric(1), f = f))
  for (i in seq_along(cands)) {
    E <- lapply(origins, function(T) {
      h  <- min(f, n - T)
      sp <- spec
      sp$y <- ts(y[seq_len(T)], start = start(y), frequency = f)
      if (!is.null(sp$xreg)) sp$xreg <- sp$xreg[seq_len(T + h), , drop = FALSE]
      if (!is.null(sp$outliers))
        sp$outliers <- sp$outliers[sp$outliers$t < T, , drop = FALSE]
      sp$st <- cands[[i]]
      sp$method <- "CSS"
      p <- tryCatch(suppressWarnings(
             as.numeric(forecast(do.call(.fit_spec, sp), h = h)$mean)),
           error = function(e) rep(NA_real_, h))
      p - y[T + seq_len(h)]
    })
    e <- unlist(E)
    if (all(is.na(e))) next
    rf <- vapply(E, function(v) sqrt(mean(v^2, na.rm = TRUE)), numeric(1))
    rf <- rf[is.finite(rf)]
    tab$RMSE[i] <- sqrt(mean(e^2, na.rm = TRUE))
    if (length(rf) > 1) tab$SE[i] <- sd(rf) / sqrt(length(rf))
  }
  tab
}

# "min": smallest CV-RMSE; "1se": simplest structure within one standard
# error of the smallest (ties broken by CV-RMSE)
.cv_pick <- function(tab, rule) {
  if (all(is.na(tab$RMSE))) return(1L)
  b <- which.min(tab$RMSE)
  if (rule == "min" || is.na(tab$SE[b])) return(b)
  ok <- which(tab$RMSE <= tab$RMSE[b] + tab$SE[b])
  ok[order(tab$complexity[ok], tab$RMSE[ok])][1]
}
