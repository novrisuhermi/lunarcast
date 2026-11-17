.l2_methods <- c("mean", "wmean", "rw", "ses", "holt")

# Forecast the effects of the next m occurrences from past effects a
.l2_path <- function(a, se, method, k, m = 1) {
  switch(method,
    mean  = rep(mean(tail(a, k)), m),
    wmean = {                                   # precision-weighted mean
      w <- 1 / tail(se, k)^2
      ok <- is.finite(w)
      rep(if (any(ok)) sum(w[ok] * tail(a, k)[ok]) / sum(w[ok])
          else mean(tail(a, k)), m)
    },
    rw    = rep(tail(a, 1), m),
    ses   = as.numeric(ses(a, h = m)$mean),
    holt  = as.numeric(holt(a, h = m, damped = TRUE)$mean),
    stop("Unknown level-2 method: ", method))
}

# One-step-ahead errors over the last n_test occurrences
.l2_errors <- function(a, se, methods, k, n_test = 8, n_min = 5) {
  J <- length(a)
  if (J <= n_min) return(NULL)
  js <- seq(max(n_min + 1, J - n_test + 1), J)
  E <- vapply(methods, function(m) vapply(js, function(j) tryCatch(
    suppressWarnings(a[j] - .l2_path(a[seq_len(j - 1)], se[seq_len(j - 1)], m, k)),
    error = function(e) NA_real_), numeric(1)), numeric(length(js)))
  matrix(E, nrow = length(js), dimnames = list(NULL, methods))
}

# Choose the level-2 rule by one-step-ahead CV on the effect sequence; a
# numeric `method` fixes the future effect (scenario)
.level2_select <- function(a, se, method, k) {
  if (is.numeric(method))
    return(list(method = "fixed", value = method, mse = 0, cv = NULL))
  methods  <- if (identical(method, "auto")) .l2_methods else method
  E        <- .l2_errors(a, se, methods, k)
  fallback <- list(method = "mean", mse = if (length(a) > 1) var(a) else NA_real_,
                   cv = NULL)
  if (is.null(E)) return(fallback)
  ok <- colSums(!is.na(E)) >= 3
  if (!any(ok)) return(fallback)
  E  <- E[complete.cases(E[, ok, drop = FALSE]), ok, drop = FALSE]
  cv <- colMeans(E^2)
  best <- names(cv)[which.min(cv)]
  list(method = best, mse = cv[[best]], cv = cv)
}

.l2_forecast <- function(a, se, sel, k, m) {
  if (sel$method == "fixed") return(rep(sel$value, m))
  tryCatch(suppressWarnings(.l2_path(a, se, sel$method, k, m)),
           error = function(e) rep(mean(tail(a, k)), m))
}
