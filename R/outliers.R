# Outlier regressors: additive outlier (AO), level shift (LS) and temporary
# change (TC, decay 0.7); rows n + 1, ..., n + h when h > 0
.outlier_X <- function(info, n, h = 0) {
  if (is.null(info) || !nrow(info)) return(NULL)
  idx <- if (h == 0) seq_len(n) else n + seq_len(h)
  X <- vapply(seq_len(nrow(info)), function(i) {
    t <- info$t[i]
    switch(info$type[i],
      AO = as.numeric(idx == t),
      LS = as.numeric(idx >= t),
      TC = ifelse(idx >= t, 0.7^(idx - t), 0))
  }, numeric(length(idx)))
  matrix(X, nrow = length(idx), dimnames = list(NULL, info$name))
}

.period_label <- function(y, t) {
  tt <- time(y)[t]
  yr <- floor(tt + 1e-6)
  sprintf("%d_%02d", yr, round((tt - yr) * frequency(y)) + 1)
}

# Forward selection on the regARIMA residuals: candidates are the largest
# robustly standardised residuals outside two-level holiday periods; the best
# candidate (AICc) is kept while its |t-statistic| exceeds cval.
.detect_outliers <- function(z, X, ord, const, protect, cval = 3.5,
                             types = c("AO", "LS", "TC"), max_out = 5) {
  n <- length(z)
  info <- data.frame(name = character(), type = character(), t = integer(),
                     tstat = numeric(), stringsAsFactors = FALSE)
  fitx <- function(inf) tryCatch(
    Arima(z, order = ord, xreg = cbind(X, .outlier_X(inf, n)),
          include.constant = const, method = "ML"), error = function(e) NULL)
  for (it in seq_len(max_out)) {
    fit <- fitx(info)
    if (is.null(fit)) break
    r  <- as.numeric(residuals(fit))
    zs <- abs(r) / (1.483 * mad(r, na.rm = TRUE))
    zs[protect | seq_len(n) <= ord[2] + 1 | seq_len(n) %in% info$t] <- 0
    cand <- head(order(zs, decreasing = TRUE), 3)
    cand <- cand[zs[cand] > cval]
    best <- NULL
    for (t in cand) for (ty in types) {
      row <- data.frame(name = paste0(ty, .period_label(z, t)), type = ty, t = t,
                        tstat = NA_real_, stringsAsFactors = FALSE)
      f2 <- fitx(rbind(info, row))
      if (is.null(f2)) next
      tv <- coef(f2)[row$name] / sqrt(diag(vcov(f2)))[row$name]
      if (is.finite(tv) && abs(tv) > cval &&
          (is.null(best) || f2$aicc < best$aicc)) {
        row$tstat <- unname(tv)
        best <- list(aicc = f2$aicc, row = row)
      }
    }
    if (is.null(best)) break
    info <- rbind(info, best$row)
  }
  info
}
