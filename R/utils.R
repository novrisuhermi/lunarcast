.mat <- function(x) {
  matrix(as.numeric(x), nrow = NROW(x), dimnames = list(NULL, colnames(x)))
}
.trans <- function(y, lambda) if (is.null(lambda)) y else BoxCox(y, lambda)
.inv   <- function(z, lambda) if (is.null(lambda)) z else InvBoxCox(z, lambda)
.is_log <- function(lambda) !is.null(lambda) && lambda == 0
.const <- function(fit) any(c("intercept", "drift") %in% names(coef(fit)))
.sord  <- function(ord) if (length(ord) > 3) ord[4:6] else c(0, 0, 0)

# Expand a per-holiday setting into a named list
.per <- function(x, nm, default) {
  out <- setNames(rep(list(default), length(nm)), nm)
  if (is.list(x)) {
    k <- intersect(names(x), nm)
    out[k] <- x[k]
  } else {
    out[] <- list(x)
  }
  out
}

# Window shares -> log scale. If the full effect is exp(a) and a share w of
# the window falls in a period, the level of that period rises by
# 1 + (exp(a) - 1) w, so the consistent log-scale regressor is
# log(1 + (exp(a) - 1) w) / a  (identical to w when w is 0 or 1).
.remap <- function(W, alpha) {
  alpha <- rep_len(alpha, ncol(W))
  for (j in seq_len(ncol(W))) {
    a <- alpha[j]
    if (is.finite(a) && abs(a) > 1e-3) W[, j] <- log1p(expm1(a) * W[, j]) / a
  }
  W
}

# Occurrence date from a column name "holiday_YYYYMMDD"
.col_date <- function(cols) as.Date(sub(".*_", "", cols), format = "%Y%m%d")

# TRUE if the ARMA part is stationary and invertible (roots outside the unit
# circle, with a small margin)
.arma_ok <- function(fit, tol = 1.001) {
  outside <- function(p) {
    p <- p[seq_len(max(c(0, which(abs(p) > 1e-10))))]
    !length(p) || all(Mod(polyroot(c(1, p))) > tol)
  }
  outside(-fit$model$phi) && outside(fit$model$theta)
}
