#' Forecasts from a calendar variation model
#'
#' Future calendar regressors are built from the holiday dates. For a two-level
#' effect, the effect of every new occurrence comes from the level-2 rule and
#' its uncertainty (the one-step-ahead CV error of that rule, growing linearly
#' with the occurrence index) is added to the ARIMA forecast variance.
#'
#' @param object A `lunarcast` object.
#' @param h Forecast horizon (periods).
#' @param level Confidence level (a single value, in percent).
#' @param level2 Optional override of the level-2 rule: a method name or a
#'   fixed log effect (scenario), e.g. `list(adha = log(2.5))`.
#' @param ... Not used.
#' @return An object of class `"forecast"`. With `lambda = 0` the point
#'   forecast is the median. New occurrence effects are in `$future_effects`.
#' @examples
#' fit <- lunarcast(sapi, list(adha = holiday_dates("idul_adha")),
#'                  window = c(-3, 7), effect = "two-level",
#'                  trend = "stochastic", season = "none")
#' fc <- forecast(fit, h = 12)
#' fc$future_effects
#' # scenario: the next Eid al-Adha doubles the level
#' forecast(fit, h = 12, level2 = list(adha = log(2)))$mean
#' @method forecast lunarcast
#' @export
forecast.lunarcast <- function(object, h = 2 * frequency(object$y), level = 95,
                            level2 = NULL, ...) {
  o   <- object
  n   <- length(o$y)
  q   <- qnorm(0.5 + level[1] / 200)
  idx <- n + seq_len(h)
  if (!is.null(o$xreg) && nrow(o$xreg) < n + h)
    stop("`xreg` must have at least length(y) + h rows.")
  if (!is.null(level2)) level2 <- .per(level2, names(o$holidays), NULL)

  blocks <- fut <- list()
  add_m <- add_v <- numeric(h)
  for (nm in names(o$holidays)) {
    W <- .cal_block(o$holidays[[nm]], o$window[[nm]], o$y, nm, h = h)
    e <- o$effects[[nm]]
    if (o$effect[[nm]] == "constant") {
      w <- matrix(rowSums(W)[idx], ncol = 1, dimnames = list(NULL, nm))
      blocks[[nm]] <- if (o$share == "level") .remap(w, e$alpha) else w
      next
    }
    cols <- o$cols[[nm]]
    old  <- W[idx, cols, drop = FALSE]          # remainder of past occurrences
    blocks[[nm]] <- if (o$share == "level") .remap(old, e$alpha) else old
    new <- W[idx, setdiff(colnames(W), cols), drop = FALSE]
    new <- new[, colSums(new) > 0, drop = FALSE]
    if (ncol(new) == 0) next
    sel <- if (!is.null(level2[[nm]]))
      .level2_select(e$alpha, e$se, level2[[nm]], o$k) else o$level2[[nm]]
    m  <- ncol(new)
    af <- .l2_forecast(e$alpha, e$se, sel, o$k, m)
    vf <- sel$mse * seq_len(m)
    if (o$share == "level") new <- .remap(new, af)
    add_m <- add_m + as.numeric(new %*% af)
    add_v <- add_v + as.numeric(new^2 %*% vf)
    fut[[nm]] <- data.frame(event = .col_date(colnames(new)), alpha = af,
                            se = sqrt(vf), method = sel$method)
    if (.is_log(o$lambda)) fut[[nm]]$ratio <- exp(af)
  }
  xf <- if (is.null(o$xreg)) NULL else o$xreg[idx, , drop = FALSE]
  Xn <- cbind(do.call(cbind, unname(blocks)), .struct_X(o$structure, o$y, h), xf,
              .outlier_X(o$outliers, n, h))
  fc <- forecast(o$fit, xreg = Xn[, colnames(o$X), drop = FALSE], level = level[1])
  zm <- as.numeric(fc$mean)
  sd <- sqrt(((as.numeric(fc$upper[, 1]) - zm) / q)^2 + add_v)
  zm <- zm + add_m

  tsf <- function(v) ts(v, start = tsp(o$y)[2] + 1 / frequency(o$y),
                        frequency = frequency(o$y))
  lab  <- paste0(level[1], "%")
  fitv <- .inv(fitted(o$fit), o$lambda)
  structure(list(
    method = paste("lunarcast +", as.character(o$fit)), model = o,
    level = level[1], x = o$y, series = "y",
    mean  = tsf(.inv(zm, o$lambda)),
    lower = tsf(matrix(.inv(zm - q * sd, o$lambda), ncol = 1,
                       dimnames = list(NULL, lab))),
    upper = tsf(matrix(.inv(zm + q * sd, o$lambda), ncol = 1,
                       dimnames = list(NULL, lab))),
    fitted = fitv, residuals = o$y - fitv, future_effects = fut),
    class = "forecast")
}
