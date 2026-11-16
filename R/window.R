#' Moving-holiday window regressors
#'
#' Computes, for every period of a monthly or quarterly series, the share of
#' each holiday window `[date + window[1], date + window[2]]` (in days) that
#' falls in that period:
#' \deqn{w_{j,t} = |W_j \cap P_t| / |W_j|,}
#' where \eqn{W_j} is the window around occurrence \eqn{j} and \eqn{P_t} the
#' set of days of period \eqn{t}.
#'
#' @param dates `Date` vector of holiday occurrences.
#' @param window Numeric `c(pre, post)` in days relative to each date, e.g.
#'   `c(-3, 7)` for three days before to seven days after. `c(0, 0)`
#'   reproduces the usual dummy for the period containing the holiday.
#' @param y A `ts` object (frequency 12 or 4) defining start and length.
#' @param h Number of future periods to append.
#' @param per_event If `TRUE`, one column per occurrence; otherwise their sum
#'   (one column named `holiday`).
#' @return A `ts` matrix with `length(y) + h` rows.
#' @examples
#' w <- holiday_window(holiday_dates("idul_adha"), c(-3, 7), sapi)
#' # Eid al-Adha on 31 July 2020: most of the window falls in August
#' window(w, start = c(2020, 6), end = c(2020, 9))
#' @export
holiday_window <- function(dates, window = c(0, 0), y, h = 0, per_event = FALSE) {
  f <- frequency(y)
  if (!f %in% c(4, 12)) stop("Only monthly or quarterly series are supported.")
  n <- length(y) + h
  s <- start(y)
  X <- vapply(seq_along(dates), function(j) {
    d   <- seq(dates[j] + window[1], dates[j] + window[2], by = "day")
    lt  <- as.POSIXlt(d)
    pos <- (lt$year + 1900 - s[1]) * f + (lt$mon %/% (12 / f) + 1 - s[2]) + 1
    tabulate(pos[pos >= 1 & pos <= n], nbins = n) / length(d)
  }, numeric(n))
  X <- matrix(X, nrow = n,
              dimnames = list(NULL, paste0("event_", format(dates, "%Y-%m-%d"))))
  if (!per_event) X <- matrix(rowSums(X), ncol = 1, dimnames = list(NULL, "holiday"))
  ts(X, start = s, frequency = f)
}

# Occurrence columns named "holiday_YYYYMMDD"
.cal_block <- function(dates, win, y, nm, h = 0) {
  W <- .mat(holiday_window(dates, win, y, h = h, per_event = TRUE))
  colnames(W) <- paste0(nm, "_", format(dates, "%Y%m%d"))
  W
}

# In-sample calendar regressors: one column per occurrence that is more than
# half observed (two-level effect) or a single column (constant effect)
.cal_fit_X <- function(holidays, win, eff, y) {
  blocks <- lapply(names(holidays), function(nm) {
    W <- .cal_block(holidays[[nm]], win[[nm]], y, nm)
    if (eff[[nm]] == "two-level") W[, colSums(W) > 0.5, drop = FALSE]
    else matrix(rowSums(W), ncol = 1, dimnames = list(NULL, nm))
  })
  list(X = do.call(cbind, blocks),
       cols = setNames(lapply(blocks, colnames), names(holidays)))
}
