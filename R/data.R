#' Google Trends search interest for "sapi"
#'
#' Monthly Google Trends index (0--100) for the Indonesian keyword "sapi"
#' (cattle), January 2004 to November 2025. Interest peaks around Eid
#' al-Adha, when cattle are bought for the sacrifice (qurban), so the series
#' is a textbook case of a moving-holiday effect that also changes over the
#' years.
#'
#' @format A monthly `ts` of length 263.
#' @source Google Trends (<https://trends.google.com>), retrieved by the
#'   package author.
#' @examples
#' plot(sapi)
"sapi"
