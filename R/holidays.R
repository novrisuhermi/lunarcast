#' Dates of moving holidays in Indonesia
#'
#' Dates of Eid al-Adha (Idul Adha), Eid al-Fitr (Idul Fitri) and the Chinese
#' New Year (Imlek) as observed in Indonesia, 2004--2027. The Islamic dates
#' are those decided by the Government of Indonesia after the official moon
#' sighting (sidang isbat); both 2006 Eid al-Adha occurrences (10 January and
#' 31 December) are included. The 2027 Islamic dates follow the joint
#' ministerial decree (SKB) and may move by one day after the moon sighting.
#' For other holidays or countries, pass your own vector of dates to
#' [lunarcast()].
#'
#' @param holiday One of `"idul_adha"`, `"idul_fitri"` or `"imlek"`.
#' @return A sorted `Date` vector.
#' @seealso [easter_dates()] for Easter-based holidays and [holiday_window()]
#'   to turn dates into regressors.
#' @examples
#' holiday_dates("idul_adha")
#' # Eid al-Adha moves about 11 days earlier every year
#' diff(holiday_dates("idul_adha"))
#' @export
holiday_dates <- function(holiday = c("idul_adha", "idul_fitri", "imlek")) {
  holiday <- match.arg(holiday)
  as.Date(switch(holiday,
    idul_adha = c(
      "2004-02-01", "2005-01-21", "2006-01-10", "2006-12-31", "2007-12-20",
      "2008-12-08", "2009-11-27", "2010-11-17", "2011-11-06", "2012-10-26",
      "2013-10-15", "2014-10-05", "2015-09-24", "2016-09-12", "2017-09-01",
      "2018-08-22", "2019-08-11", "2020-07-31", "2021-07-20", "2022-07-10",
      "2023-06-29", "2024-06-17", "2025-06-06", "2026-05-27", "2027-05-17"),
    idul_fitri = c(
      "2004-11-14", "2005-11-03", "2006-10-24", "2007-10-13", "2008-10-01",
      "2009-09-20", "2010-09-10", "2011-08-31", "2012-08-19", "2013-08-08",
      "2014-07-28", "2015-07-17", "2016-07-06", "2017-06-25", "2018-06-15",
      "2019-06-05", "2020-05-24", "2021-05-13", "2022-05-02", "2023-04-22",
      "2024-04-10", "2025-03-31", "2026-03-21", "2027-03-10"),
    imlek = c(
      "2004-01-22", "2005-02-09", "2006-01-29", "2007-02-18", "2008-02-07",
      "2009-01-26", "2010-02-14", "2011-02-03", "2012-01-23", "2013-02-10",
      "2014-01-31", "2015-02-19", "2016-02-08", "2017-01-28", "2018-02-16",
      "2019-02-05", "2020-01-25", "2021-02-12", "2022-02-01", "2023-01-22",
      "2024-02-10", "2025-01-29", "2026-02-17", "2027-02-06")))
}

#' Dates of Easter and related moveable feasts
#'
#' Easter Sunday in the Gregorian calendar, computed with the anonymous
#' Gregorian algorithm (Meeus/Jones/Butcher). Related holidays follow from an
#' offset in days: `-2` for Good Friday, `39` for Ascension Day and `49` for
#' Pentecost.
#'
#' @param years Integer vector of years.
#' @param offset Days relative to Easter Sunday.
#' @return A `Date` vector.
#' @examples
#' easter_dates(2024:2027)
#' easter_dates(2024:2027, offset = -2)   # Good Friday
#' @export
easter_dates <- function(years, offset = 0) {
  yr <- as.integer(years)
  a  <- yr %% 19
  b  <- yr %/% 100
  cc <- yr %% 100
  d  <- b %/% 4
  e  <- b %% 4
  f  <- (b + 8) %/% 25
  g  <- (b - f + 1) %/% 3
  h  <- (19 * a + b - d - g + 15) %% 30
  i  <- cc %/% 4
  k  <- cc %% 4
  l  <- (32 + 2 * e + 2 * i - h - k) %% 7
  m  <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day   <- (h + l - 7 * m + 114) %% 31 + 1
  as.Date(sprintf("%04d-%02d-%02d", yr, month, day)) + offset
}
