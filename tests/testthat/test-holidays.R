test_that("holiday dates are sorted and complete", {
  for (h in c("idul_adha", "idul_fitri", "imlek")) {
    d <- holiday_dates(h)
    expect_s3_class(d, "Date")
    expect_false(is.unsorted(d))
  }
  expect_length(holiday_dates("idul_adha"), 25)          # 2006 twice
  expect_true(all(diff(holiday_dates("idul_adha")) %in% 351:356))
})

test_that("Easter dates match known values", {
  expect_equal(easter_dates(c(2000, 2019, 2024, 2025, 2026, 2027)),
               as.Date(c("2000-04-23", "2019-04-21", "2024-03-31",
                         "2025-04-20", "2026-04-05", "2027-03-28")))
  expect_equal(easter_dates(2025, offset = -2), as.Date("2025-04-18"))
})
