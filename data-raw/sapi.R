# Google Trends index for "sapi" (monthly), exported as search.csv
raw  <- read.csv("data-raw/search.csv")
sapi <- ts(raw$Search, start = c(2004, 1), frequency = 12)
usethis::use_data(sapi, overwrite = TRUE)
