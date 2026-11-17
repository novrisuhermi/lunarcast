# lunarcast 0.1.1

* The trend type now follows KPSS tests by default (`trend = "auto"`): a
  stochastic trend is always a candidate and a deterministic linear trend only
  when the tests indicate trend stationarity. This avoids extrapolating a
  straight line when the slope of a series has changed. `trend = "cv"`
  compares both trend types by cross-validation, as in 0.1.0.
* Every ARIMA model, including those refitted during cross-validation, must
  be stationary and invertible; a fit that is not is re-estimated by maximum
  likelihood. Fitting no longer produces "NaNs produced" or "MA part of model
  is not invertible" warnings.
* The precision-weighted level-2 rule (`wmean`) ignores occurrences without
  a standard error.
* Rolling-origin results for `sapi` updated: RMSE 4.94 (was 5.00); the 2025
  forecast error drops from 4.40 to 1.83.

# lunarcast 0.1.0

First release.

## Models

* `lunarcast()` fits the automatic two-level calendar variation model:
  window regressors for moving holidays, multiplicative effects with a
  log-scale mapping of window shares, occurrence-specific (two-level) or
  constant effects, and a regression with ARIMA errors whose trend and
  seasonal structure is chosen by calendar-aware cross-validation with a
  one-standard-error rule. Outliers (AO, LS, TC) can be detected outside
  holiday periods.
* `forecast()` builds future holiday regressors from the dates, forecasts the
  effect of new occurrences with the selected level-2 rule and supports
  scenarios through `level2`.
* `holiday_effects()` returns the effect of every holiday occurrence.
* `calvar_classic()` and `calvar_classic_all()` fit the classical calendar
  variation model (holiday-month dummy, constant effect) with the six
  structures m1 to m6.

## Holidays and regressors

* `holiday_dates()` gives Indonesian dates of Eid al-Adha, Eid al-Fitr and the
  Chinese New Year (2004 to 2027); `easter_dates()` computes Easter-based
  holidays; `holiday_window()` turns any dates into window regressors.

## Evaluation

* `rolling_origin()`, `rolling_summary()` and `forecast_accuracy()`.

## Data and documentation

* Data set `sapi` (Google Trends interest in "sapi", 2004 to 2025).
* Vignettes: getting started, theory, classical model versus lunarcast, and a
  guide in Indonesian.
