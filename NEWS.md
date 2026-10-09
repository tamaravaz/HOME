# HOME 0.1.2

## om_calibrate_luy() rewritten to reproduce Luy's calculation workbooks

The function was rebuilt from Luy's calculation workbooks
(`Multipliers_Italy_<year>_<SEX>_NEW_REF_YEARS.xls`, which produce Tables
A.5-A.20 of Luy 2009). Fed with Luy's inputs, the engine reproduces his
reference periods and mean-age tables exactly and W(n) to within about 1 %
up to respondent age group 45. Main changes:

* Inputs are four data frames in a fixed long format prepared by the user
  from any source: `asfr` (age, year, rate), `cohort_qx` (age, cohort, qx),
  `pop_age` (age, year, n), `period_lx` (year, age, lx or qx). The package
  does not read files.
* Parental survival is built, as in Luy, by averaging the cohorts'
  probabilities of dying with fixed weights (`mixing = "luy"`), not by
  mixing survival curves. `mixing = "exact"` gives the proper mixture.
* Unobserved ages are not filled in. Ages for which a parental cohort has no
  probability of dying (e.g. after the end of its series) are kept as `NA`
  and get no weight: the average in each year is taken over the cohorts
  observed in that year. Luy's workbooks filled these cells with q = 0
  (1998) or q = 1 (2003); results differ from his published tables from
  respondent age group 50 onwards. If no parental cohort is observed in
  some year of exposure, W(n), a(n) and b(n) are `NA` with a warning;
  implausible W(n) are also set to `NA` with a warning.
* No re-levelling to the observed S(n): W(n) and t(n) come from the
  reconstructed survival only. The argument `p_surv` was removed.
* Exposure is n + 3 years; parental cohort = floor(T) - n - cohort_lag - x
  with `cohort_lag = 4` (documented with reference to Luy 2009);
  reference period = T - K + mean death time measured at mid-year;
  period l(x) interpolated linearly between calendar years.
* Fertility: Schmertmann parameters read off the averaged ASFR (P, f(P), H),
  QS point densities at integer ages, mean age = sum (x+0.5) N f / sum N f,
  P/H stepped (+2/+1 up, -1/-2 down) and Brass-Gompertz fine-tuning.
  Paternal schedules are modelled on the female age scale and shifted by
  `male_shift`.
* New `weighting` (`"Nf"` = 2010 supplement / built-in tables, `"f"` = Luy
  2009), `cohort_lag`, `period_lx` and `smoothing` arguments.
* a(n), b(n) from Brass-logit mortality scenarios S x 0.5-1.5.
* New output table `mac`: mean age at childbirth of all parents given the
  mean of surviving parents (Luy's Tables A.5-A.8).

## New functions

* `om_luy_data_needs()` lists the years, ages and cohorts each input of
  `om_calibrate_luy()` must cover for a given survey date. Ages are capped
  at the open age interval of the life tables (`max_age = 110`).
* `om_luy_validate_inputs()` checks prepared inputs (columns, numeric whole
  ages/years, duplicated keys, ASFR per woman, q between 0 and 1) and, given
  a survey date, whether they cover the years, ages and cohorts needed.
* `om_luy_intermediates()` extracts every intermediate quantity of a
  calibration -- f'(x), N(x), weights, q[x,k], q-bar, L_k, share of observed
  weights and a per-mean-age summary -- laid out like Luy's workbooks.

## Renamed and removed

* `om_sensitivity_family()` was renamed `om_sensitivity_modelLT()`.
* The deprecated alias `om_sensitivity()` was removed; use
  `om_sensitivity_Mn()`.

## Other

* `om_estimate_index()`: a scalar `mean_age_parent` is recycled for all
  methods; length mismatches of `p_surv` / `mean_age_parent` are errors.
* `tests/testthat.R` now calls `test_check("HOME")`; previously no test ran
  during R CMD check.
* `.qs_phi()` returns 0 beyond the spline end point beta.
