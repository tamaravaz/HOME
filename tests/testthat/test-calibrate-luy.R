# ------------------------------------------------------------------------------
# om_calibrate_luy / .luy_engine -- regression against Luy's own workbook
#
# The fixture holds, for mothers of the 1998 Italian survey (n = 20 and 60),
# Luy's modelled schedules f'(x), the cohort q[x, k] his sheets use, the
# period life tables, and his published Tables A.5, A.9, A.13 and A.17
# (Luy 2009, EDRP 2009-3), extracted from
# Multipliers_Italy_1998_FEMALES_NEW_REF_YEARS.xls.
# ------------------------------------------------------------------------------

fx <- readRDS(test_path("fixtures", "luy_1998_female.rds"))
lx_at <- HOME:::.luy_period_lx_fun(fx$period_lx, NULL)

test_that(".luy_engine reproduces Luy's reference periods, MAC and W(n)", {
  for (n in c(20, 60)) {
    d <- fx[[as.character(n)]]
    e <- HOME:::.luy_engine(d$w, d$q, d$ages, n, 1998.5, lx_at, d$acb)
    expect_equal(e$ref, d$ref, tolerance = 1e-8)
    expect_equal(e$mac, d$mac, tolerance = 1e-4)
    expect_lt(max(abs(e$W / d$W - 1)), 0.005)
    expect_lt(max(abs(e$a - d$a)), 0.05)
    expect_lt(max(abs(e$b - d$b)), 0.05)
  }
})

test_that("Luy's q-averaging differs from exact mixing at old ages", {
  d <- fx[["60"]]
  luy <- HOME:::.luy_engine(d$w, d$q, d$ages, 60, 1998.5, lx_at, d$acb,
                            mixing = "luy")
  ex  <- HOME:::.luy_engine(d$w, d$q, d$ages, 60, 1998.5, lx_at, d$acb,
                            mixing = "exact")
  expect_true(all(ex$S >= luy$S - 1e-12))
  expect_gt(ex$S[9] / luy$S[9], 2)        # mean age 30: factor ~2.8
})

test_that("QS point densities reproduce Luy's schedule (P = 27, H = 39.18)", {
  # 1933-37 baseline schedule, Gompertz alpha 0.037 -> column 'ACB 30'
  ages <- 15:49
  f <- HOME:::.qs_phi(ages, HOME:::.qs_spline(14, 27, 39.18))
  Fx <- cumsum(f) / sum(f)
  g <- diff(c(0, Fx^exp(-0.037)))
  expect_equal(g, fx[["60"]]$w[, 9], tolerance = 1e-6)
})

test_that("om_calibrate_luy returns tables in the built-in layout", {
  ages <- 15:49
  yrs  <- 1920:1990
  asfr <- expand.grid(age = ages, year = yrs)
  asfr$rate <- 0.12 * dnorm(asfr$age, 28, 5.5) / dnorm(28, 28, 5.5)
  coh <- expand.grid(age = 0:110, cohort = 1860:1990)
  coh$qx <- pmin(1, 0.00004 * exp(0.095 * coh$age) *
                   exp(-0.01 * (coh$cohort - 1900)))
  pop <- expand.grid(age = 0:110, year = yrs)
  pop$n <- 1000 * exp(-0.01 * pop$age)

  res <- om_calibrate_luy("Female", asfr = asfr, cohort_qx = coh,
                          pop_age = pop, survey_date = 2003.9,
                          age_respondent = c(20, 40, 60))
  expect_named(res$Female, c("wn", "an", "bn", "mac"))
  expect_equal(names(res$Female$wn), c("Age_Group", as.character(22:35)))
  W <- as.matrix(res$Female$wn[, -1])
  expect_true(all(is.finite(W)))
  expect_true(all(W[1, ] > 0.9 & W[1, ] < 1.1))
  # older mothers -> larger W(n) at a given respondent age
  expect_true(all(diff(W[3, ]) > 0))
  # MAC: all-parent mean >= surviving-parent mean
  mac <- as.matrix(res$Female$mac[, -1])
  expect_true(all(sweep(mac, 2, 22:35) >= -1e-8))
  # coefficients plug into om_estimate_index
  est <- om_estimate_index(method = "luy", sex_parent = "Female",
                           age_respondent = c(20, 40, 60),
                           p_surv = c(0.98, 0.85, 0.3),
                           mean_age_parent = rep(28, 3), surv_date = 2003.9,
                           custom_coef_luy = res)
  expect_s3_class(est, "OrphanhoodEstimate")
})

test_that("unobserved q[x, k] get no weight (no closure rule)", {
  w <- matrix(c(0.25, 0.25, 0.5), 3, 1)
  q <- matrix(c(0.01, 0.02, 0.03,
                0.10, NA,   0.30,
                0.20, NA,   NA), 3, 3)
  lx_at <- function(t) rep(1, 120)
  e <- HOME:::.luy_engine(w, q, ages = 30:32, n = 0, survey_date = 2000,
                          lx_at = lx_at, acb = 30)
  # rows: parental ages 30:32; columns: k = 1:3
  # year 2: average over the observed cohorts (ages 30 and 32) only
  expect_equal(e$q_bar[2, 1], (0.25 * 0.10 + 0.5 * 0.30) / 0.75)
  # year 3: only the cohort aged 30 observed
  expect_equal(e$q_bar[3, 1], 0.20)
  expect_equal(e$w_observed[, 1], c(1, 0.75, 0.25))
  # NA is not read as q = 0: the result differs from filling with zeros
  q0 <- q; q0[is.na(q0)] <- 0
  e0 <- HOME:::.luy_engine(w, q0, 30:32, 0, 2000, lx_at, 30)
  expect_gt(e0$S, e$S)
  # a year with no observed cohort gives no survival
  q[, 3] <- NA
  expect_true(is.na(HOME:::.luy_engine(w, q, 30:32, 0, 2000, lx_at, 30)$S))
})

test_that("om_calibrate_luy leaves extinct ages out and flags short series", {
  asfr <- expand.grid(age = 15:49, year = 1920:1990)
  asfr$rate <- 0.1 * dnorm(asfr$age, 28, 5.5) / dnorm(28, 28, 5.5)
  coh <- expand.grid(age = 0:110, cohort = 1860:1990)
  coh$qx <- pmin(1, 0.00004 * exp(0.095 * coh$age))
  # oldest cohorts' tables stop at age 100 (q = 1 there): extinct
  coh <- coh[!(coh$cohort < 1900 & coh$age > 100), ]
  coh$qx[coh$cohort < 1900 & coh$age == 100] <- 1
  coh <- coh[coh$cohort + coh$age <= 2005, ]
  cal <- suppressMessages(om_calibrate_luy("Female", asfr, coh,
                                           survey_date = 2003.9,
                                           age_respondent = c(20, 60)))
  expect_true(all(is.finite(as.matrix(cal$Female$wn[, -1]))))
  wo <- om_luy_intermediates(cal, "w_observed", n = 60)
  expect_true(all(wo <= 1 + 1e-12) && any(wo < 1))
  # cohort data ending before calendar year floor(T) - 2: W(n) is NA
  short <- coh[coh$cohort + coh$age <= 1995, ]
  expect_warning(
    cal2 <- suppressMessages(om_calibrate_luy("Female", asfr, short,
                                              survey_date = 2003.9,
                                              age_respondent = 20)),
    "calendar year 2001")
  expect_true(all(is.na(as.matrix(cal2$Female$wn[, -1]))))
})

test_that("om_luy_data_needs caps the oldest age at the open age interval", {
  d <- om_luy_data_needs(2024, "Female")
  expect_equal(d$cohort_ages[d$n == 60], "15-110")
  expect_equal(d$cohort_ages[d$n == 20], "15-71")
  expect_output(print(d), "ages up to 110\\+")
  expect_equal(om_luy_data_needs(2024, "Female", max_age = 120)$cohort_ages[9],
               "15-111")
})

test_that("om_luy_data_needs follows Luy's timing conventions", {
  d <- om_luy_data_needs(1998.5, "Female")
  expect_s3_class(d, "LuyDataNeeds")
  expect_equal(d$birth_years[d$n == 60], "1933-1937")      # Table A.1 notes
  expect_equal(d$cohorts[d$n == 20], "1925-1959")          # Coh M20: B122 "=BN56"
  o <- attr(d, "overall")
  expect_equal(o$fertility_years, c(1933, 1977))
  m <- om_luy_data_needs(2003.9, "Male")
  expect_equal(m$pop_ages[1], "19-53")
  expect_output(print(m), "fathers")
})

test_that("om_luy_intermediates returns f'(x), q[x,k] and the survival", {
  ages <- 15:49
  asfr <- expand.grid(age = ages, year = 1920:1990)
  asfr$rate <- 0.12 * dnorm(asfr$age, 28, 5.5) / dnorm(28, 28, 5.5)
  coh <- expand.grid(age = 0:110, cohort = 1860:1990)
  coh$qx <- pmin(1, 0.00004 * exp(0.095 * coh$age))
  pop <- expand.grid(age = 0:110, year = 1920:1990)
  pop$n <- 1000 * exp(-0.01 * pop$age)
  cal <- om_calibrate_luy("Female", asfr, coh, pop, survey_date = 1998.5,
                          age_respondent = c(20, 60))
  fp <- om_luy_intermediates(cal, "f_prime", n = 60)
  expect_equal(dim(fp), c(35L, 14L))
  expect_equal(unname(colSums(fp)), rep(1, 14))
  q <- om_luy_intermediates(cal, "q", n = 60)
  expect_equal(dim(q), c(35L, 63L))
  L <- om_luy_intermediates(cal, "L", n = 60)
  s <- om_luy_intermediates(cal, "summary", n = 60)
  expect_equal(unname(L[64, ]), s$S_model)
  expect_equal(s$W, unname(as.numeric(cal$Female$wn[2, -1])))
  lg <- om_luy_intermediates(cal, "q", format = "long")
  expect_true(all(c("n", "age", "k", "value", "cohort") %in% names(lg)))
  expect_equal(lg$cohort[lg$n == 60 & lg$age == 49 & lg$k == 1][1], 1998 - 60 - 4 - 49)
})

test_that("om_luy_validate_inputs accepts the standard format", {
  asfr <- expand.grid(age = 15:49, year = 1930:2001)
  asfr$rate <- 0.1 * dnorm(asfr$age, 28, 5) / dnorm(28, 28, 5)
  coh <- expand.grid(age = 0:110, cohort = 1860:1990)
  coh$qx <- pmin(1, 0.00004 * exp(0.095 * coh$age))
  pop <- expand.grid(age = 0:110, year = 1920:2003)
  pop$n <- 1000
  per <- expand.grid(age = 0:110, year = 1950:2003)
  per$lx <- 1e5 * exp(-0.0001 * per$age^2)
  s <- expect_silent(om_luy_validate_inputs(asfr, coh, pop, per, verbose = FALSE))
  expect_equal(s$input, c("asfr", "cohort_qx", "pop_age", "period_lx"))
  s <- expect_silent(om_luy_validate_inputs(asfr, coh, pop, per, survey_date = 2003.9,
                                            verbose = FALSE))
  expect_length(attr(s, "gaps"), 0)
})

test_that("om_luy_validate_inputs catches common preparation errors", {
  asfr <- data.frame(age = 15:16, year = 1990, rate = c(0.05, 0.1))
  expect_error(om_luy_validate_inputs(asfr = transform(asfr, rate = rate * 1000),
                                      verbose = FALSE), "per 1000")
  expect_error(om_luy_validate_inputs(asfr = setNames(asfr, c("Age", "Year", "ASFR")),
                                      verbose = FALSE), "lacks column")
  expect_error(om_luy_validate_inputs(asfr = rbind(asfr, asfr), verbose = FALSE),
               "duplicated")
  coh <- data.frame(age = c("0", "110+"), cohort = 1900, qx = c(0.1, 1))
  expect_error(om_luy_validate_inputs(cohort_qx = coh, verbose = FALSE), "numeric")
  coh <- data.frame(age = 0:1, cohort = 1900, qx = c(0.1, 1.5))
  expect_error(om_luy_validate_inputs(cohort_qx = coh, verbose = FALSE), "between 0 and 1")
  coh$qx[2] <- NA
  expect_warning(om_luy_validate_inputs(cohort_qx = coh, verbose = FALSE), "NA")
  expect_warning(om_luy_validate_inputs(asfr = asfr, survey_date = 2003.9,
                                        verbose = FALSE), "asfr: years")
})

test_that("om_luy_validate_inputs flags cohort series that stop too early", {
  coh <- expand.grid(age = 0:110, cohort = 1880:1970)
  coh$qx <- pmin(1, 0.00004 * exp(0.095 * coh$age))
  # HMD-like: complete life tables only up to cohort 1932
  lt <- coh[coh$cohort <= 1932, ]
  expect_warning(om_luy_validate_inputs(cohort_qx = lt, survey_date = 2003.9,
                                        verbose = FALSE), "1933-1964")
  # young cohorts observed only up to 1995 (survey needs up to 2001)
  trunc <- coh[coh$cohort <= 1932 | coh$cohort + coh$age <= 1995, ]
  expect_warning(om_luy_validate_inputs(cohort_qx = trunc, survey_date = 2003.9,
                                        verbose = FALSE),
                 "end before calendar year 2001")
  # observed up to 2023: complete
  ok <- coh[coh$cohort <= 1932 | coh$cohort + coh$age <= 2023, ]
  s <- expect_silent(om_luy_validate_inputs(cohort_qx = ok, survey_date = 2003.9,
                                            verbose = FALSE))
  expect_length(attr(s, "gaps"), 0)
})
