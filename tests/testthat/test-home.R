library(testthat)
library(HOME)

# Shared test data — simple synthetic example used throughout
.age <- seq(20, 60, by = 5)
.sn  <- c(0.987, 0.967, 0.934, 0.908, 0.882, 0.835, 0.769, 0.669, 0.565)
.mn  <- rep(27, 9)
.date <- 1998.5

# ------------------------------------------------------------------------------
# om_estimate_index — basic output structure
# ------------------------------------------------------------------------------

test_that("om_estimate_index returns OrphanhoodEstimate for all methods", {
  for (mth in c("luy", "timaeus", "brass")) {
    res <- om_estimate_index(
      method          = mth,
      sex_parent      = "Female",
      age_respondent  = .age,
      p_surv          = .sn,
      mean_age_parent = .mn,
      surv_date       = .date
    )
    expect_s3_class(res, "OrphanhoodEstimate")
    expect_true(is.list(res))
    expect_named(res, c("estimates", "meta", "inputs"))
    expect_s3_class(res$estimates, "data.frame")
    expect_true(nrow(res$estimates) > 0)
    expect_true(all(c("RefYear", "Alpha", "30q30", "45q15", "e30") %in%
                      names(res$estimates)))
  }
})

test_that("om_estimate_index works for Male parent", {
  res <- om_estimate_index(
    method          = "luy",
    sex_parent      = "Male",
    age_respondent  = .age,
    p_surv          = .sn,
    mean_age_parent = rep(30, 9),
    surv_date       = .date
  )
  expect_s3_class(res, "OrphanhoodEstimate")
  expect_equal(res$meta$sex, "Male")
})

test_that("om_estimate_index accepts all valid model_family values", {
  families <- c("General", "Latin", "Chilean", "South_Asian", "Far_East_Asian",
                "West", "North", "East", "South")
  for (fam in families) {
    res <- om_estimate_index(
      method          = "luy",
      sex_parent      = "Female",
      age_respondent  = .age,
      p_surv          = .sn,
      mean_age_parent = .mn,
      surv_date       = .date,
      model_family    = fam
    )
    expect_true(inherits(res, "OrphanhoodEstimate"),
                info = paste("model_family =", fam))
  }
})

test_that("om_estimate_index rejects invalid model_family", {
  expect_error(
    om_estimate_index(
      method          = "luy",
      sex_parent      = "Female",
      age_respondent  = .age,
      p_surv          = .sn,
      mean_age_parent = .mn,
      surv_date       = .date,
      model_family    = "NotAFamily"
    ),
    regexp = "model_family"
  )
})

test_that("a scalar mean_age_parent is recycled for every method", {
  for (mth in c("luy", "timaeus", "brass")) {
    a <- suppressWarnings(om_estimate_index(mth, "Female", .age, .sn, 27, .date))
    b <- suppressWarnings(om_estimate_index(mth, "Female", .age, .sn, .mn, .date))
    expect_equal(a$estimates, b$estimates, label = mth)
  }
  expect_error(om_estimate_index("luy", "Female", .age, .sn, c(27, 28), .date),
               "mean_age_parent")
  expect_error(om_estimate_index("luy", "Female", .age, .sn[-1], .mn, .date),
               "p_surv")
})

test_that("om_estimate_index rejects mismatched num_respondents length", {
  expect_error(
    om_estimate_index(
      method          = "luy",
      sex_parent      = "Female",
      age_respondent  = .age,
      p_surv          = .sn,
      mean_age_parent = .mn,
      surv_date       = .date,
      num_respondents = c(100, 200)   # wrong length
    ),
    regexp = "num_respondents"
  )
})

test_that("30q30 estimates are in (0, 1)", {
  res <- om_estimate_index(
    method          = "luy",
    sex_parent      = "Female",
    age_respondent  = .age,
    p_surv          = .sn,
    mean_age_parent = .mn,
    surv_date       = .date
  )
  vals <- res$estimates[["30q30"]]
  vals <- vals[!is.na(vals)]
  expect_true(all(vals > 0 & vals < 1))
})

# ------------------------------------------------------------------------------
# S3 methods — OrphanhoodEstimate
# ------------------------------------------------------------------------------

test_that("print.OrphanhoodEstimate returns invisibly", {
  res <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  out <- capture.output(ret <- print(res))
  expect_identical(ret, res)
  expect_true(any(grepl("Orphanhood", out)))
})

test_that("summary.OrphanhoodEstimate returns invisibly", {
  res <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  out <- capture.output(ret <- summary(res))
  expect_identical(ret, res)
  expect_true(any(grepl("Summary|Range|Mean|Median", out)))
})

# ------------------------------------------------------------------------------
# om_sensitivity_Mn
# ------------------------------------------------------------------------------

test_that("om_sensitivity_Mn returns OrphanhoodSensitivity", {
  res  <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens <- om_sensitivity_Mn(res, range_m = seq(-1, 1, by = 0.5))
  expect_s3_class(sens, "OrphanhoodSensitivity")
  expect_s3_class(sens, "OrphanhoodSensitivityBase")
  expect_named(sens, c("data", "meta"))
  expect_true("Offset_M" %in% names(sens$data))
})

test_that("print.OrphanhoodSensitivity works", {
  res  <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens <- om_sensitivity_Mn(res, range_m = c(-1, 0, 1))
  out  <- capture.output(ret <- print(sens))
  expect_identical(ret, sens)
  expect_true(any(grepl("Sensitivity|Mean Age", out)))
})

test_that("summary.OrphanhoodSensitivity works", {
  res  <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens <- om_sensitivity_Mn(res, range_m = c(-1, 0, 1))
  out  <- capture.output(ret <- summary(sens, index = "30q30"))
  expect_identical(ret, sens)
  expect_true(any(grepl("30q30|Spread|range", out)))
})

test_that("plot.OrphanhoodSensitivity returns a ggplot", {
  res  <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens <- om_sensitivity_Mn(res, range_m = c(-1, 0, 1))
  p    <- plot(sens, index = "30q30")
  expect_s3_class(p, "ggplot")
})

# ------------------------------------------------------------------------------
# om_sensitivity_modelLT
# ------------------------------------------------------------------------------

test_that("om_sensitivity_modelLT returns OrphanhoodSensitivityFamily", {
  res      <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens_fam <- om_sensitivity_modelLT(res, type = "UN")
  expect_s3_class(sens_fam, "OrphanhoodSensitivityFamily")
  expect_s3_class(sens_fam, "OrphanhoodSensitivityBase")
  expect_true("Family" %in% names(sens_fam$data))
})

test_that("print.OrphanhoodSensitivityFamily works", {
  res      <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens_fam <- om_sensitivity_modelLT(res, type = "CD")
  out      <- capture.output(ret <- print(sens_fam))
  expect_identical(ret, sens_fam)
  expect_true(any(grepl("Sensitivity|Family|System", out)))
})

test_that("summary.OrphanhoodSensitivityFamily works", {
  res      <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens_fam <- om_sensitivity_modelLT(res, type = "CD")
  out      <- capture.output(ret <- summary(sens_fam, index = "30q30"))
  expect_identical(ret, sens_fam)
  expect_true(any(grepl("30q30|Spread|Family", out)))
})

test_that("plot.OrphanhoodSensitivityFamily returns a ggplot", {
  res      <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  sens_fam <- om_sensitivity_modelLT(res, type = "UN")
  p        <- plot(sens_fam, index = "30q30")
  expect_s3_class(p, "ggplot")
})

# ------------------------------------------------------------------------------
# om_plot_linearity
# ------------------------------------------------------------------------------

test_that("om_plot_linearity returns a ggplot", {
  res <- om_estimate_index("luy", "Female", .age, .sn, .mn, .date)
  p   <- om_plot_linearity(res)
  expect_s3_class(p, "ggplot")
})

test_that("om_plot_linearity rejects non-OrphanhoodEstimate input", {
  expect_error(om_plot_linearity(list()), regexp = "OrphanhoodEstimate")
})

# ------------------------------------------------------------------------------
# om_sensitivity_Mn without object — direct argument passing
# ------------------------------------------------------------------------------

test_that("om_sensitivity_Mn works without an object via ...", {
  sens <- om_sensitivity_Mn(
    method          = "luy",
    sex_parent      = "Female",
    age_respondent  = .age,
    p_surv          = .sn,
    mean_age_parent = .mn,
    surv_date       = .date,
    range_m         = c(-0.5, 0, 0.5)
  )
  expect_s3_class(sens, "OrphanhoodSensitivity")
})
