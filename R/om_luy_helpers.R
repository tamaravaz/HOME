# ==============================================================================
# om_luy_helpers.R
#
# Helpers around om_calibrate_luy():
#   om_luy_data_needs()     which years, ages and cohorts each input must cover
#   om_luy_intermediates()  extract the intermediate quantities of a calibration
#                           (f'(x), N(x), weights, q[x,k], survival, ...) laid
#                           out like Luy's workbooks, for comparison
#   om_luy_validate_inputs() check that user-prepared inputs are in the
#                           standard long format and cover what is needed
#
# Reading data files is the user's job: om_calibrate_luy() takes four data
# frames in a fixed long format (see ?om_luy_validate_inputs).
# ==============================================================================

#' Data Requirements for om_calibrate_luy()
#'
#' Lists, for a given survey date, the calendar years, ages and cohorts that
#' each input of \code{\link{om_calibrate_luy}} must cover, following the
#' timing conventions of Luy (2009) and his calculation workbooks.
#'
#' @details For respondent age group \eqn{n} (respondents aged \eqn{n} to
#' \eqn{n+4}), survey date \eqn{T}, exposure \eqn{K = n + 3} years and
#' parental age \eqn{x} at the respondent's birth (Luy 2009, pp. 10--18):
#' \itemize{
#'   \item \code{asfr} and \code{pop_age}: the respondents' five birth years
#'     \eqn{\lfloor T \rfloor - n - 5} to \eqn{\lfloor T \rfloor - n - 1};
#'     ASFR at ages 15--49 (female rates, also for fathers), population at
#'     the parental ages (15--49 mothers, 19--53 fathers).
#'   \item \code{cohort_qx}: parental cohort \eqn{\lfloor T \rfloor - n -
#'     cohort\_lag - x} (see \code{cohort_lag} in
#'     \code{\link{om_calibrate_luy}}), from age \eqn{x} to \eqn{x + K - 1 =
#'     x + n + 2}, i.e. from calendar year \eqn{\lfloor T \rfloor - n - 4}
#'     to \eqn{\lfloor T \rfloor - 2}.
#'   \item \code{period_lx}: ages 30 and \eqn{33 + n} (the ratio
#'     \eqn{l(33+n)/l(30)}, Luy 2009, eq. 1) in the years around the
#'     reference period. The reference period depends on mortality; it always
#'     lies between \eqn{T - K} and \eqn{T}, and in practice within the
#'     "typical" range shown (\eqn{T - K + 0.55K} to \eqn{T - K + 0.9K}, plus
#'     one year for the linear interpolation between calendar years).
#' }
#' \strong{Oldest age.} The age reached by the oldest parents at the end of
#' the exposure, \eqn{x + n + 2}, can exceed the last age of a life table
#' (for \eqn{n = 60}: 111 for mothers, 115 for fathers). It is capped at
#' \code{max_age}: the last age of a life table is an open interval (e.g.
#' 110+, ages 0 to 110+ are 111 age classes) in which \eqn{q = 1}, so no
#' older age exists in the data or is needed. Ages without data are not
#' filled in by \code{om_calibrate_luy()}; they get no weight (see
#' Details there).
#'
#' @inheritParams om_calibrate_luy
#' @param max_age Last age of the cohort life tables, an open interval
#'   (default 110, as in the Human Mortality Database).
#' @return A data frame of class \code{"LuyDataNeeds"} with one row per
#'   respondent age group and the columns
#'   \describe{
#'     \item{\code{n}}{respondent age group (lower bound).}
#'     \item{\code{birth_years}}{respondents' birth years: years needed in
#'       \code{asfr} and \code{pop_age}.}
#'     \item{\code{asfr_ages}}{ages needed in \code{asfr} (female ages).}
#'     \item{\code{pop_ages}}{ages needed in \code{pop_age} (parent's sex).}
#'     \item{\code{cohorts}}{parental birth cohorts needed in
#'       \code{cohort_qx}.}
#'     \item{\code{cohort_ages}}{ages of those cohorts needed in
#'       \code{cohort_qx} (capped at \code{max_age}).}
#'     \item{\code{cohort_last_year}}{last calendar year the cohort series
#'       must reach, \eqn{\lfloor T \rfloor - 2}.}
#'     \item{\code{period_ages}}{ages read from \code{period_lx}: 30 and
#'       \eqn{33 + n}.}
#'     \item{\code{period_years_bounds}}{years within which the reference
#'       period always lies.}
#'     \item{\code{period_years_typical}}{years within which it usually
#'       lies.}
#'   }
#'   The attribute \code{"overall"} holds the ranges over all age groups,
#'   which the print method shows first.
#' @references
#' Luy, M. (2009). Estimating mortality differentials in developed
#' populations from survey information on maternal and paternal orphanhood.
#' \emph{European Demographic Research Papers} 2009-3. Vienna Institute of
#' Demography.
#' @examples
#' om_luy_data_needs(2003.9, "Female")
#' om_luy_data_needs(1998.5, "Male")
#' @seealso \code{\link{om_calibrate_luy}}, \code{\link{om_luy_validate_inputs}}
#' @export
om_luy_data_needs <- function(survey_date,
                              sex_parent     = c("Female", "Male"),
                              age_respondent = seq(20, 60, by = 5),
                              cohort_lag     = 4,
                              male_shift     = 4,
                              age_range      = NULL,
                              max_age        = 110) {
  sex_parent <- match.arg(sex_parent)
  shift <- if (sex_parent == "Male") male_shift else 0
  if (is.null(age_range)) age_range <- c(15, 49) + shift
  Tf <- floor(survey_date)
  n  <- age_respondent
  K  <- n + 3
  out <- data.frame(
    n                = n,
    birth_years      = sprintf("%d-%d", Tf - n - 5, Tf - n - 1),
    asfr_ages        = sprintf("%d-%d", age_range[1] - shift, age_range[2] - shift),
    pop_ages         = sprintf("%d-%d", age_range[1], age_range[2]),
    cohorts          = sprintf("%d-%d", Tf - n - cohort_lag - age_range[2],
                               Tf - n - cohort_lag - age_range[1]),
    cohort_ages      = sprintf("%d-%d", age_range[1],
                               pmin(age_range[2] + n + 2, max_age)),
    cohort_last_year = Tf - n - cohort_lag + n + 2,
    period_ages      = sprintf("30, %d", 33 + n),
    period_years_bounds  = sprintf("%d-%d", floor(survey_date - K), Tf),
    period_years_typical = sprintf("%d-%d", floor(survey_date - K + 0.55 * K),
                                   floor(survey_date - K + 0.9 * K) + 1),
    stringsAsFactors = FALSE)
  attr(out, "overall") <- list(
    survey_date = survey_date, sex_parent = sex_parent,
    fertility_years = c(Tf - max(n) - 5, Tf - min(n) - 1),
    population_years = c(Tf - max(n) - 5, Tf - min(n) - 1),
    cohorts = c(Tf - max(n) - cohort_lag - age_range[2],
                Tf - min(n) - cohort_lag - age_range[1]),
    cohort_max_age = min(age_range[2] + max(n) + 2, max_age),
    max_age = max_age,
    cohort_last_year = max(out$cohort_last_year),
    period_years = c(floor(survey_date - max(K)), Tf),
    period_years_typical = c(min(floor(survey_date - K + 0.55 * K)),
                             max(floor(survey_date - K + 0.9 * K) + 1)))
  class(out) <- c("LuyDataNeeds", "data.frame")
  out
}

#' @rdname om_luy_data_needs
#' @param x An object of class \code{"LuyDataNeeds"}.
#' @param ... Ignored.
#' @export
print.LuyDataNeeds <- function(x, ...) {
  o <- attr(x, "overall")
  par <- if (o$sex_parent == "Female") "mothers" else "fathers"
  cat(sprintf("Data needed by om_calibrate_luy() -- %s, survey date %s\n\n",
              par, format(o$survey_date)))
  cat(sprintf("  asfr       (female ASFR)        years %d-%d\n",
              o$fertility_years[1], o$fertility_years[2]))
  cat(sprintf("  pop_age    (%s population)   years %d-%d\n",
              if (o$sex_parent == "Female") "female" else "male  ",
              o$population_years[1], o$population_years[2]))
  cat(sprintf("  cohort_qx  (cohort tables)      cohorts %d-%d, through calendar year %d (ages up to %s)\n",
              o$cohorts[1], o$cohorts[2], o$cohort_last_year,
              if (o$cohort_max_age >= o$max_age) paste0(o$max_age, "+")
              else o$cohort_max_age))
  cat(sprintf("  period_lx  (period tables)      years %d-%d (typically %d-%d)\n\n",
              o$period_years[1], o$period_years[2],
              o$period_years_typical[1], o$period_years_typical[2]))
  cat("By respondent age group:\n")
  df <- x; class(df) <- "data.frame"; attr(df, "overall") <- NULL
  print(df, row.names = FALSE)
  invisible(x)
}


#' Extract the Intermediate Quantities of a Luy Calibration
#'
#' Returns the intermediate quantities computed by
#' \code{\link{om_calibrate_luy}} for each respondent age group, arranged
#' like the sheets of Luy's workbooks (Multipliers_*_NEW_REF_YEARS.xls), so
#' that they can be compared cell by cell with his values.
#'
#' @param cal Output of \code{om_calibrate_luy()}.
#' @param what Which quantity:
#'   \describe{
#'     \item{\code{"summary"}}{per mean age: model S(n), reference year,
#'       t(n), period l(33+n)/l(30), W(n), mean age of all parents,
#'       Schmertmann P, H and Gompertz alpha (sheet rows 4-9, 769-774).}
#'     \item{\code{"f_prime"}}{modelled schedules f(x)/sum f, parental age x
#'       by mean age (Luy's "MODELED f(x)/TFR", sheet rows 14-48).}
#'     \item{\code{"weights"}}{weights actually used, f' or N f'
#'       normalised (equal to \code{f_prime} when \code{weighting = "f"}).}
#'     \item{\code{"N"}}{population age structure N(x) (average over the
#'       birth years).}
#'     \item{\code{"f_empirical"}}{averaged empirical ASFR.}
#'     \item{\code{"q"}}{cohort probabilities of dying q[x, k], parental age x
#'       by year k after the respondent's birth (sheet rows 55-89).}
#'     \item{\code{"q_bar"}}{weighted average q, year k by mean age (sheet row
#'       129 of each mean-age block).}
#'     \item{\code{"L"}}{reconstructed survival L_k, k = 0..K, by mean age
#'       (sheet row 130 / 10000).}
#'     \item{\code{"w_observed"}}{share of the weights whose q[x, k] is
#'       observed, year k by mean age (1 = all parental cohorts observed;
#'       unobserved ages get no weight, see \code{\link{om_calibrate_luy}}).}
#'   }
#' @param n Respondent age group(s). Default: all.
#' @param format \code{"wide"} (a matrix per age group; a list when several
#'   groups are requested) or \code{"long"} (one data frame with columns
#'   \code{n}, row and column identifiers, and \code{value}).
#' @return With \code{format = "wide"}: for one age group, the matrix (or
#'   named vector for \code{"N"} and \code{"f_empirical"}, data frame for
#'   \code{"summary"}); for several, a list of them named by age group. With
#'   \code{format = "long"}: a data frame with column \code{n}, the row and
#'   column identifiers (\code{age}, \code{k}, \code{mean_age}; for
#'   \code{"q"} also the parental \code{cohort}) and \code{value}. The
#'   \code{"summary"} columns are \code{mean_age}, \code{S_model},
#'   \code{ref_year}, \code{t_n}, \code{lx_ratio}, \code{W}, \code{mac},
#'   \code{P}, \code{H} and \code{gompertz_alpha}.
#' @references
#' Luy, M. (2009). Estimating mortality differentials in developed
#' populations from survey information on maternal and paternal orphanhood.
#' \emph{European Demographic Research Papers} 2009-3. Vienna Institute of
#' Demography.
#' @examples
#' # synthetic inputs in the standard long format
#' asfr <- expand.grid(age = 15:49, year = 1950:2000)
#' asfr$rate <- 0.1 * exp(-0.5 * ((asfr$age - 28) / 5.5)^2)
#' cohort_qx <- expand.grid(age = 0:110, cohort = 1900:1990)
#' cohort_qx$qx <- pmin(1, 0.00005 * exp(0.095 * cohort_qx$age))
#' cohort_qx <- cohort_qx[cohort_qx$cohort + cohort_qx$age <= 2003, ]
#' pop_age <- expand.grid(age = 0:110, year = 1950:2000)
#' pop_age$n <- 1e5 * exp(-0.01 * pop_age$age)
#' period_lx <- expand.grid(age = 0:110, year = 1980:2003)
#' period_lx$lx <- 1e5 * exp(-0.00005 / 0.095 * (exp(0.095 * period_lx$age) - 1))
#' cal <- om_calibrate_luy("Female", asfr = asfr, cohort_qx = cohort_qx,
#'                         pop_age = pop_age, period_lx = period_lx,
#'                         survey_date = 2003.9, age_respondent = c(20, 40))
#' om_luy_intermediates(cal, "f_prime", n = 40)   # Luy's "MODELED f(x)/TFR"
#' om_luy_intermediates(cal, "summary", n = 40)
#' head(om_luy_intermediates(cal, "q", format = "long"))
#' @seealso \code{\link{om_calibrate_luy}}
#' @export
om_luy_intermediates <- function(cal,
                                 what = c("summary", "f_prime", "weights", "N",
                                          "f_empirical", "q", "q_bar", "L",
                                          "w_observed"),
                                 n = NULL,
                                 format = c("wide", "long")) {
  what   <- match.arg(what)
  format <- match.arg(format)
  im <- attr(cal, "intermediates")
  if (is.null(im) || !length(im)) {
    stop("'cal' has no intermediates; re-run om_calibrate_luy() ",
         "(HOME >= 0.1.2).", call. = FALSE)
  }
  if (!is.null(n)) {
    miss <- setdiff(as.character(n), names(im))
    if (length(miss)) stop("Age group(s) not in 'cal': ",
                           paste(miss, collapse = ", "), call. = FALSE)
    im <- im[as.character(n)]
  }
  get1 <- function(z) z[[what]]
  if (format == "wide") {
    res <- lapply(im, get1)
    return(if (length(res) == 1L) res[[1]] else res)
  }
  dims <- switch(what,
    f_prime = c("age", "mean_age"), weights = c("age", "mean_age"),
    q = c("age", "k"), q_bar = c("k", "mean_age"), L = c("k", "mean_age"),
    w_observed = c("k", "mean_age"),
    N = "age", f_empirical = "age", summary = NULL)
  do.call(rbind, lapply(im, function(z) {
    v <- get1(z)
    if (what == "summary") return(cbind(n = z$n, v))
    if (is.null(dim(v))) {
      d <- data.frame(n = z$n, as.numeric(names(v)), value = unname(v))
      names(d)[2] <- dims[1]
      return(d)
    }
    d <- data.frame(n = z$n,
                    as.numeric(rownames(v))[row(v)],
                    as.numeric(colnames(v))[col(v)],
                    value = as.vector(v))
    names(d)[2:3] <- dims
    if (what == "q") d$cohort <- z$cohorts[match(d$age, z$ages)]
    d
  }))
}


#' Validate the Inputs of om_calibrate_luy()
#'
#' Checks that the data frames you prepared are in the standard long format
#' expected by \code{\link{om_calibrate_luy}} and, if \code{survey_date} is
#' given, that they cover the years, ages and cohorts the calibration needs
#' (see \code{\link{om_luy_data_needs}}). Run it after reading your data and
#' before calibrating.
#'
#' @section Standard format:
#' \tabular{lll}{
#'   \strong{argument} \tab \strong{columns} \tab \strong{content} \cr
#'   \code{asfr}      \tab \code{age, year, rate}   \tab period ASFR of women, births per woman (not per 1000) \cr
#'   \code{cohort_qx} \tab \code{age, cohort, qx}   \tab cohort probability of dying, 0--1 \cr
#'   \code{pop_age}   \tab \code{age, year, n}      \tab population size (not exposure) \cr
#'   \code{period_lx} \tab \code{year, age, lx} (or \code{qx}) \tab period life table \cr
#' }
#' \code{age}, \code{year} and \code{cohort} are whole numbers (completed age,
#' calendar year, birth year). Each key combination appears once.
#'
#' @section Checks:
#' \strong{Errors:} not a data frame; missing columns; non-numeric columns;
#' non-integer ages/years/cohorts; duplicated keys; \code{rate} > 1 (per 1000)
#' or negative; \code{qx} outside 0--1; negative \code{n} or \code{lx}.
#' \strong{Warnings:} rows with \code{NA} (they are ignored by the
#' calibration); with \code{survey_date}, years, ages or cohorts the
#' calibration needs but that are missing.
#'
#' @param asfr,cohort_qx,pop_age,period_lx Data frames to check (each
#'   optional; \code{NULL} is skipped).
#' @param survey_date Optional survey date (decimal year). If given, the
#'   coverage of the inputs is compared with \code{om_luy_data_needs()}.
#' @param sex_parent \code{"Female"} or \code{"Male"}; used with
#'   \code{survey_date}.
#' @param age_respondent,cohort_lag,male_shift As in
#'   \code{\link{om_calibrate_luy}}; used with \code{survey_date}.
#' @param verbose Print a summary (default \code{TRUE}).
#'
#' @return Invisibly, a data frame with one row per input: number of
#'   complete rows and the age and year (or cohort) ranges. Stops with an
#'   informative error if an input is malformed.
#'
#' @examples
#' # synthetic inputs in the standard long format
#' asfr <- expand.grid(age = 15:49, year = 1950:2000)
#' asfr$rate <- 0.1 * exp(-0.5 * ((asfr$age - 28) / 5.5)^2)
#' cohort_qx <- expand.grid(age = 0:110, cohort = 1900:1990)
#' cohort_qx$qx <- pmin(1, 0.00005 * exp(0.095 * cohort_qx$age))
#' cohort_qx <- cohort_qx[cohort_qx$cohort + cohort_qx$age <= 2003, ]
#' pop_age <- expand.grid(age = 0:110, year = 1950:2000)
#' pop_age$n <- 1e5 * exp(-0.01 * pop_age$age)
#' period_lx <- expand.grid(age = 0:110, year = 1980:2003)
#' period_lx$lx <- 1e5 * exp(-0.00005 / 0.095 * (exp(0.095 * period_lx$age) - 1))
#' om_luy_validate_inputs(asfr, cohort_qx, pop_age, period_lx,
#'                        survey_date = 2003.9, sex_parent = "Female",
#'                        age_respondent = c(20, 40))
#'
#' # a typical preparation error: rates per 1000 women
#' try(om_luy_validate_inputs(asfr = transform(asfr, rate = rate * 1000)))
#' @seealso \code{\link{om_calibrate_luy}}, \code{\link{om_luy_data_needs}}
#' @export
om_luy_validate_inputs <- function(asfr = NULL, cohort_qx = NULL,
                                   pop_age = NULL, period_lx = NULL,
                                   survey_date = NULL,
                                   sex_parent = c("Female", "Male"),
                                   age_respondent = seq(20, 60, by = 5),
                                   cohort_lag = 4, male_shift = 4,
                                   verbose = TRUE) {
  sex_parent <- match.arg(sex_parent)
  err <- function(...) stop(sprintf(...), call. = FALSE)

  check <- function(df, name, keys, value) {
    if (is.null(df)) return(NULL)
    if (!is.data.frame(df)) err("'%s' must be a data frame, not %s.", name,
                                class(df)[1])
    miss <- setdiff(c(keys, value), names(df))
    if (length(miss)) {
      err("'%s' lacks column(s) %s. Required: %s. Found: %s.", name,
          paste(miss, collapse = ", "), paste(c(keys, value), collapse = ", "),
          paste(names(df), collapse = ", "))
    }
    for (v in c(keys, value)) {
      if (!is.numeric(df[[v]])) {
        err("'%s$%s' must be numeric (it is %s). Convert it, e.g. with as.numeric(); for HMD ages like \"110+\" use as.integer(sub(\"+\", \"\", Age, fixed = TRUE)).",
            name, v, class(df[[v]])[1])
      }
    }
    ok <- stats::complete.cases(df[, c(keys, value)])
    if (!all(ok)) {
      warning(sprintf("'%s': %d row(s) with NA are ignored.", name, sum(!ok)),
              call. = FALSE)
    }
    d <- df[ok, c(keys, value), drop = FALSE]
    if (!nrow(d)) err("'%s' has no complete rows.", name)
    for (k in keys) {
      if (any(d[[k]] != round(d[[k]]))) {
        err("'%s$%s' must hold whole numbers (completed age / calendar year / birth year).",
            name, k)
      }
    }
    dup <- duplicated(d[, keys])
    if (any(dup)) {
      first <- d[which(dup)[1], keys]
      err("'%s' has %d duplicated %s combination(s), e.g. %s. Each combination must appear once.",
          name, sum(dup), paste(keys, collapse = "/"),
          paste(paste(keys, unlist(first), sep = " = "), collapse = ", "))
    }
    d
  }

  a <- check(asfr, "asfr", c("age", "year"), "rate")
  if (!is.null(a)) {
    if (any(a$rate < 0)) err("'asfr$rate' has negative values.")
    if (max(a$rate) > 1) {
      err("'asfr$rate' has values up to %.3g: this looks like births per 1000 women. Divide by 1000 (rate = births per woman).",
          max(a$rate))
    }
  }
  q <- check(cohort_qx, "cohort_qx", c("age", "cohort"), "qx")
  if (!is.null(q) && (any(q$qx < 0) || any(q$qx > 1))) {
    err("'cohort_qx$qx' must be probabilities between 0 and 1 (range found: %g to %g).",
        min(q$qx), max(q$qx))
  }
  p <- check(pop_age, "pop_age", c("age", "year"), "n")
  if (!is.null(p) && any(p$n < 0)) err("'pop_age$n' has negative values.")
  l <- NULL
  if (!is.null(period_lx)) {
    vcol <- if ("lx" %in% names(period_lx)) "lx" else "qx"
    l <- check(period_lx, "period_lx", c("year", "age"), vcol)
    if (vcol == "lx" && any(l$lx < 0)) err("'period_lx$lx' has negative values.")
    if (vcol == "qx" && (any(l$qx < 0) || any(l$qx > 1))) {
      err("'period_lx$qx' must be probabilities between 0 and 1.")
    }
  }

  rng <- function(v) sprintf("%d-%d", as.integer(min(v)), as.integer(max(v)))
  rows <- list(
    if (!is.null(a)) data.frame(input = "asfr", rows = nrow(a), ages = rng(a$age),
                                years = rng(a$year), cohorts = NA),
    if (!is.null(q)) data.frame(input = "cohort_qx", rows = nrow(q), ages = rng(q$age),
                                years = rng(q$cohort + q$age), cohorts = rng(q$cohort)),
    if (!is.null(p)) data.frame(input = "pop_age", rows = nrow(p), ages = rng(p$age),
                                years = rng(p$year), cohorts = NA),
    if (!is.null(l)) data.frame(input = "period_lx", rows = nrow(l), ages = rng(l$age),
                                years = rng(l$year), cohorts = NA))
  summ <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])

  # coverage against what the calibration needs
  gaps <- character(0)
  if (!is.null(survey_date)) {
    nd <- om_luy_data_needs(survey_date, sex_parent, age_respondent,
                            cohort_lag = cohort_lag, male_shift = male_shift)
    o  <- attr(nd, "overall")
    shift <- if (sex_parent == "Male") male_shift else 0
    lack <- function(have, need) setdiff(need, unique(have))
    span <- function(v) if (length(v) > 3) sprintf("%d-%d (%d values)", min(v), max(v), length(v))
                        else paste(v, collapse = ", ")
    if (!is.null(a)) {
      m <- lack(a$year[a$age >= 15 & a$age <= 49],
                seq(o$fertility_years[1], o$fertility_years[2]))
      if (length(m)) gaps <- c(gaps, sprintf("asfr: years %s missing", span(m)))
      m <- lack(a$age, 15:49)
      if (length(m)) gaps <- c(gaps, sprintf("asfr: ages %s missing", span(m)))
    }
    if (!is.null(p)) {
      m <- lack(p$year, seq(o$population_years[1], o$population_years[2]))
      if (length(m)) gaps <- c(gaps, sprintf("pop_age: years %s missing", span(m)))
      m <- lack(p$age, (15:49) + shift)
      if (length(m)) gaps <- c(gaps, sprintf("pop_age: ages %s missing", span(m)))
    }
    if (!is.null(q)) {
      need_c <- seq(o$cohorts[1], o$cohorts[2])
      m <- lack(q$cohort, need_c)
      if (length(m)) gaps <- c(gaps, sprintf("cohort_qx: cohorts %s missing", span(m)))
      # each needed cohort must start by its youngest parental age and run up
      # to the last calendar year needed (unless its series is closed, i.e.
      # it reaches very old ages)
      have <- intersect(need_c, unique(q$cohort))
      if (length(have)) {
        k      <- as.character(have)
        a_min  <- tapply(q$age, q$cohort, min)[k]
        a_max  <- tapply(q$age, q$cohort, max)[k]
        y_last <- tapply(q$cohort + q$age, q$cohort, max)[k]
        Tf     <- floor(survey_date)
        x_need <- pmax(15 + shift, Tf - max(age_respondent) - cohort_lag - have)
        late   <- have[a_min > x_need]
        short  <- have[y_last < o$cohort_last_year & a_max < 100]
        if (length(late)) {
          gaps <- c(gaps, sprintf("cohort_qx: cohorts %s start after the youngest parental age needed",
                                  span(late)))
        }
        if (length(short)) {
          gaps <- c(gaps, sprintf("cohort_qx: cohorts %s end before calendar year %d",
                                  span(short), o$cohort_last_year))
        }
      }
    }
    if (!is.null(l)) {
      m <- lack(l$year, seq(o$period_years_typical[1], o$period_years_typical[2]))
      if (length(m)) gaps <- c(gaps, sprintf("period_lx: years %s missing", span(m)))
    }
    for (g in gaps) warning(g, call. = FALSE)
  }

  if (verbose) {
    cat("Inputs for om_calibrate_luy(): format OK\n")
    print(summ, row.names = FALSE)
    if (!is.null(survey_date)) {
      cat(sprintf("\nCoverage for survey date %s (%s parents): %s\n",
                  format(survey_date), tolower(sex_parent),
                  if (length(gaps)) paste(length(gaps), "gap(s), see warnings")
                  else "complete"))
    }
  }
  attr(summ, "gaps") <- gaps
  invisible(summ)
}
