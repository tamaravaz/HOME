# ==============================================================================
# om_estimate_index.R
# Core estimation function and S3 methods for indirect adult mortality
# estimation via orphanhood-based approaches.
# ==============================================================================

#' @importFrom stats na.omit approxfun uniroot median
#' @importFrom utils tail
#' @keywords internal
NULL

# ------------------------------------------------------------------------------
# Internal constants
# ------------------------------------------------------------------------------

# Tolerance (in years) for the monotonicity check on reference dates.
# Estimates deviating by more than this threshold are set to NA.
.MONOTONICITY_TOLERANCE <- 0.01

# ------------------------------------------------------------------------------
# Main exported function
# ------------------------------------------------------------------------------

#' Estimate Adult Mortality Indices from Orphanhood Data
#'
#' Estimates conditional survivorship probabilities using orphanhood methods
#' and harmonizes them into common adult mortality indices using a
#' one-parameter relational logit model.
#'
#' @details
#' The function implements three orphanhood-based estimation methods (Brass,
#' Timaeus, and Luy). Regardless of the method selected, estimates are
#' harmonized into three comparable mortality indices:
#'
#' \itemize{
#'   \item \strong{30q30}: Probability of dying between ages 30 and 60.
#'   \item \strong{45q15}: Probability of dying between ages 15 and 60.
#'   \item \strong{e30}: Life expectancy at age 30.
#' }
#'
#' @param method Character. One of \code{"luy"}, \code{"timaeus"}, or
#'   \code{"brass"}.
#' @param sex_parent Character. \code{"Female"} or \code{"Male"}.
#' @param age_respondent Numeric vector. Lower bound of respondent age group.
#' @param p_surv Numeric vector. Proportion of parents reported alive.
#' @param mean_age_parent Numeric vector. Mean age of parents at respondent's
#'   birth (\eqn{M_n}).
#' @param surv_date Numeric. Decimal survey year (e.g., \code{2015.5}).
#' @param num_respondents Numeric vector. Optional. Number of respondents per
#'   age group. Used by the Brass method for weighted averaging.
#' @param sn_10 Numeric. Optional. Survival proportion for the 10--14 age
#'   group, required by the Brass method when data do not start at ages 10--14.
#' @param model_family Character. Model life table family. One of
#'   \code{"General"}, \code{"Latin"}, \code{"Chilean"},
#'   \code{"South_Asian"}, \code{"Far_East_Asian"} (UN systems), or
#'   \code{"West"}, \code{"North"}, \code{"East"}, \code{"South"}
#'   (Coale-Demeny systems). Default is \code{"General"}.
#'
#' @return An object of class \code{OrphanhoodEstimate}, which is a named list
#'   with components:
#'   \describe{
#'     \item{\code{estimates}}{A data frame of estimated mortality indices by
#'       respondent age group.}
#'     \item{\code{meta}}{A list of metadata: sex, model family, system type,
#'       method identifier, and primary index name.}
#'     \item{\code{inputs}}{A list of the original input arguments, enabling
#'       reproducibility and downstream sensitivity analysis.}
#'   }
#'
#' @seealso \code{\link{om_sensitivity}}, \code{\link{om_sensitivity_family}},
#'   \code{\link{om_plot_linearity}}
#'
#' @examples
#' \dontrun{
#'   result <- om_estimate_index(
#'     method          = "luy",
#'     sex_parent      = "Female",
#'     age_respondent  = seq(15, 60, by = 5),
#'     p_surv          = c(0.98, 0.97, 0.95, 0.92, 0.88,
#'                         0.82, 0.75, 0.65, 0.55, 0.40),
#'     mean_age_parent = rep(27, 10),
#'     surv_date       = 2024.75,
#'     model_family    = "General"
#'   )
#'   print(result)
#' }
#'
#' @export
om_estimate_index <- function(method      = c("luy", "timaeus", "brass"),
                              sex_parent  = c("Female", "Male"),
                              age_respondent,
                              p_surv,
                              mean_age_parent,
                              surv_date,
                              num_respondents = NULL,
                              sn_10           = NULL,
                              model_family    = "General") {

  method     <- match.arg(method)
  sex_parent <- match.arg(sex_parent)

  # Input validation
  if (!is.null(num_respondents) &&
      length(num_respondents) != length(age_respondent)) {
    stop("Length of 'num_respondents' must match 'age_respondent'.",
         call. = FALSE)
  }

  # ----------------------------------------------------------------------
  # 1. Dispatch to method-specific internal function
  # ----------------------------------------------------------------------

  if (method == "luy") {
    citation <- "Luy (2012)"
    raw      <- .om_luy(sex_parent, age_respondent, p_surv,
                        mean_age_parent, surv_date)

  } else if (method == "timaeus") {
    citation <- "Timaeus (1992)"
    raw      <- .om_timaeus(sex_parent, age_respondent, p_surv,
                            mean_age_parent, surv_date)

  } else {
    citation <- "Brass (1973)"
    if (is.null(sn_10) && min(age_respondent) > 15L) sn_10 <- NA_real_
    raw <- .om_brass(sex_parent, age_respondent, p_surv,
                     mean_age_parent, surv_date, sn_10, num_respondents)
  }

  # ----------------------------------------------------------------------
  # 2. Load and filter model life table standard
  # ----------------------------------------------------------------------

  std_data <- mlt_un_data

  fam_un <- c("General", "Latin", "Chilean", "South_Asian", "Far_East_Asian")
  fam_cd <- c("West", "North", "East", "South")

  if (model_family %in% fam_un) {
    selected_type <- "UN"
  } else if (model_family %in% fam_cd) {
    selected_type <- "CD"
  } else {
    stop(sprintf("'model_family' must be one of: %s.",
                 paste(c(fam_un, fam_cd), collapse = ", ")),
         call. = FALSE)
  }

  std <- std_data[
    std_data$Sex    == sex_parent    &
      std_data$Family == model_family  &
      std_data$Type_MLT == selected_type &
      std_data$E0    == 60, ]

  if (nrow(std) == 0L) {
    stop(sprintf(
      "Standard life table not found for Family = '%s', Sex = '%s'.",
      model_family, sex_parent),
      call. = FALSE)
  }

  # Logit helper functions (Brass relational logit model)
  logit     <- function(x) 0.5 * log((1 - x) / x)
  inv_logit <- function(y) 1 / (1 + exp(2 * y))

  std$Yx    <- logit(std$lx)
  Y_std_fun <- stats::approxfun(std$Age, std$Yx, rule = 2)

  # ----------------------------------------------------------------------
  # 3. Estimate alpha and mortality indices row-by-row
  # ----------------------------------------------------------------------

  n_rows          <- nrow(raw)
  raw$Alpha       <- NA_real_
  raw$est_e30     <- NA_real_
  raw$est_45q15   <- NA_real_
  raw$est_30q30   <- NA_real_

  # Display age convention differs by method
  raw$DisplayAge <- if (method == "timaeus") {
    raw$age_input + 5L
  } else {
    raw$age_input
  }

  for (i in seq_len(n_rows)) {

    if (is.na(raw$prob[i])) next

    val_obs <- raw$prob[i]
    age_b   <- raw$base_age[i]
    age_t   <- raw$target_age[i]

    Ys_b <- Y_std_fun(age_b)
    Ys_t <- Y_std_fun(age_t)

    # Objective function: solve for alpha such that modelled CondProb = observed
    obj_fun <- function(a) {
      lx_t <- inv_logit(a + Ys_t)
      lx_b <- inv_logit(a + Ys_b)
      (lx_t / lx_b) - val_obs
    }

    tryCatch({
      root      <- stats::uniroot(obj_fun, interval = c(-5, 5))
      alpha_val <- root$root

      raw$Alpha[i]   <- alpha_val
      raw$lx_base[i] <- inv_logit(alpha_val + Ys_b)
      raw$lx_n[i]    <- inv_logit(alpha_val + Ys_t)

      # Derive full lx schedule from alpha and standard
      max_age_std <- max(std$Age)
      age_seq     <- seq(15L, max_age_std, by = 1L)
      lx_seq      <- inv_logit(alpha_val + Y_std_fun(age_seq))

      l15 <- lx_seq[age_seq == 15L]
      l30 <- lx_seq[age_seq == 30L]
      l60 <- lx_seq[age_seq == 60L]

      # 45q15: probability of dying between ages 15 and 60
      raw$est_45q15[i] <- 1 - (l60 / l15)

      # 30q30: probability of dying between ages 30 and 60
      raw$est_30q30[i] <- 1 - (l60 / l30)

      # e30: life expectancy at age 30 via trapezoidal integration
      idx_30  <- which(age_seq == 30L)
      lx_tail <- lx_seq[idx_30:length(lx_seq)]
      Lx_tail <- 0.5 * (lx_tail[-length(lx_tail)] + lx_tail[-1L])
      T30_val <- sum(Lx_tail, na.rm = TRUE) + (utils::tail(lx_tail, 1L) * 0.5)
      e30_est <- T30_val / lx_tail[1L]

      # Luy sex correction applied to e30 only (per original paper)
      raw$est_e30[i] <- if (method == "luy") {
        sex_correction <- c(Female = 0.9709, Male = 0.9837)
        e30_est * sex_correction[[sex_parent]]
      } else {
        e30_est
      }

    }, error = function(e) {
      # uniroot failed for this row — estimates remain NA
      invisible(NULL)
    })
  }

  # ----------------------------------------------------------------------
  # 4. Monotonicity check on reference dates
  # ----------------------------------------------------------------------
  # Reference dates must decrease monotonically as respondent age increases.
  # Estimates deviating by more than .MONOTONICITY_TOLERANCE years are set
  # to NA to preserve data quality without discarding minor rounding noise.

  last_valid_date  <- Inf
  violation_count  <- 0L

  for (k in seq_len(nrow(raw))) {
    curr <- raw$ref_date[k]
    if (is.na(curr)) next

    if (curr < last_valid_date) {
      last_valid_date <- curr
    } else {
      diff <- curr - last_valid_date
      if (diff > .MONOTONICITY_TOLERANCE) {
        raw$ref_date[k] <- NA_real_
        violation_count  <- violation_count + 1L
      }
      # Within tolerance: accept as rounding noise; do not update
      # last_valid_date to prevent upward drift.
    }
  }

  if (violation_count > 0L) {
    warning(sprintf(
      "Monotonicity check: %d estimate(s) removed due to reference date inconsistency > %.2f years.",
      violation_count, .MONOTONICITY_TOLERANCE),
      call. = FALSE)
  }

  # ----------------------------------------------------------------------
  # 5. Assemble output data frame
  # ----------------------------------------------------------------------
  # check.names = FALSE preserves column names that begin with digits
  # (e.g., "30q30", "45q15"), which R would otherwise mangle to "X30q30".

  output_df <- data.frame(
    Method        = citation,
    RespondentAge = raw$DisplayAge,
    BaseAge       = raw$base_age,
    NumeratorAge  = raw$target_age,
    lx_base       = raw$lx_base,
    lx_n          = raw$lx_n,
    CondProb      = raw$prob,
    RefTime       = raw$ref_date,
    Alpha         = raw$Alpha,
    "30q30"       = raw$est_30q30,
    "45q15"       = raw$est_45q15,
    e30           = raw$est_e30,
    check.names   = FALSE
  )

  # ----------------------------------------------------------------------
  # 6. Construct and return S3 object
  # ----------------------------------------------------------------------

  out_obj <- list(
    estimates = output_df,
    meta = list(
      sex        = sex_parent,
      family     = model_family,
      type       = selected_type,
      method_id  = method,
      index_name = "30q30"
    ),
    inputs = list(
      method          = method,
      sex_parent      = sex_parent,
      age_respondent  = age_respondent,
      p_surv          = p_surv,
      mean_age_parent = mean_age_parent,
      surv_date       = surv_date,
      num_respondents = num_respondents,
      sn_10           = sn_10,
      model_family    = model_family
    )
  )

  class(out_obj) <- "OrphanhoodEstimate"
  out_obj
}

# ------------------------------------------------------------------------------
# S3 methods: OrphanhoodEstimate
# ------------------------------------------------------------------------------

#' Print method for \code{OrphanhoodEstimate} objects
#'
#' @param x An object of class \code{OrphanhoodEstimate}.
#' @param ... Further arguments passed to \code{print.data.frame}.
#' @return Invisibly returns \code{x}.
#' @export
print.OrphanhoodEstimate <- function(x, ...) {
  cat("\n=== Orphanhood Mortality Estimates ===\n")
  cat(sprintf("Method:    %s\n", x$estimates$Method[1L]))
  cat(sprintf("Target:    %s parent (%s \u2014 %s)\n",
              x$meta$sex, x$meta$type, x$meta$family))

  age_lbl <- if (x$meta$method_id == "timaeus") {
    "Upper bound (n)"
  } else {
    "Lower bound (n)"
  }
  cat(sprintf("Age ref:   %s\n\n", age_lbl))

  print_df <- x$estimates
  print_df$Method <- NULL

  # Round for display only — source data frame is not modified
  round_cols <- c("lx_base", "lx_n", "CondProb")
  for (col in round_cols) {
    if (col %in% names(print_df))
      print_df[[col]] <- round(print_df[[col]], 4L)
  }
  print_df$RefTime <- round(print_df$RefTime, 2L)
  print_df$Alpha   <- round(print_df$Alpha,   3L)

  if ("30q30" %in% names(print_df))
    print_df[["30q30"]] <- round(print_df[["30q30"]], 4L)
  if ("45q15" %in% names(print_df))
    print_df[["45q15"]] <- round(print_df[["45q15"]], 4L)
  if ("e30"   %in% names(print_df))
    print_df[["e30"]]   <- round(print_df[["e30"]],   2L)

  print(print_df, row.names = FALSE, ...)
  invisible(x)
}

#' Summary method for \code{OrphanhoodEstimate} objects
#'
#' @param object An object of class \code{OrphanhoodEstimate}.
#' @param ... Further arguments (currently unused).
#' @return Invisibly returns \code{object}.
#' @export
summary.OrphanhoodEstimate <- function(object, ...) {
  # Default to 30q30; fall back to e30 if missing
  if ("30q30" %in% names(object$estimates)) {
    vals      <- stats::na.omit(object$estimates[["30q30"]])
    idx_label <- "30q30"
  } else if ("e30" %in% names(object$estimates)) {
    vals      <- stats::na.omit(object$estimates$e30)
    idx_label <- "e30"
  } else {
    idx_label <- object$meta$index_name
    vals      <- stats::na.omit(object$estimates[[idx_label]])
  }

  cat(sprintf(
    "Summary for %s parent mortality (index: %s):\n",
    object$meta$sex, idx_label))

  if (length(vals) > 0L) {
    cat(sprintf("  Range:   %.4f \u2013 %.4f\n", min(vals), max(vals)))
    cat(sprintf("  Mean:    %.4f\n", mean(vals)))
    cat(sprintf("  Median:  %.4f\n", stats::median(vals)))
  } else {
    cat("  No valid estimates found.\n")
  }

  invisible(object)
}

# ------------------------------------------------------------------------------
# Internal method-specific functions
# ------------------------------------------------------------------------------

#' Luy (2012) orphanhood estimation
#'
#' @param sex Character. \code{"Female"} or \code{"Male"}.
#' @param age Numeric vector. Respondent age group lower bounds.
#' @param sn Numeric vector. Parental survival proportions.
#' @param mn Numeric vector. Mean age of parents at respondent's birth.
#' @param date Numeric. Decimal survey year.
#' @return A data frame with columns \code{age_input}, \code{base_age},
#'   \code{target_age}, \code{prob}, \code{ref_date}.
#' @keywords internal
.om_luy <- function(sex, age, sn, mn, date) {
  coef_set <- coef_luy[[sex]]
  out_list <- vector("list", length(age))

  for (i in seq_along(age)) {
    curr_n  <- age[i]
    curr_sn <- sn[i]
    curr_mn <- mn[i]
    base    <- 30L
    target  <- 33L + curr_n

    get_coef <- function(df) {
      row <- df[df$Age_Group == curr_n, ]
      if (nrow(row) == 0L) return(NA_real_)
      col_names <- names(row)[-1L]
      x_vals    <- as.numeric(gsub("[^0-9.]", "", col_names))
      y_vals    <- as.numeric(unlist(row[1L, -1L]))
      if (any(is.na(x_vals))) return(NA_real_)
      stats::approx(x_vals, y_vals, xout = curr_mn, rule = 2)$y
    }

    Wn <- get_coef(coef_set$wn)
    An <- get_coef(coef_set$an)
    Bn <- get_coef(coef_set$bn)

    if (is.na(Wn)) {
      est <- NA_real_
      lag <- NA_real_
    } else {
      est <- curr_sn * Wn
      lag <- if (curr_sn > 0 && !is.na(An)) {
        An * log(curr_sn) + Bn
      } else {
        NA_real_
      }
    }

    out_list[[i]] <- data.frame(
      age_input  = curr_n,
      base_age   = base,
      target_age = target,
      prob       = est,
      ref_date   = date - lag
    )
  }

  do.call(rbind, out_list)
}

#' Timaeus (1992) orphanhood estimation
#'
#' @inheritParams .om_luy
#' @return A data frame with columns \code{age_input}, \code{base_age},
#'   \code{target_age}, \code{prob}, \code{ref_date}.
#' @keywords internal
.om_timaeus <- function(sex, age, sn, mn, date) {
  coefs <- coef_timaeus[[sex]]
  has_d <- "d_n" %in% names(coefs)

  sn_lead <- if (has_d) c(sn[-1L], NA_real_) else rep(NA_real_, length(sn))
  out_list <- vector("list", length(age))

  if (is.na(date))
    warning("Survey date is NA — all reference dates will be NA.", call. = FALSE)

  # Ensure mn is a vector of the same length as age
  if (length(mn) == 1L) {
    mn <- rep(mn, length(age))
  } else if (length(mn) != length(age)) {
    warning(
      "Length of 'mn' does not match 'age_respondent'; values will be recycled.",
      call. = FALSE)
    mn <- rep_len(mn, length(age))
  }

  for (i in seq_along(age)) {
    curr_age <- age[i]
    curr_sn  <- sn[i]
    curr_mn  <- mn[i]
    n_dur    <- curr_age + 5L
    base     <- if (sex == "Female") 25L else 35L

    # Conditional survivorship estimate
    cf  <- coefs[coefs$Age == n_dur, ]
    est <- if (nrow(cf) == 0L || is.na(curr_mn) || is.na(curr_sn)) {
      NA_real_
    } else {
      val <- cf$a_n + cf$b_n * curr_mn + cf$c_n * curr_sn
      if (has_d && !is.na(sn_lead[i])) val + cf$d_n * sn_lead[i] else val
    }

    # Reference date (IUSSP formula)
    N     <- curr_age + 2.5
    T_lag <- NA_real_

    if (!is.na(curr_mn) && !is.na(N)) {
      if (sex == "Female") {
        safe_sn <- if (!is.na(curr_sn) && curr_sn > 0) curr_sn else 0.99
        num     <- 80 - curr_mn - N
        den     <- 80 - curr_mn
        term_m  <- if (num > 0 && den > 0) log(num / den) else 0
        T_lag   <- (N / 2) * (1 - (1/3) * log(safe_sn) + (1/3) * term_m)
      } else {
        prev_sn    <- if (i > 1L) sn[i - 1L] else curr_sn
        safe_curr  <- if (!is.na(curr_sn)  && curr_sn  > 0) curr_sn  else 0.99
        safe_prev  <- if (!is.na(prev_sn)  && prev_sn  > 0) prev_sn  else 0.99
        geom_mean  <- sqrt(safe_curr * safe_prev)
        num        <- 80 - curr_mn - N
        den        <- 80 - (curr_mn - 0.75)
        term_m     <- if (num > 0 && den > 0) log(num / den) else 0
        T_lag      <- ((N + 0.75) / 2) *
          (1 - (1/3) * log(geom_mean) + (1/3) * term_m)
      }
    }

    out_list[[i]] <- data.frame(
      age_input  = curr_age,
      base_age   = base,
      target_age = base + n_dur,
      prob       = est,
      ref_date   = date - T_lag
    )
  }

  do.call(rbind, out_list)
}

#' Brass (1973) / Hill orphanhood estimation
#'
#' @inheritParams .om_luy
#' @param sn_10 Numeric. Survival proportion for the 10--14 age group.
#' @param n_resp Numeric vector. Optional. Number of respondents per age group.
#' @return A data frame with columns \code{age_input}, \code{base_age},
#'   \code{target_age}, \code{prob}, \code{ref_date}.
#' @keywords internal
.om_brass <- function(sex, age, sn, mn, date, sn_10, n_resp) {
  all_coefs <- coef_brass_hill
  z_tab     <- coef_z_brass
  out_list  <- vector("list", length(age))

  if (length(mn) == 1L) mn <- rep(mn, length(age))
  if (is.null(sn_10) || length(sn_10) == 0L) sn_10 <- NA_real_

  for (i in seq_along(age)) {
    curr_age <- age[i]
    curr_sn  <- sn[i]
    curr_mn  <- mn[i]
    n_mid    <- curr_age + 2.5

    if (sex == "Female") {
      base               <- 25L
      target_n           <- 25L + curr_age
      coefs              <- all_coefs$Female
      z_arg              <- curr_mn + n_mid
      correction_factor  <- 0
    } else {
      if (curr_mn < 35.5) {
        base    <- 32.5
        coefs   <- all_coefs$Male_A
      } else {
        base    <- 37.5
        coefs   <- all_coefs$Male_B
      }
      target_n          <- base + curr_age + 2.5
      z_arg             <- curr_mn + n_mid + 0.75
      correction_factor <- 0.75
    }

    prev_sn <- if (i == 1L) sn_10 else sn[i - 1L]
    row     <- coefs[coefs$Age_Group == curr_age, ]

    if (nrow(row) == 0L || is.na(prev_sn)) {
      est <- NA_real_
      ref <- NA_real_
    } else {
      mn_cols <- as.numeric(names(row)[-1L])
      vals    <- as.numeric(row[1L, -1L])
      Wn      <- stats::approx(mn_cols, vals, xout = curr_mn, rule = 2)$y
      est     <- Wn * prev_sn + (1 - Wn) * curr_sn

      # Weighted average survival for time location
      S_10 <- if (!is.null(n_resp)) {
        N_curr <- n_resp[i]
        if (i == 1L) {
          (prev_sn + curr_sn) / 2
        } else {
          N_prev <- n_resp[i - 1L]
          ((N_prev * prev_sn) + (N_curr * curr_sn)) / (N_prev + N_curr)
        }
      } else {
        (prev_sn + curr_sn) / 2
      }

      z_val   <- stats::approx(z_tab$Age, z_tab$Z, xout = z_arg, rule = 2)$y
      log_val <- if (!is.na(S_10) && S_10 > 0) log(S_10) else log(0.95)
      term_m  <- 27 - curr_mn + correction_factor
      u_n     <- 0.3333 * log_val + z_val + 0.0037 * term_m
      lag     <- ((n_mid + correction_factor) * (1 - u_n)) / 2
      ref     <- date - lag
    }

    out_list[[i]] <- data.frame(
      age_input  = curr_age,
      base_age   = base,
      target_age = target_n,
      prob       = est,
      ref_date   = ref
    )
  }

  do.call(rbind, out_list)
}
