#' @importFrom stats na.omit approxfun uniroot
#' @importFrom utils tail
#' @importFrom stats median
NULL

#' Estimate Adult Mortality Indices from Orphanhood Data
#'
#' Estimates conditional survivorship probabilities using orphanhood methods
#' and harmonizes them into common adult mortality indices using a one-parameter
#' relational logit model.
#'
#' @details
#' The function implements three orphanhood-based estimation methods (Brass,
#' Timæus, and Luy). Regardless of the method selected, estimates are harmonized
#' into three comparable mortality indices:
#'
#' \itemize{
#'   \item \strong{30q30}: Probability of dying between ages 30 and 60 (default index).
#'   \item \strong{45q15}: Probability of dying between ages 15 and 60.
#'   \item \strong{e30}: Life expectancy at age 30.
#' }
#'
#' @param method Character. One of \code{"luy"}, \code{"timaeus"}, or \code{"brass"}.
#' @param sex_parent Character. \code{"Female"} or \code{"Male"}.
#' @param age_respondent Numeric. Lower bound of respondent age group.
#' @param p_surv Numeric. Proportion of parents alive.
#' @param mean_age_parent Numeric. Mean age of parents at birth (\eqn{M_n}).
#' @param surv_date Numeric. Decimal survey year.
#' @param num_respondents Numeric. Optional. Number of respondents per age group.
#' @param sn_10 Numeric. Required for Brass if data do not start at ages 10--14.
#' @param model_family Character. Model life table family. Default is \code{"General"}.
#' @param std_level Numeric. Life expectancy at birth (\eqn{e_0}) of the standard
#'        model life table. Default is 80.
#'
#' @return An object of class \code{OrphanhoodEstimate}.
#' @export
om_estimate_index <- function(method = c("luy", "timaeus", "brass"),
                              sex_parent = c("Female", "Male"),
                              age_respondent,
                              p_surv,
                              mean_age_parent,
                              surv_date,
                              num_respondents = NULL,
                              sn_10 = NULL,
                              model_family = "General",
                              std_level = 80) {

  method <- match.arg(method)
  sex_parent <- match.arg(sex_parent)

  # Validate Inputs
  if (!is.null(num_respondents) && length(num_respondents) != length(age_respondent)) {
    stop("Length of 'num_respondents' must match 'age_respondent'.")
  }

  if (!is.numeric(std_level) || length(std_level) != 1) {
    stop("'std_level' must be a single numeric value.")
  }

  # 1. Dispatch Method
  if (method == "luy") {
    citation <- "Luy (2012)"
    raw <- .om_luy(sex_parent, age_respondent, p_surv, mean_age_parent, surv_date)
  } else if (method == "timaeus") {
    citation <- "Timaeus (1991)"
    raw <- .om_timaeus(sex_parent, age_respondent, p_surv, mean_age_parent, surv_date)
  } else if (method == "brass") {
    citation <- "Brass (1973)"
    # Fallback logic for sn_10 in wrapper
    if (is.null(sn_10)) {
      if (min(age_respondent) > 15) sn_10 <- NA
    }
    raw <- .om_brass(sex_parent, age_respondent, p_surv, mean_age_parent, surv_date, sn_10, num_respondents)
  }

  # 2. Load Standard Table
  std_data <- mlt_un_data

  # Map Family
  fam_un <- c("General", "Latin", "Chilean", "South_Asian", "Far_East_Asian")
  fam_cd <- c("West", "North", "East", "South")

  if (model_family %in% fam_un) { selected_type <- "UN" }
  else if (model_family %in% fam_cd) { selected_type <- "CD" }
  else { stop("Invalid 'model_family'.") }

  # Filter Standard (UPDATED to use std_level)
  std <- std_data[std_data$Sex == sex_parent &
                    std_data$Family == model_family &
                    std_data$Type_MLT == selected_type &
                    std_data$E0 == std_level, ]

  if (nrow(std) == 0) {
    stop(sprintf("Standard table not found for Family='%s', Sex='%s', E0=%s.",
                 model_family, sex_parent, std_level))
  }

  # Logit Helpers
  logit <- function(x) 0.5 * log((1-x)/x)
  inv_logit <- function(y) 1 / (1 + exp(2*y))

  std$Yx <- logit(std$lx)
  Y_std_fun <- stats::approxfun(std$Age, std$Yx, rule = 2)

  # 3. Process Row-by-Row
  n_rows <- nrow(raw)
  raw$Alpha       <- NA_real_
  raw$est_e30     <- NA_real_
  raw$est_45q15   <- NA_real_
  raw$est_30q30   <- NA_real_ # New default index

  # Define Display Age and Primary Index Name
  primary_index_name <- "30q30"

  if (method == "luy") {
    raw$DisplayAge <- raw$age_input
  } else if (method == "brass") {
    raw$DisplayAge <- raw$age_input
  } else {
    raw$DisplayAge <- raw$age_input + 5
  }

  for(i in 1:n_rows) {
    if(is.na(raw$prob[i])) next

    val_obs <- raw$prob[i]
    age_b   <- raw$base_age[i]
    age_t   <- raw$target_age[i]

    Ys_b <- Y_std_fun(age_b)
    Ys_t <- Y_std_fun(age_t)

    # Objective function to find Alpha
    obj_fun <- function(a) {
      lx_t <- inv_logit(a + Ys_t)
      lx_b <- inv_logit(a + Ys_b)
      (lx_t / lx_b) - val_obs
    }

    try({
      root <- stats::uniroot(obj_fun, interval = c(-5, 5))
      alpha_val <- root$root
      raw$Alpha[i] <- alpha_val

      raw$lx_base[i] <- inv_logit(alpha_val + Ys_b)
      raw$lx_n[i]    <- inv_logit(alpha_val + Ys_t)

      # --- UNIFIED CALCULATION ---
      max_age_std <- max(std$Age)
      age_seq <- seq(15, max_age_std, by = 1)
      Ys_seq  <- Y_std_fun(age_seq)
      lx_seq  <- inv_logit(alpha_val + Ys_seq)

      # Extract standard key ages
      l15_val <- lx_seq[which(age_seq == 15)]
      l30_val <- lx_seq[which(age_seq == 30)]
      l60_val <- lx_seq[which(age_seq == 60)]

      # 45q15 (1 - l60/l15)
      q45_15_est <- 1 - (l60_val / l15_val)

      # 30q30 (1 - l60/l30)
      q30_30_est <- 1 - (l60_val / l30_val)

      # e30 (Trapezoidal integration starting at age 30)
      idx_30  <- which(age_seq == 30)
      lx_tail <- lx_seq[idx_30:length(lx_seq)]
      Lx_tail <- 0.5 * (lx_tail[-length(lx_tail)] + lx_tail[-1])
      last_lx <- tail(lx_tail, 1)
      T30_val <- sum(Lx_tail, na.rm = TRUE) + (last_lx * 0.5)

      # Use l30 from tail to ensure consistency
      e30_est <- T30_val / lx_tail[1]

      # Luy Sex Correction (Only applied to e30 as per paper)
      if (method == "luy") {
        sex_correction <- c(Female = 0.9709, Male = 0.9837)
        e30_final <- e30_est * sex_correction[sex_parent]
      } else {
        e30_final <- e30_est
      }

      raw$est_45q15[i] <- q45_15_est
      raw$est_30q30[i] <- q30_30_est
      raw$est_e30[i]    <- e30_final

    }, silent = TRUE)
  }

  # 4. Monotonicity Check (Selective Filtering with Tolerance)
  # Logic: Reference dates must decrease as respondent age increases.
  # Tolerance: Minor fluctuations (<= 0.5 years) are accepted as noise.
  # Deviations > 0.5 years are marked as NA to preserve data quality.

  last_valid_date <- Inf
  violation_count <- 0
  margin_error <- 0.5

  for (k in seq_len(nrow(raw))) {
    curr <- raw$ref_date[k]

    if (is.na(curr)) next

    # Check if current date is strictly older (smaller) than the last valid one
    if (curr < last_valid_date) {
      last_valid_date <- curr
    } else {
      # Deviation detected: Date went forward or stagnated
      diff <- curr - last_valid_date

      if (diff <= margin_error) {
        # Within tolerance. Accept as noise but do not update last_valid_date
        # to prevent upward drift.
      } else {
        # Exceeds tolerance. Mark estimate as invalid.
        raw$ref_date[k] <- NA_real_
        violation_count <- violation_count + 1
      }
    }
  }

  if (violation_count > 0) {
    warning(sprintf(
      "Monotonicity check: %d estimates removed due to date inconsistency > %.1f years.",
      violation_count, margin_error
    ))
  }

  # 5. Build Final Data Frame
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
    `30q30`       = raw$est_30q30,
    `45q15`       = raw$est_45q15,
    e30           = raw$est_e30,
    check.names = FALSE # Preserves numeric column names
  )

  # Construct return object
  out_obj <- list(
    estimates = output_df,
    meta = list(
      sex = sex_parent,
      family = model_family,
      type = selected_type,
      method_id = method,
      index_name = primary_index_name,
      std_level = std_level # Saved in Meta
    ),
    inputs = list(
      method = method,
      sex_parent = sex_parent,
      age_respondent = age_respondent,
      p_surv = p_surv,
      mean_age_parent = mean_age_parent,
      surv_date = surv_date,
      num_respondents = num_respondents,
      sn_10 = sn_10,
      model_family = model_family,
      std_level = std_level # Saved in Inputs
    )
  )

  class(out_obj) <- "OrphanhoodEstimate"
  return(out_obj)
}

# --- S3 Methods ---

#' @export
print.OrphanhoodEstimate <- function(x, ...) {
  cat("\n=== Orphanhood Mortality Estimates ===\n")
  cat(sprintf("Method:    %s\n", x$estimates$Method[1]))
  cat(sprintf("Target:    %s Parent (%s - %s)\n", x$meta$sex, x$meta$type, x$meta$family))
  cat(sprintf("Standard:  e0 = %s\n", x$meta$std_level)) # Show in print

  age_lbl <- if(x$meta$method_id == "timaeus") "Upper Bound (n)" else "Lower Bound (n)"
  cat(sprintf("Age Ref:   %s\n", age_lbl))
  cat("\n")

  print_df <- x$estimates
  print_df$Method <- NULL

  # Rounding for display
  cols_to_round <- c("lx_base", "lx_n", "CondProb")
  for(col in cols_to_round) {
    if(col %in% names(print_df)) print_df[[col]] <- round(print_df[[col]], 4)
  }

  print_df$RefTime <- round(print_df$RefTime, 2)
  print_df$Alpha   <- round(print_df$Alpha, 3)

  # Round Indices
  if("30q30" %in% names(print_df)) print_df$`30q30` <- round(print_df$`30q30`, 4)
  if("45q15" %in% names(print_df)) print_df$`45q15` <- round(print_df$`45q15`, 4)
  if("e30" %in% names(print_df)) print_df$e30 <- round(print_df$e30, 2)

  print(print_df, row.names = FALSE)
  invisible(x)
}

#' @export
summary.OrphanhoodEstimate <- function(object, ...) {
  # Logic: Default to "30q30", fall back if missing
  target_index <- "30q30"

  if(target_index %in% names(object$estimates)) {
    vals <- na.omit(object$estimates[[target_index]])
    idx_label <- target_index
  } else if("e30" %in% names(object$estimates)) {
    vals <- na.omit(object$estimates$e30)
    idx_label <- "e30"
  } else {
    idx_label <- object$meta$index_name
    vals <- na.omit(object$estimates[[idx_label]])
  }

  cat(sprintf("Summary for %s Parent Mortality (Index: %s):\n", object$meta$sex, idx_label))
  if(length(vals) > 0) {
    cat(sprintf("  Range:          %.4f - %.4f\n", min(vals), max(vals)))
    cat(sprintf("  Mean:           %.4f\n", mean(vals)))
    cat(sprintf("  Median:         %.4f\n", median(vals)))
  } else {
    cat("  No valid estimates found.\n")
  }
}

# --- Internal Helper Implementations ---

.om_luy <- function(sex, age, sn, mn, date) {
  coef_set <- coef_luy[[sex]]
  out_list <- vector("list", length(age))
  for(i in seq_along(age)) {
    curr_n <- age[i]; curr_sn <- sn[i]; curr_mn <- mn[i]
    base <- 30; target <- 33 + curr_n
    get_coef <- function(df) {
      row <- df[df$Age_Group == curr_n, ]
      if(nrow(row) == 0) return(NA)
      col_names <- names(row)[-1]
      x_vals <- as.numeric(gsub("[^0-9.]", "", col_names))
      y_vals <- as.numeric(unlist(row[1, -1]))
      if(any(is.na(x_vals))) return(NA)
      stats::approx(x_vals, y_vals, xout=curr_mn, rule=2)$y
    }
    Wn <- get_coef(coef_set$wn)
    An <- get_coef(coef_set$an)
    Bn <- get_coef(coef_set$bn)
    if (is.na(Wn)) { est <- NA; lag <- NA }
    else {
      est <- curr_sn * Wn
      lag <- if(curr_sn > 0 && !is.na(An)) An * log(curr_sn) + Bn else NA
    }
    out_list[[i]] <- data.frame(age_input=curr_n, base_age=base, target_age=target, prob=est, ref_date=date-lag)
  }
  do.call(rbind, out_list)
}

.om_timaeus <- function(sex, age, sn, mn, date) {
  coefs <- coef_timaeus[[sex]]
  has_d <- "d_n" %in% names(coefs)
  sn_lead <- if(has_d) c(sn[-1], NA) else rep(NA, length(sn))

  out_list <- vector("list", length(age))

  # --- ROBUSTNESS CHECKS ---
  # 1. Check Survey Date
  if (is.na(date)) warning("Survey date is NA. All reference dates will be NA.")

  # 2. Ensure mn is a vector of correct length (Handling recycling manually to be safe)
  if(length(mn) == 1) {
    mn <- rep(mn, length(age))
  } else if(length(mn) != length(age)) {
    # If lengths differ (and not 1), warn or recycle carefully
    warning("Length of 'mn' does not match 'age'. Check inputs.")
    mn <- rep(mn, length.out = length(age))
  }

  for(i in seq_along(age)) {
    curr_age <- age[i]
    curr_sn  <- sn[i]
    curr_mn  <- mn[i] # Now guaranteed to exist, but might be NA

    # --- ESTIMATION (Probability) ---
    n_dur <- curr_age + 5
    base <- if(sex == "Female") 25 else 35

    cf <- coefs[coefs$Age == n_dur, ]
    if(nrow(cf) == 0) {
      est <- NA
    } else {
      # Handle NA in mn or sn for estimation
      if(is.na(curr_mn) || is.na(curr_sn)) {
        est <- NA
      } else {
        est <- cf$a_n + cf$b_n * curr_mn + cf$c_n * curr_sn
        if(has_d && !is.na(sn_lead[i])) est <- est + cf$d_n * sn_lead[i]
      }
    }

    # --- TIME REFERENCE CALCULATION (IUSPP) ---
    N <- curr_age + 2.5

    # Initialize T_lag as NA to prevent errors if inputs are missing
    T_lag <- NA

    # Only proceed if we have valid inputs for Time calculation
    if (!is.na(curr_mn) && !is.na(N)) {

      if (sex == "Female") {
        # Formula for Women
        safe_sn <- if(!is.na(curr_sn) && curr_sn > 0) curr_sn else 0.99

        num <- 80 - curr_mn - N
        den <- 80 - curr_mn

        # Protect against log(<=0)
        if(num > 0 && den > 0) {
          term_m <- log(num / den)
        } else {
          term_m <- 0
        }

        T_lag <- (N / 2) * (1 - (1/3) * log(safe_sn) + (1/3) * term_m)

      } else {
        # Formula for Men
        prev_sn <- if(i > 1) sn[i-1] else curr_sn
        safe_curr <- if(!is.na(curr_sn) && curr_sn > 0) curr_sn else 0.99
        safe_prev <- if(!is.na(prev_sn) && prev_sn > 0) prev_sn else 0.99

        geom_mean_sn <- sqrt(safe_curr * safe_prev)

        num <- 80 - curr_mn - N
        den <- 80 - (curr_mn - 0.75)

        if(num > 0 && den > 0) {
          term_m <- log(num / den)
        } else {
          term_m <- 0
        }

        T_lag <- ((N + 0.75) / 2) * (1 - (1/3) * log(geom_mean_sn) + (1/3) * term_m)
      }
    }

    # Final Calculation (will be NA if date or T_lag is NA)
    ref_date <- date - T_lag

    out_list[[i]] <- data.frame(
      age_input = curr_age,
      base_age = base,
      target_age = base + n_dur,
      prob = est,
      ref_date = ref_date
    )
  }
  do.call(rbind, out_list)
}

.om_brass <- function(sex, age, sn, mn, date, sn_10, n_resp) {
  all_coefs <- coef_brass_hill
  z_tab <- coef_z_brass

  out_list <- vector("list", length(age))

  # Ensure mn is a vector
  if(length(mn) == 1) mn <- rep(mn, length(age))

  if (is.null(sn_10) || length(sn_10) == 0) sn_10 <- NA

  for(i in seq_along(age)) {
    curr_age <- age[i]; curr_sn <- sn[i]; curr_mn <- mn[i]

    # 1. Define Mid-point (n) based on Manual X
    # age is lower bound (e.g., 20). Mid-point is 22.5 for 5-year groups.
    n_mid <- curr_age + 2.5

    if (sex == "Female") {
      base <- 25; target_n <- 25 + curr_age
      coefs <- all_coefs$Female

      # Time Lag Parameters for Female (Mothers) - Eq B.7 & B.8
      # Z argument is M + n (mid-point)
      z_arg <- curr_mn + n_mid
      correction_factor <- 0 # No extra correction for females

    } else {
      # Time Lag Parameters for Male (Fathers) - Eq B.9 & B.10
      if (curr_mn < 35.5) {
        base <- 32.5; target_n <- 32.5 + curr_age + 2.5
        coefs <- all_coefs$Male_A
      } else {
        base <- 37.5; target_n <- 37.5 + curr_age + 2.5
        coefs <- all_coefs$Male_B
      }

      # Z argument is M + n + 0.75 (Eq B.10)
      z_arg <- curr_mn + n_mid + 0.75
      # Correction for conception timing
      correction_factor <- 0.75
    }

    # --- WEIGHTING ESTIMATION (Standard Brass) ---
    if (i == 1) prev_sn <- sn_10 else prev_sn <- sn[i-1]
    row <- coefs[coefs$Age_Group == curr_age, ]

    if(nrow(row) == 0 || is.na(prev_sn)) {
      est <- NA; ref <- NA
    } else {
      mn_cols <- as.numeric(names(row)[-1])
      vals    <- as.numeric(row[1, -1])
      Wn      <- stats::approx(mn_cols, vals, xout=curr_mn, rule=2)$y
      est     <- Wn * prev_sn + (1 - Wn) * curr_sn

      # --- TIME LOCATION CALCULATION (Manual X) ---

      # 1. Calculate S_10 (10Sn-5 in manual notation)
      if (!is.null(n_resp)) {
        N_curr <- n_resp[i]
        if (i == 1) {
          # If no previous N, just average proportions
          S_10 <- (prev_sn + curr_sn) / 2
        } else {
          N_prev <- n_resp[i-1]
          S_10 <- ((N_prev * prev_sn) + (N_curr * curr_sn)) / (N_prev + N_curr)
        }
      } else {
        S_10 <- (prev_sn + curr_sn) / 2
      }

      # 2. Lookup Z(M+n)
      z_val <- stats::approx(z_tab$Age, z_tab$Z, xout = z_arg, rule=2)$y

      # 3. Calculate u(n) - Eq B.8 / B.10
      log_val <- if(!is.na(S_10) && S_10 > 0) log(S_10) else log(0.95)

      # Note: 0.0037 term also needs the correction for males (27 - M + 0.75)
      term_m <- 27 - curr_mn + correction_factor

      u_n <- 0.3333 * log_val + z_val + 0.0037 * term_m

      # 4. Calculate Lag t(n) - Eq B.7 / B.9
      # t(n) = (n + correction)(1 - u)/2
      # For females correction is 0, so it matches n(1-u)/2
      # For males correction is 0.75, matching (n+0.75)(1-u)/2
      lag <- ((n_mid + correction_factor) * (1 - u_n)) / 2

      ref <- date - lag
    }

    out_list[[i]] <- data.frame(
      age_input = curr_age,
      base_age = base,
      target_age = target_n,
      prob = est,
      ref_date = ref
    )
  }
  do.call(rbind, out_list)
}
