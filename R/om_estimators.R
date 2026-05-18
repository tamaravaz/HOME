#' Estimate Adult Mortality Indices from Orphanhood Data
#'
#' Estimates conditional survivorship probabilities using orphanhood methods
#' and harmonizes them into common adult mortality indices using a
#' one-parameter relational logit model.
#'
#' @details
#' Three orphanhood-based estimation methods are supported. Regardless of the
#' method selected, estimates are harmonized into three comparable mortality
#' indices:
#' \itemize{
#'   \item \strong{30q30}: Probability of dying between ages 30 and 60.
#'   \item \strong{45q15}: Probability of dying between ages 15 and 60.
#'   \item \strong{e30}: Life expectancy at age 30.
#' }
#'
#' For the Brass method, adjacent orphanhood proportions are paired across
#' successive age groups. The first observation yields \code{NA} by
#' construction because no preceding adjacent proportion exists.
#'
#' @param method Character. Estimation method. One of \code{"luy"},
#'   \code{"timaeus"}, or \code{"brass"}.
#' @param sex_parent Character. Sex of the parent. One of \code{"Female"} or
#'   \code{"Male"}.
#' @param age_respondent Numeric vector. Lower bound of respondent age groups
#'   (e.g., \code{c(15, 20, 25, 30)}).
#' @param p_surv Numeric vector. Proportion of respondents reporting the parent
#'   alive, one value per age group.
#' @param mean_age_parent Numeric scalar or vector. Mean age of parents at
#'   respondent's birth (\eqn{M}). A scalar is recycled across all age groups.
#' @param surv_date Numeric. Survey reference date as a decimal year
#'   (e.g., \code{2015.5}).
#' @param num_respondents Numeric vector. Optional. Number of respondents per
#'   age group. Reserved for future use; currently has no effect on estimates.
#' @param model_family Character. Model life table family used as the relational
#'   logit standard. UN families: \code{"General"}, \code{"Latin"},
#'   \code{"Chilean"}, \code{"South_Asian"}, \code{"Far_East_Asian"}.
#'   Coale-Demeny families: \code{"West"}, \code{"North"}, \code{"East"},
#'   \code{"South"}. Default is \code{"General"}.
#'
#' @return An object of class \code{OrphanhoodEstimate}, a named list with:
#'   \describe{
#'     \item{\code{estimates}}{
#'       Data frame of estimated adult mortality indices by respondent age
#'       group. Columns:
#'       \describe{
#'         \item{\code{Method}}{
#'           Citation label of the estimation method used.
#'         }
#'         \item{\code{RefYear}}{
#'           Estimated reference year of the mortality estimate, expressed
#'           as a decimal year.
#'         }
#'         \item{\code{Age}}{
#'           Lower bound of the respondent age group (\eqn{n}) used to
#'           derive the orphanhood estimate.
#'         }
#'         \item{\code{b}}{
#'           Initial exact age of the conditional survivorship interval.
#'           Equals 30 for Luy (2012); 25 for Timaeus (1992) females,
#'           35 for males; 25 for Brass (1973) females, 32.5 or 37.5
#'           for males depending on \eqn{\bar{M}}.
#'         }
#'         \item{\code{n_b}}{
#'           Terminal exact age of the survivorship interval
#'           (\eqn{b + \text{duration}}). For Luy (2012) this equals
#'           \eqn{33 + n}.
#'         }
#'         \item{\code{l(b)}}{
#'           Estimated survivorship at exact age \eqn{b}, derived from
#'           the relational logit model.
#'         }
#'         \item{\code{l(n_b)}}{
#'           Estimated survivorship at exact age \eqn{n\_b}, derived from
#'           the relational logit model.
#'         }
#'         \item{\code{lx_ratio}}{
#'           Conditional survivorship probability \eqn{l(n\_b) / l(b)},
#'           i.e.\ the probability of surviving from exact age \eqn{b}
#'           to exact age \eqn{n\_b}. This is the key observable quantity
#'           derived from the orphanhood method and used to estimate
#'           \code{Alpha}.
#'         }
#'         \item{\code{Alpha}}{
#'           Relational logit level parameter \eqn{\alpha} estimated by
#'           solving \eqn{l(n\_b) / l(b) = \text{lx\_ratio}} against the
#'           standard life table. Higher values indicate lower mortality.
#'         }
#'         \item{\code{30q30}}{
#'           Probability of dying between exact ages 30 and 60.
#'         }
#'         \item{\code{45q15}}{
#'           Probability of dying between exact ages 15 and 60.
#'         }
#'         \item{\code{e30}}{
#'           Life expectancy at exact age 30.
#'         }
#'       }
#'     }
#'     \item{\code{meta}}{
#'       Named list of metadata: sex of the parent, model life table family,
#'       family system type (\code{"UN"} or \code{"CD"}), method identifier,
#'       and primary index name.
#'     }
#'     \item{\code{inputs}}{
#'       Named list of the original input arguments, retained for
#'       reproducibility and use by \code{\link{om_sensitivity}} and
#'       \code{\link{om_sensitivity_family}}.
#'     }
#'   }
#'
#' @seealso \code{\link{om_sensitivity}}, \code{\link{om_sensitivity_family}},
#'   \code{\link{om_plot_linearity}}
#'
#' @examples
#' \dontrun{
#' result <- om_estimate_index(
#'   method          = "brass",
#'   sex_parent      = "Female",
#'   age_respondent  = c(20, 25, 30, 35, 40, 45),
#'   p_surv          = c(0.906, 0.840, 0.747, 0.631, 0.518, 0.400),
#'   mean_age_parent = 28.8,
#'   surv_date       = 1975.64,
#'   model_family    = "General"
#' )
#' print(result)
#' }
#'
#' @export
om_estimate_index <- function(method          = c("luy", "timaeus", "brass"),
                              sex_parent      = c("Female", "Male"),
                              age_respondent,
                              p_surv,
                              mean_age_parent,
                              surv_date,
                              num_respondents = NULL,
                              model_family    = "General") {

  method     <- match.arg(method)
  sex_parent <- match.arg(sex_parent)

  if (!is.null(num_respondents) &&
      length(num_respondents) != length(age_respondent)) {
    stop("Length of 'num_respondents' must match 'age_respondent'.",
         call. = FALSE)
  }

  # --- 1. Dispatch to method-specific internal function -------------------

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
    raw      <- .om_brass(
      sex    = sex_parent,
      age    = age_respondent,
      sn     = p_surv,
      mn     = mean_age_parent,
      date   = surv_date,
      sn_10  = NULL,
      n_resp = num_respondents
    )
  }

  # --- 2. Load and filter model life table standard -----------------------

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
    std_data$Sex      == sex_parent    &
      std_data$Family   == model_family  &
      std_data$Type_MLT == selected_type &
      std_data$E0       == 60, ]

  if (nrow(std) == 0L) {
    stop(sprintf(
      "Standard life table not found for Family = '%s', Sex = '%s'.",
      model_family, sex_parent),
      call. = FALSE)
  }

  logit     <- function(x) 0.5 * log((1 - x) / x)
  inv_logit <- function(y) 1 / (1 + exp(2 * y))

  std$Yx    <- logit(std$lx)
  Y_std_fun <- stats::approxfun(std$Age, std$Yx, rule = 2)

  # --- 3. Estimate alpha and mortality indices row-by-row -----------------

  n_rows        <- nrow(raw)
  raw$Alpha     <- rep(NA_real_, n_rows)
  raw$lx_base   <- rep(NA_real_, n_rows)
  raw$lx_n      <- rep(NA_real_, n_rows)
  raw$est_e30   <- rep(NA_real_, n_rows)
  raw$est_45q15 <- rep(NA_real_, n_rows)
  raw$est_30q30 <- rep(NA_real_, n_rows)

  raw$DisplayAge <- raw$age_input

  for (i in seq_len(n_rows)) {

    if (is.na(raw$prob[i])) next

    val_obs <- raw$prob[i]
    age_b   <- raw$base_age[i]
    age_t   <- raw$target_age[i]

    Ys_b <- Y_std_fun(age_b)
    Ys_t <- Y_std_fun(age_t)

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

      age_seq <- seq(15L, max(std$Age), by = 1L)
      lx_seq  <- inv_logit(alpha_val + Y_std_fun(age_seq))

      l15 <- lx_seq[age_seq == 15L]
      l30 <- lx_seq[age_seq == 30L]
      l60 <- lx_seq[age_seq == 60L]

      raw$est_45q15[i] <- 1 - (l60 / l15)
      raw$est_30q30[i] <- 1 - (l60 / l30)

      idx_30  <- which(age_seq == 30L)
      lx_tail <- lx_seq[idx_30:length(lx_seq)]
      Lx_tail <- 0.5 * (lx_tail[-length(lx_tail)] + lx_tail[-1L])
      T30_val <- sum(Lx_tail, na.rm = TRUE) + (utils::tail(lx_tail, 1L) * 0.5)
      e30_est <- T30_val / lx_tail[1L]

      raw$est_e30[i] <- if (method == "luy") {
        sex_correction <- c(Female = 0.9709, Male = 0.9837)
        e30_est * sex_correction[[sex_parent]]
      } else {
        e30_est
      }

    }, error = function(e) invisible(NULL))
  }

  # --- 4. Monotonicity check on reference dates ---------------------------
  # Reference dates must decrease as respondent age increases.
  # Violations exceeding .MONOTONICITY_TOLERANCE years are set to NA.

  last_valid_date <- Inf
  violation_count <- 0L

  for (k in seq_len(nrow(raw))) {
    curr <- raw$ref_date[k]
    if (is.na(curr)) next

    if (curr < last_valid_date) {
      last_valid_date <- curr
    } else if ((curr - last_valid_date) > .MONOTONICITY_TOLERANCE) {
      raw$ref_date[k] <- NA_real_
      violation_count <- violation_count + 1L
    }
  }

  if (violation_count > 0L) {
    warning(sprintf(
      paste("Monotonicity check: %d estimate(s) removed due to",
            "reference date inconsistency > %.2f years."),
      violation_count, .MONOTONICITY_TOLERANCE),
      call. = FALSE)
  }

  # --- 5. Assemble output data frame --------------------------------------
  output_df <- data.frame(
    Method   = citation,
    RefYear  = raw$ref_date,
    Age      = raw$DisplayAge,
    b        = raw$base_age,
    n_b      = raw$target_age,
    "l(b)"   = raw$lx_base,
    "l(n_b)" = raw$lx_n,
    lx_ratio = raw$prob,
    Alpha    = raw$Alpha,
    "30q30"  = raw$est_30q30,
    "45q15"  = raw$est_45q15,
    e30      = raw$est_e30,
    check.names = FALSE
  )

  # --- 6. Construct and return S3 object ----------------------------------

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
      model_family    = model_family
    )
  )

  class(out_obj) <- "OrphanhoodEstimate"
  out_obj
}


# ------------------------------------------------------------------------------
# S3 methods
# ------------------------------------------------------------------------------

#' Print method for \code{OrphanhoodEstimate} objects
#'
#' @param x An object of class \code{OrphanhoodEstimate}.
#' @param ... Further arguments passed to \code{\link{print.data.frame}}.
#'
#' @return Invisibly returns \code{x}.
#' @export
print.OrphanhoodEstimate <- function(x, ...) {
  cat("\n=== Orphanhood Mortality Estimates ===\n")
  cat(sprintf("Method:    %s\n", x$estimates$Method[1L]))
  cat(sprintf("Target:    %s parent (%s \u2014 %s)\n",
              x$meta$sex, x$meta$type, x$meta$family))
  cat(sprintf("Age ref:   %s\n\n",
              if (x$meta$method_id == "timaeus") "Upper bound (n)" else "Lower bound (n)"))

  print_df        <- x$estimates
  print_df$Method <- NULL

  for (col in c("l(b)", "l(n_b)", "lx_ratio")) {
    if (col %in% names(print_df))
      print_df[[col]] <- round(print_df[[col]], 4L)
  }
  print_df$RefYear <- round(print_df$RefYear, 2L)
  print_df$Alpha   <- round(print_df$Alpha,   3L)

  for (col in c("30q30", "45q15")) {
    if (col %in% names(print_df))
      print_df[[col]] <- round(print_df[[col]], 4L)
  }
  if ("e30" %in% names(print_df))
    print_df[["e30"]] <- round(print_df[["e30"]], 2L)

  print(print_df, row.names = FALSE, ...)
  invisible(x)
}


#' Summary method for \code{OrphanhoodEstimate} objects
#'
#' @param object An object of class \code{OrphanhoodEstimate}.
#' @param ... Further arguments (currently unused).
#'
#' @return Invisibly returns \code{object}.
#' @export
summary.OrphanhoodEstimate <- function(object, ...) {
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

  cat(sprintf("Summary for %s parent mortality (index: %s):\n",
              object$meta$sex, idx_label))

  if (length(vals) > 0L) {
    cat(sprintf("  Range:   %.4f \u2013 %.4f\n", min(vals), max(vals)))
    cat(sprintf("  Mean:    %.4f\n",              mean(vals)))
    cat(sprintf("  Median:  %.4f\n",              stats::median(vals)))
  } else {
    cat("  No valid estimates found.\n")
  }

  invisible(object)
}


# ------------------------------------------------------------------------------
# Internal estimation functions
# ------------------------------------------------------------------------------

#' Luy (2012) orphanhood estimation
#'
#' @param sex Character. \code{"Female"} or \code{"Male"}.
#' @param age Numeric vector. Respondent age group lower bounds.
#' @param sn Numeric vector. Parental survival proportions.
#' @param mn Numeric scalar or vector. Mean age of parents at respondent's
#'   birth.
#' @param date Numeric. Survey reference date as a decimal year.
#'
#' @return A data frame with columns \code{age_input}, \code{base_age},
#'   \code{target_age}, \code{prob}, and \code{ref_date}.
#'
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
      x_vals <- as.numeric(gsub("[^0-9.]", "", names(row)[-1L]))
      y_vals <- as.numeric(unlist(row[1L, -1L]))
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
      lag <- if (curr_sn > 0 && !is.na(An)) An * log(curr_sn) + Bn else NA_real_
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
#'
#' @return A data frame with columns \code{age_input}, \code{base_age},
#'   \code{target_age}, \code{prob}, and \code{ref_date}.
#'
#' @keywords internal
.om_timaeus <- function(sex, age, sn, mn, date) {
  coefs    <- coef_timaeus[[sex]]
  has_d    <- "d_n" %in% names(coefs)
  out_list <- vector("list", length(age))

  if (length(mn) == 1L) {
    mn <- rep(mn, length(age))
  } else if (length(mn) != length(age)) {
    mn <- rep_len(mn, length(age))
  }

  for (i in seq_along(age)) {
    curr_age <- age[i]
    curr_mn  <- mn[i]

    if (sex == "Male") {
      base        <- 35L
      n_dur       <- curr_age + 5L
      sn_curr_eff <- if ((i + 1L) <= length(sn)) sn[i]       else NA_real_
      sn_lead_eff <- if ((i + 1L) <= length(sn)) sn[i + 1L]  else NA_real_
    } else {
      base        <- 25L
      n_dur       <- curr_age + 5L
      sn_curr_eff <- sn[i]
      sn_lead_eff <- NA_real_
    }

    cf <- coefs[coefs$Age == n_dur, ]

    est <- if (nrow(cf) == 0L || is.na(curr_mn) || is.na(sn_curr_eff) ||
               (has_d && is.na(sn_lead_eff))) {
      NA_real_
    } else {
      cf$a_n + cf$b_n * curr_mn + cf$c_n * sn_curr_eff +
        if (has_d) cf$d_n * sn_lead_eff else 0
    }

    T_lag <- NA_real_

    if (!is.na(curr_mn) && !is.na(est)) {
      if (sex == "Female") {
        N       <- curr_age + 2.5
        safe_sn <- max(sn_curr_eff, 0.001)
        num     <- 80 - curr_mn - N
        den     <- 80 - curr_mn
        if (num > 0 && den > 0)
          T_lag <- (N / 2) * (1 - (1/3) * log(safe_sn) + (1/3) * log(num / den))
      } else {
        N         <- as.numeric(n_dur)
        geom_mean <- sqrt(max(sn_curr_eff, 0.001) * max(sn_lead_eff, 0.001))
        num       <- 80 - curr_mn - N
        den       <- 80 - (curr_mn - 0.75)
        if (num > 0 && den > 0)
          T_lag <- ((N + 0.75) / 2) *
          (1 - (1/3) * log(geom_mean) + (1/3) * log(num / den))
      }
    }

    out_list[[i]] <- data.frame(
      age_input  = curr_age,
      base_age   = base,
      target_age = if (is.na(n_dur)) NA_real_ else base + n_dur,
      prob       = est,
      ref_date   = if (!is.na(date) && !is.na(T_lag)) date - T_lag else NA_real_
    )
  }

  do.call(rbind, out_list)
}


#' Brass (1973) / Hill orphanhood estimation
#'
#' @inheritParams .om_luy
#' @param sn_10 Numeric. Optional proportion with parent alive for the age
#'   group immediately preceding the first entry in \code{age}. When supplied,
#'   enables estimation for the first respondent age group; otherwise that
#'   row returns \code{NA} by construction.
#' @param n_resp Numeric vector. Reserved for future use.
#'
#' @return A data frame with columns \code{age_input}, \code{base_age},
#'   \code{target_age}, \code{prob}, and \code{ref_date}.
#'
#' @details
#' Implements UN Manual X equations B.4--B.8. Conditional survivorship is
#' estimated via weighting factors \eqn{W(n)} (Hill 1977):
#'
#' \deqn{\frac{l(25+n)}{l(25)} = W(n)\,S(n-5) + (1 - W(n))\,S(n)}
#'
#' The reference time \eqn{t(n)} is located using:
#'
#' \deqn{u(n) = \frac{1}{3} \ln S(n) + Z(M+n) + 0.0037\,(27 - M)}
#' \deqn{t(n) = \frac{n\,(1 - u(n))}{2}}
#'
#' For fathers, base ages 32.5 and 37.5 replace 25 (equations B.5--B.6),
#' and a correction of 0.75 is applied to \eqn{t(n)} and \eqn{Z}.
#'
#' @references
#' United Nations (1983). \emph{Manual X: Indirect Techniques for Demographic
#' Estimation}. ST/ESA/SER.A/81. New York: United Nations.
#'
#' @keywords internal
.om_brass <- function(sex, age, sn, mn, date, sn_10 = NULL, n_resp = NULL) {
  all_coefs <- coef_brass_hill
  z_tab     <- coef_z_brass
  out_list  <- vector("list", length(age))

  if (length(mn) == 1L) mn <- rep(mn, length(age))
  if (is.null(sn_10) || length(sn_10) == 0L) sn_10 <- NA_real_

  for (i in seq_along(age)) {
    curr_age <- age[i]
    curr_sn  <- sn[i]
    curr_mn  <- mn[i]

    if (sex == "Female") {
      base              <- 25L
      target_n          <- 25L + curr_age
      coefs             <- all_coefs$Female
      z_arg             <- curr_mn + curr_age
      correction_factor <- 0
    } else {
      if (curr_mn < 36) {
        base  <- 32.5
        coefs <- all_coefs$Male_A
      } else {
        base  <- 37.5
        coefs <- all_coefs$Male_B
      }
      target_n          <- base + curr_age + 2.5
      z_arg             <- curr_mn + curr_age + 0.75
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

      z_val <- stats::approx(z_tab$Age, z_tab$Z, xout = z_arg, rule = 2)$y
      u_n   <- (1/3) * log(max(curr_sn, 1e-12)) +
        z_val +
        0.0037 * (27 - curr_mn + correction_factor)
      lag   <- ((curr_age + correction_factor) * (1 - u_n)) / 2
      ref   <- date - lag
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
