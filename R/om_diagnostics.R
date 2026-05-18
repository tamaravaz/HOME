# ==============================================================================
# om_diagnostics.R
# Sensitivity analyses and diagnostic plots for OrphanhoodEstimate objects.
# ==============================================================================

#' @import ggplot2
#' @importFrom stats median
#' @keywords internal
NULL

# CRAN NOTE fix: suppress R CMD check warnings for ggplot2 non-standard
# evaluation variables used in aes() calls throughout this file.
utils::globalVariables(c(
  "RefYear", "Offset_M", "Alpha", "Age", "Family"
))

# ------------------------------------------------------------------------------
# Internal helper
# ------------------------------------------------------------------------------

#' Resolve inputs for diagnostic functions
#'
#' Extracts estimation arguments from an existing \code{OrphanhoodEstimate}
#' object, optionally overriding individual arguments supplied via \code{...}.
#'
#' @param object Optional. An object of class \code{OrphanhoodEstimate}.
#' @param provided_args A list of arguments captured from \code{...} in the
#'   calling function.
#'
#' @return A named list of arguments suitable for passing to
#'   \code{\link{om_estimate_index}}.
#' @keywords internal
.resolve_inputs <- function(object, provided_args) {
  if (!is.null(object)) {
    if (!inherits(object, "OrphanhoodEstimate")) {
      stop("'object' must be of class 'OrphanhoodEstimate'.", call. = FALSE)
    }
    final_args <- object$inputs
    for (nm in names(provided_args)) {
      final_args[[nm]] <- provided_args[[nm]]
    }
    return(final_args)
  }
  provided_args
}

# ------------------------------------------------------------------------------
# Sensitivity: mean age of childbearing (M)
# ------------------------------------------------------------------------------

#' Sensitivity Analysis: Mean Age of Childbearing
#'
#' Evaluates the sensitivity of orphanhood-based mortality estimates to
#' assumptions about the mean age of childbearing (\eqn{M_n}) by
#' re-estimating indices across a grid of additive offsets.
#'
#' @param object Optional. An object of class \code{OrphanhoodEstimate}.
#'   When supplied, all estimation arguments are taken from \code{object$inputs}
#'   and may be overridden via \code{...}.
#' @param range_m Numeric vector of offsets (in years) applied to the baseline
#'   mean age of childbearing. Default: \code{seq(-1.5, 1.5, 0.5)}.
#' @param ... Additional arguments passed to \code{\link{om_estimate_index}},
#'   overriding values stored in \code{object}.
#'
#' @return An object of class \code{OrphanhoodSensitivity}, a named list with:
#'   \describe{
#'     \item{\code{data}}{
#'       A data frame of estimates for each offset value, with columns
#'       matching \code{om_estimate_index()$estimates} plus an additional
#'       column \code{Offset_M} identifying the applied offset in years.
#'     }
#'     \item{\code{meta}}{
#'       A named list of metadata: variable name, offset range, standard
#'       level, and estimation method.
#'     }
#'   }
#'
#' @seealso \code{\link{plot.OrphanhoodSensitivity}},
#'   \code{\link{om_sensitivity_family}}, \code{\link{om_estimate_index}}
#'
#' @examples
#' \dontrun{
#'   result <- om_estimate_index(
#'     method          = "luy",
#'     sex_parent      = "Female",
#'     age_respondent  = seq(20, 60, by = 5),
#'     p_surv          = c(0.987, 0.967, 0.934, 0.908, 0.882,
#'                         0.835, 0.769, 0.669, 0.565),
#'     mean_age_parent = rep(27, 9),
#'     surv_date       = 1998.5
#'   )
#'   sens <- om_sensitivity(result, range_m = seq(-2, 2, by = 0.5))
#'   plot(sens, index = "30q30")
#' }
#'
#' @export
om_sensitivity <- function(object  = NULL,
                           range_m = seq(-1.5, 1.5, 0.5),
                           ...) {
  raw_args <- list(...)
  args     <- .resolve_inputs(object, raw_args)

  if (is.null(args$mean_age_parent)) {
    stop(
      "Missing 'mean_age_parent'. ",
      "Provide an 'OrphanhoodEstimate' object or supply arguments via '...'.",
      call. = FALSE
    )
  }

  base_mn <- args$mean_age_parent
  results <- list()

  for (offset in range_m) {
    curr_args                 <- args
    curr_args$mean_age_parent <- base_mn + offset

    suppressWarnings({
      res <- do.call(om_estimate_index, curr_args)
    })

    if (!is.null(res$estimates)) {
      df          <- res$estimates
      df$Offset_M <- offset
      results[[as.character(offset)]] <- df
    }
  }

  out <- list(
    data = do.call(rbind, results),
    meta = list(
      variable  = "Mean Age of Childbearing (M)",
      range     = range_m,
      std_level = args$std_level,
      method    = args$method
    )
  )

  class(out) <- "OrphanhoodSensitivity"
  out
}

#' Plot Method for \code{OrphanhoodSensitivity} Objects
#'
#' Displays estimated mortality indices across reference years for each
#' mean-age-of-childbearing offset, coloured from light (low offset) to
#' dark (high offset).
#'
#' @param x An object of class \code{OrphanhoodSensitivity}.
#' @param index Character. Mortality index to plot. One of \code{"30q30"},
#'   \code{"45q15"}, or \code{"e30"}. Default: \code{"30q30"}.
#' @param ... Further arguments (currently unused).
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{om_sensitivity}}
#'
#' @export
plot.OrphanhoodSensitivity <- function(x, index = "30q30", ...) {
  df <- x$data

  if (!index %in% names(df)) {
    stop(sprintf("Index '%s' not found in sensitivity results.", index),
         call. = FALSE)
  }

  ggplot2::ggplot(df, ggplot2::aes(
    x     = RefYear,
    y     = .data[[index]],
    group = Offset_M,
    color = Offset_M
  )) +
    ggplot2::geom_line(linewidth = 0.6, alpha = 0.8) +
    ggplot2::scale_color_gradient(
      low  = "grey80",
      high = "black",
      name = "Adj. M (yrs)"
    ) +
    ggplot2::labs(
      title = paste("Sensitivity of", index, "to Mean Age of Childbearing"),
      x     = "Reference Year",
      y     = index
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# Sensitivity: model life table family
# ------------------------------------------------------------------------------

#' Sensitivity Analysis: Model Life Table Family
#'
#' Evaluates the sensitivity of orphanhood-based mortality estimates to the
#' assumed age pattern of mortality by re-estimating indices across multiple
#' model life table families.
#'
#' @param object Optional. An object of class \code{OrphanhoodEstimate}.
#' @param type Character. Family system to evaluate. One of \code{"UN"},
#'   \code{"CD"}, or \code{"All"} (default). Ignored when \code{families}
#'   is supplied.
#' @param families Optional character vector of specific family names to test,
#'   overriding \code{type}.
#' @param ... Additional arguments passed to \code{\link{om_estimate_index}}.
#'
#' @return An object of class \code{OrphanhoodSensitivityFamily}, a named list
#'   with:
#'   \describe{
#'     \item{\code{data}}{
#'       A data frame of estimates for each family, with columns matching
#'       \code{om_estimate_index()$estimates} plus an additional column
#'       \code{Family} identifying the model life table family.
#'     }
#'     \item{\code{meta}}{
#'       A named list of metadata: variable name, families tested, standard
#'       level, family system name, and estimation method.
#'     }
#'   }
#'
#' @seealso \code{\link{plot.OrphanhoodSensitivityFamily}},
#'   \code{\link{om_sensitivity}}, \code{\link{om_estimate_index}}
#'
#' @examples
#' \dontrun{
#'   result <- om_estimate_index(
#'     method          = "luy",
#'     sex_parent      = "Female",
#'     age_respondent  = seq(20, 60, by = 5),
#'     p_surv          = c(0.987, 0.967, 0.934, 0.908, 0.882,
#'                         0.835, 0.769, 0.669, 0.565),
#'     mean_age_parent = rep(27, 9),
#'     surv_date       = 1998.5
#'   )
#'   sens_fam <- om_sensitivity_family(result, type = "UN")
#'   plot(sens_fam, index = "30q30")
#' }
#'
#' @export
om_sensitivity_family <- function(object   = NULL,
                                  type     = c("UN", "CD", "All"),
                                  families = NULL,
                                  ...) {
  raw_args <- list(...)
  args     <- .resolve_inputs(object, raw_args)

  map_families <- list(
    UN = c("General", "Latin", "Chilean", "South_Asian", "Far_East_Asian"),
    CD = c("West", "North", "East", "South")
  )

  if (!is.null(families)) {
    target_families <- families
    system_name     <- "Custom"
  } else {
    type            <- match.arg(type)
    target_families <- if (type == "All") unlist(map_families) else map_families[[type]]
    system_name     <- type
  }

  results <- list()

  for (fam in target_families) {
    curr_args              <- args
    curr_args$model_family <- fam

    suppressWarnings({
      res <- tryCatch(
        do.call(om_estimate_index, curr_args),
        error = function(e) NULL
      )
    })

    if (!is.null(res) && !is.null(res$estimates)) {
      df        <- res$estimates
      df$Family <- fam
      results[[fam]] <- df
    }
  }

  if (length(results) == 0L) {
    stop("No valid estimates produced for the selected families.", call. = FALSE)
  }

  out <- list(
    data = do.call(rbind, results),
    meta = list(
      variable  = "Model Life Table Family",
      range     = target_families,
      std_level = args$std_level,
      system    = system_name,
      method    = args$method
    )
  )

  class(out) <- "OrphanhoodSensitivityFamily"
  out
}

#' Plot Method for \code{OrphanhoodSensitivityFamily} Objects
#'
#' Displays estimated mortality indices across reference years for each model
#' life table family, distinguished by line type.
#'
#' @param x An object of class \code{OrphanhoodSensitivityFamily}.
#' @param index Character. Mortality index to plot. One of \code{"30q30"},
#'   \code{"45q15"}, or \code{"e30"}. Default: \code{"30q30"}.
#' @param ... Further arguments (currently unused).
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{om_sensitivity_family}}
#'
#' @export
plot.OrphanhoodSensitivityFamily <- function(x, index = "30q30", ...) {
  df <- x$data

  if (!index %in% names(df)) {
    stop(sprintf("Index '%s' not found in sensitivity results.", index),
         call. = FALSE)
  }

  ggplot2::ggplot(df, ggplot2::aes(
    x        = RefYear,
    y        = .data[[index]],
    group    = Family,
    linetype = Family
  )) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::labs(
      title    = paste("Sensitivity of", index, "to Model Life Table Family"),
      x        = "Reference Year",
      y        = index,
      linetype = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

# ------------------------------------------------------------------------------
# Internal consistency diagnostic
# ------------------------------------------------------------------------------

#' Diagnostic Plot: Internal Consistency Check
#'
#' Displays the estimated Brass logit \eqn{\alpha} parameter across respondent
#' age groups. A roughly constant \eqn{\alpha} indicates internal consistency
#' of the orphanhood-based estimates; a systematic trend suggests age-reporting
#' errors, adoption effects, or structural changes in mortality.
#'
#' @param object An object of class \code{OrphanhoodEstimate}.
#'
#' @return A \code{ggplot} object with respondent age group (\code{Age}) on
#'   the x-axis and the relational logit level parameter \eqn{\alpha}
#'   (\code{Alpha}) on the y-axis. A dashed horizontal line marks the median
#'   \eqn{\alpha} across age groups.
#'
#' @seealso \code{\link{om_estimate_index}}, \code{\link{om_dashboard}}
#'
#' @examples
#' \dontrun{
#'   result <- om_estimate_index(
#'     method          = "luy",
#'     sex_parent      = "Female",
#'     age_respondent  = seq(20, 60, by = 5),
#'     p_surv          = c(0.987, 0.967, 0.934, 0.908, 0.882,
#'                         0.835, 0.769, 0.669, 0.565),
#'     mean_age_parent = rep(27, 9),
#'     surv_date       = 1998.5
#'   )
#'   om_plot_linearity(result)
#' }
#'
#' @export
om_plot_linearity <- function(object) {
  if (!inherits(object, "OrphanhoodEstimate")) {
    stop("'object' must be of class 'OrphanhoodEstimate'.", call. = FALSE)
  }

  df <- object$estimates

  ggplot2::ggplot(df, ggplot2::aes(x = Age, y = Alpha)) +
    ggplot2::geom_hline(
      yintercept = stats::median(df$Alpha, na.rm = TRUE),
      linetype   = "dashed",
      color      = "grey50"
    ) +
    ggplot2::geom_line(group = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Internal Consistency Check",
      x     = "Respondent Age Group",
      y     = "Alpha (Mortality Level)"
    ) +
    ggplot2::theme_bw()
}

# ------------------------------------------------------------------------------
# Combined diagnostic dashboard
# ------------------------------------------------------------------------------

#' Combined Diagnostic Dashboard
#'
#' Produces a three-panel diagnostic display combining the internal consistency
#' check with sensitivity analyses for mean age of childbearing and model life
#' table family. Requires the \pkg{gridExtra} package.
#'
#' @param object An object of class \code{OrphanhoodEstimate}.
#' @param index Character. Mortality index to display in sensitivity panels.
#'   One of \code{"30q30"}, \code{"45q15"}, or \code{"e30"}.
#'   Default: \code{"30q30"}.
#' @param family_type Character. Life table family system for the family
#'   sensitivity panel. One of \code{"UN"}, \code{"CD"}, or \code{"All"}.
#'   Default: \code{"UN"}.
#' @param range_m Numeric vector. Offsets applied to the mean age of
#'   childbearing. Default: \code{seq(-1.5, 1.5, 0.5)}.
#'
#' @return If \pkg{gridExtra} is available, arranges and displays the three
#'   plots and returns the \code{gtable} object invisibly. Otherwise returns a
#'   named list with elements \code{Linearity}, \code{Sensitivity_M}, and
#'   \code{Sensitivity_Family}.
#'
#' @seealso \code{\link{om_plot_linearity}}, \code{\link{om_sensitivity}},
#'   \code{\link{om_sensitivity_family}}
#'
#' @examples
#' \dontrun{
#'   result <- om_estimate_index(
#'     method          = "luy",
#'     sex_parent      = "Female",
#'     age_respondent  = seq(20, 60, by = 5),
#'     p_surv          = c(0.987, 0.967, 0.934, 0.908, 0.882,
#'                         0.835, 0.769, 0.669, 0.565),
#'     mean_age_parent = rep(27, 9),
#'     surv_date       = 1998.5
#'   )
#'   om_dashboard(result, index = "30q30", family_type = "UN")
#' }
#'
#' @export
om_dashboard <- function(object,
                         index       = "30q30",
                         family_type = "UN",
                         range_m     = seq(-1.5, 1.5, 0.5)) {
  if (!inherits(object, "OrphanhoodEstimate")) {
    stop("'object' must be of class 'OrphanhoodEstimate'.", call. = FALSE)
  }

  sens_m   <- om_sensitivity(object, range_m = range_m)
  sens_fam <- om_sensitivity_family(object, type = family_type)

  p1 <- om_plot_linearity(object)
  p2 <- plot(sens_m,   index = index)
  p3 <- plot(sens_fam, index = index)

  if (requireNamespace("gridExtra", quietly = TRUE)) {
    g <- gridExtra::grid.arrange(
      p1,
      gridExtra::arrangeGrob(p2, p3, ncol = 2L),
      nrow = 2L,
      top  = paste("Diagnostics \u2014", toupper(object$meta$method_id))
    )
    invisible(g)
  } else {
    message(
      "Install the 'gridExtra' package for a combined dashboard layout. ",
      "Returning individual plots as a list."
    )
    list(
      Linearity          = p1,
      Sensitivity_M      = p2,
      Sensitivity_Family = p3
    )
  }
}
