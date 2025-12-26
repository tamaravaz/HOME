#' @import ggplot2
NULL

# CRAN check fix for ggplot2 NSE
utils::globalVariables(c(
  "RefTime", "Offset_M", "Alpha", "RespondentAge", "Family"
))

# ==============================================================================
# 1. INTERNAL HELPER: INPUT RESOLUTION
# ==============================================================================

#' Resolve Inputs for Diagnostic Functions
#'
#' Internal helper to extract arguments from an existing object or use raw inputs.
#'
#' @param object Optional. An object of class \code{OrphanhoodEstimate}.
#' @param provided_args A list of arguments captured from \code{...}.
#'
#' @return A list of combined arguments ready for \code{om_estimate_index}.
#' @noRd
.resolve_inputs <- function(object, provided_args) {

  if (!is.null(object)) {

    if (!inherits(object, "OrphanhoodEstimate")) {
      stop("The 'object' must be of class 'OrphanhoodEstimate'.")
    }

    final_args <- object$inputs

    for (nm in names(provided_args)) {
      final_args[[nm]] <- provided_args[[nm]]
    }

    return(final_args)
  }

  provided_args
}

# ==============================================================================
# 2. SENSITIVITY: MEAN AGE OF CHILDBEARING (M)
# ==============================================================================

#' Run Sensitivity Diagnostics on Orphanhood Estimates
#'
#' Performs a sensitivity analysis by varying the Mean Age of Childbearing (M)
#' while holding all other assumptions constant, including the level of the
#' standard life table.
#'
#' @param object Optional. An object of class \code{OrphanhoodEstimate}.
#' @param range_m Numeric vector. Offsets applied to M
#'        (default: \code{seq(-1.5, 1.5, 0.5)}).
#' @param ... Additional arguments passed to \code{om_estimate_index}.
#'
#' @return An object of class \code{OrphanhoodSensitivity}.
#' @export
om_sensitivity <- function(object = NULL,
                           range_m = seq(-1.5, 1.5, 0.5),
                           ...) {

  raw_args <- list(...)
  args <- .resolve_inputs(object, raw_args)

  if (is.null(args$mean_age_parent)) {
    stop("Missing 'mean_age_parent'. Provide an object or pass arguments via '...'.")
  }

  base_mn <- args$mean_age_parent
  results <- list()

  for (offset in range_m) {

    curr_args <- args
    curr_args$mean_age_parent <- base_mn + offset

    suppressWarnings({
      res <- do.call(om_estimate_index, curr_args)
    })

    if (!is.null(res$estimates)) {
      df <- res$estimates
      df$Offset_M <- offset
      results[[as.character(offset)]] <- df
    }
  }

  final_df <- do.call(rbind, results)

  out <- list(
    data = final_df,
    meta = list(
      variable = "Mean Age of Childbearing (M)",
      range = range_m,
      std_level = args$std_level,
      method = args$method
    )
  )

  class(out) <- "OrphanhoodSensitivity"
  out
}

#' Plot Sensitivity to Mean Age of Childbearing
#'
#' @param x An object of class \code{OrphanhoodSensitivity}.
#' @param index Character. Mortality index to plot ("e30", "45q15", or "30q30").
#' @param ... Unused.
#'
#' @export
plot.OrphanhoodSensitivity <- function(x, index = "e30", ...) {

  df <- x$data

  if (!index %in% names(df)) {
    stop(sprintf("Index '%s' not found in results.", index))
  }

  ggplot(df, aes(
    x = RefTime,
    y = .data[[index]],
    group = Offset_M,
    color = Offset_M
  )) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    scale_color_gradient(
      low = "grey80",
      high = "black",
      name = "Adj. M (yrs)"
    ) +
    labs(
      title = paste("Sensitivity of", index, "to Mean Age of Childbearing"),
      x = "Reference Date",
      y = index
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}

# ==============================================================================
# 3. SENSITIVITY: MODEL LIFE TABLE FAMILY
# ==============================================================================

#' Run Sensitivity Analysis on Model Life Table Families
#'
#' Evaluates the sensitivity of orphanhood-based estimates to the assumed
#' age pattern of mortality by varying the Model Life Table family.
#'
#' @param object Optional. An object of class \code{OrphanhoodEstimate}.
#' @param type Character. One of \code{"UN"}, \code{"CD"}, or \code{"All"}.
#' @param families Optional character vector of families to test.
#' @param ... Additional arguments passed to \code{om_estimate_index}.
#'
#' @return An object of class \code{OrphanhoodSensitivityFamily}.
#' @export
om_sensitivity_family <- function(object = NULL,
                                  type = c("UN", "CD", "All"),
                                  families = NULL,
                                  ...) {

  raw_args <- list(...)
  args <- .resolve_inputs(object, raw_args)

  map_families <- list(
    UN = c("General", "Latin", "Chilean",
           "South_Asian", "Far_East_Asian"),
    CD = c("West", "North", "East", "South")
  )

  if (!is.null(families)) {
    target_families <- families
    system_name <- "Custom"
  } else {
    type <- match.arg(type)
    target_families <- if (type == "All") {
      unlist(map_families)
    } else {
      map_families[[type]]
    }
    system_name <- type
  }

  results <- list()

  for (fam in target_families) {

    curr_args <- args
    curr_args$model_family <- fam

    suppressWarnings({
      res <- try(do.call(om_estimate_index, curr_args), silent = TRUE)
    })

    if (!inherits(res, "try-error") && !is.null(res$estimates)) {
      df <- res$estimates
      df$Family <- fam
      results[[fam]] <- df
    }
  }

  if (length(results) == 0) {
    stop("No valid estimates produced for the selected families.")
  }

  out <- list(
    data = do.call(rbind, results),
    meta = list(
      variable = "Model Life Table Family",
      range = target_families,
      std_level = args$std_level,
      system = system_name,
      method = args$method
    )
  )

  class(out) <- "OrphanhoodSensitivityFamily"
  out
}

#' Plot Model Life Table Family Sensitivity
#'
#' @param x An object of class \code{OrphanhoodSensitivityFamily}.
#' @param index Character. Mortality index to plot.
#' @param ... Unused.
#'
#' @export
plot.OrphanhoodSensitivityFamily <- function(x, index = "e30", ...) {

  df <- x$data

  ggplot(df, aes(
    x = RefTime,
    y = .data[[index]],
    group = Family,
    linetype = Family
  )) +
    geom_line(linewidth = 0.6) +
    labs(
      title = paste("Sensitivity of", index, "to Model Life Table Family"),
      x = "Reference Date",
      y = index,
      linetype = NULL
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}

# ==============================================================================
# 4. INTERNAL CONSISTENCY CHECK (ALPHA)
# ==============================================================================

#' Diagnostic Plot: Internal Consistency Check
#'
#' Displays the estimated alpha parameter across respondent ages.
#'
#' @param object An object of class \code{OrphanhoodEstimate}.
#'
#' @return A ggplot object.
#' @export
om_plot_linearity <- function(object) {

  if (!inherits(object, "OrphanhoodEstimate")) {
    stop("Object must be of class 'OrphanhoodEstimate'.")
  }

  df <- object$estimates

  ggplot(df, aes(x = RespondentAge, y = Alpha)) +
    geom_hline(
      yintercept = stats::median(df$Alpha, na.rm = TRUE),
      linetype = "dashed",
      color = "grey50"
    ) +
    geom_line(group = 1) +
    geom_point(size = 2) +
    labs(
      title = "Internal Consistency Check",
      x = "Respondent Age Group",
      y = "Alpha (Mortality Level)"
    ) +
    theme_bw()
}

# ==============================================================================
# 5. DASHBOARD
# ==============================================================================

#' Create a Diagnostic Dashboard
#'
#' Produces a combined panel of diagnostic plots for orphanhood estimates.
#'
#' @param object An object of class \code{OrphanhoodEstimate}.
#' @param index Character. Mortality index to display.
#' @param family_type Character. "UN", "CD", or "All".
#' @param range_m Numeric vector. Range for sensitivity to M.
#'
#' @export
om_dashboard <- function(object,
                         index = "e30",
                         family_type = "UN",
                         range_m = seq(-1.5, 1.5, 0.5)) {

  if (!inherits(object, "OrphanhoodEstimate")) {
    stop("Object must be of class 'OrphanhoodEstimate'.")
  }

  sens_m   <- om_sensitivity(object, range_m = range_m)
  sens_fam <- om_sensitivity_family(object, type = family_type)

  p1 <- plot(sens_m, index = index)
  p2 <- plot(sens_fam, index = index)
  p3 <- om_plot_linearity(object)

  if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::grid.arrange(
      p3,
      gridExtra::arrangeGrob(p1, p2, ncol = 2),
      nrow = 2,
      top = paste("Diagnostics for", toupper(object$meta$method))
    )
  } else {
    message("Install 'gridExtra' for a combined dashboard view.")
    list(Linearity = p3, Sensitivity_M = p1, Sensitivity_Family = p2)
  }
}
