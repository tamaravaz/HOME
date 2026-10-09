# ==============================================================================
# schmertmann.R
#
# Schmertmann (2003) Quadratic Spline (QS) model fertility schedules, plus the
# schedule-shifting scheme Luy (2012, Appendix 2, Step 2) uses to move a
# fertility schedule to a target mean age at childbearing.
#
# The QS implementation follows Schmertmann (2003), conditions 1-11 and
# Appendix B, and has been checked against the worked example in the paper's
# Figure 2 (Netherlands 2001, alpha/P/H = 15.6/32.4/36.6): the computed knots
# reproduce the published [15.6, 26.8, 32.4, 34.5, 42.9] and the computed
# coefficients reproduce the published theta vector to the precision printed
# in the article. All five shape constraints hold exactly:
#   phi(P) = 1, phi(H) = 0.5, phi(beta) = 0, phi'(P) = 0, phi'(beta) = 0.
#
# The schedule-shifting scheme Luy applies is implemented in
# .luy_schedules() (R/om_calibrate_luy.R). Luy's shift scheme, verbatim from
# Appendix 2 Step 2: "For each year that the
# target mean was higher or lower than the base mean, P and H were increased
# by 2.0 and 1.0, or decreased by 1.0 and 2.0 years, respectively. The other
# two parameters of the Schmertmann model were always kept constant at
# alpha = 14 and f(P) as given by the basic fertility schedule." The
# Schmertmann shift is used only when the two means differ by more than one
# year; the relational Gompertz model then fine-tunes to the exact target.
# ==============================================================================

#' @importFrom stats optim uniroot
#' @keywords internal
NULL

#' Schmertmann quadratic spline knots and coefficients
#'
#' @param alpha Numeric. Youngest age at which fertility rises above zero.
#' @param P Numeric. Age at peak fertility.
#' @param H Numeric. Youngest age above \code{P} at which fertility falls to
#'   half its peak.
#' @return A list with \code{knots}, \code{theta}, \code{beta} and \code{W}.
#' @references Schmertmann, C. P. (2003). A system of model fertility
#'   schedules with graphically intuitive parameters. \emph{Demographic
#'   Research}, 9(5), 81-110.
#' @keywords internal
.qs_spline <- function(alpha, P, H) {
  if (!(alpha < P && P < H)) return(NULL)
  W  <- min(0.75, 0.25 + 0.025 * (P - alpha))
  lo <- H + (H - P) / 3
  hi <- H + 3 * (H - P)
  beta <- if (lo > 50) lo else if (hi < 50) hi else 50

  t  <- c(alpha, (1 - W) * alpha + W * P, P, (P + H) / 2, (H + beta) / 2)
  th <- numeric(5)
  th[1] <- 1 / (W * (P - alpha)^2)
  th[2] <- -th[1] / (1 - W)

  ZA <- (H - t[3])^2;    ZB <- (H - t[4])^2
  ZC <- (beta - t[3])^2; ZD <- (beta - t[4])^2; ZE <- (beta - t[5])^2
  ZF <- 2 * (beta - t[3]); ZG <- 2 * (beta - t[4]); ZH <- 2 * (beta - t[5])

  Z1 <- 0.5 - (th[1] * (H - alpha)^2 + th[2] * (H - t[2])^2)
  Z2 <-   0 - (th[1] * (beta - alpha)^2 + th[2] * (beta - t[2])^2)
  Z3 <-   0 - (2 * th[1] * (beta - alpha) + 2 * th[2] * (beta - t[2]))

  DEN <- ZA * ZD * ZH - ZA * ZE * ZG - ZC * ZB * ZH + ZF * ZB * ZE
  if (!is.finite(DEN) || abs(DEN) < 1e-12) return(NULL)
  th[3] <- (Z1 * (ZD * ZH - ZE * ZG) - Z2 * (ZB * ZH) + Z3 * (ZB * ZE)) / DEN
  th[4] <- (Z1 * (ZE * ZF - ZC * ZH) + Z2 * (ZA * ZH) - Z3 * (ZA * ZE)) / DEN
  th[5] <- (Z1 * (ZC * ZG - ZD * ZF) + Z2 * (ZB * ZF - ZA * ZG) +
              Z3 * (ZA * ZD - ZB * ZC)) / DEN

  list(knots = t, theta = th, beta = beta, W = W)
}

#' Shape function and cumulated fertility of a QS schedule
#'
#' \code{.qs_phi} is the point density (zero outside \eqn{(\alpha, \beta)}).
#' Luy evaluates it at integer ages and uses the value as the rate of
#' completed age \eqn{x}.
#' @param x Numeric vector of exact ages.
#' @param sp A spline object from \code{.qs_spline}.
#' @return Numeric vector of densities at \code{x}.
#' @keywords internal
.qs_phi <- function(x, sp)
  vapply(x, function(a) {
    if (a >= sp$beta) return(0)
    max(0, sum(sp$theta * pmax(0, a - sp$knots)^2))
  }, numeric(1))

#' @rdname dot-qs_phi
#' @keywords internal
.qs_cumf <- function(x, sp)
  vapply(x, function(a) {
    a <- min(a, sp$beta)
    sum(sp$theta * pmax(0, a - sp$knots)^3) / 3
  }, numeric(1))

#' Single-year rates 1fx implied by a QS schedule
#' @param ages Integer vector of ages (rate applies from age to age + 1).
#' @param sp A spline object from \code{.qs_spline}.
#' @param R Numeric level parameter (peak fertility).
#' @return Numeric vector of single-year rates at \code{ages}.
#' @keywords internal
.qs_1fx <- function(ages, sp, R = 1)
  R * (.qs_cumf(ages + 1, sp) - .qs_cumf(ages, sp))

#' Fit a QS schedule to an empirical single-year fertility schedule
#'
#' Minimises the unweighted sum of squared differences between the empirical
#' rates and the QS-implied single-year rates, as in Schmertmann (2003).
#'
#' @param ages Integer vector of single-year ages.
#' @param rates Numeric vector of fertility rates, same length as \code{ages}.
#' @param alpha_fixed Numeric or NULL. If numeric, \eqn{\alpha} is held at this
#'   value (Luy holds it at 14); if NULL it is estimated.
#' @return A list with \code{alpha}, \code{P}, \code{H}, \code{R},
#'   \code{spline} and \code{sse}, or NULL if the fit fails.
#' @keywords internal
.qs_fit <- function(ages, rates, alpha_fixed = 14) {
  ok <- is.finite(rates) & rates >= 0
  ages <- ages[ok]; rates <- rates[ok]
  if (length(ages) < 4 || sum(rates) <= 0) return(NULL)

  P0 <- ages[which.max(rates)] + 0.5
  above <- ages[ages > P0 & rates < max(rates) / 2]
  H0 <- if (length(above)) min(above) + 0.5 else P0 + 5
  R0 <- max(rates)
  a0 <- if (is.null(alpha_fixed)) max(10, min(ages) - 1) else alpha_fixed

  sse <- function(par) {
    al <- if (is.null(alpha_fixed)) par[4] else alpha_fixed
    sp <- .qs_spline(al, par[1], par[2])
    if (is.null(sp)) return(1e12)
    sum((.qs_1fx(ages, sp, par[3]) - rates)^2)
  }
  init <- if (is.null(alpha_fixed)) c(P0, H0, R0, a0) else c(P0, H0, R0)
  fit <- tryCatch(stats::optim(init, sse, method = "Nelder-Mead",
                               control = list(maxit = 2000, reltol = 1e-10)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  al <- if (is.null(alpha_fixed)) fit$par[4] else alpha_fixed
  sp <- .qs_spline(al, fit$par[1], fit$par[2])
  if (is.null(sp)) return(NULL)
  list(alpha = al, P = fit$par[1], H = fit$par[2], R = fit$par[3],
       spline = sp, sse = fit$value)
}
