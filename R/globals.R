utils::globalVariables(
  c(
    "coef_brass_hill",
    "coef_luy",
    "coef_timaeus",
    "coef_z_brass",
    "mlt_un_data"
  )
)

# Tolerance (in years) for the reference-date monotonicity check in
# om_estimate_index(). Reference dates should decrease as respondent age
# increases; an increase larger than this tolerance flags the estimate as
# inconsistent and sets its reference date to NA.
.MONOTONICITY_TOLERANCE <- 0.5
