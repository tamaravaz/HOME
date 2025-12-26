# HOME: Harmonized Orphanhood Mortality Estimation

**HOME** is an R package designed to implement, compare, and diagnose indirect mortality estimation methods based on orphanhood data.

## Key Features

* **Multi-Method Estimation:** Implements standard methods (Brass, Timaeus) and modern adaptations for low-mortality settings (Luy, 2012).
* **Diagnostic Suite:** Tools to assess internal consistency of orphanhood-based estimates by examining the stability of the estimated mortality level (alpha) across respondent age groups, and by evaluating sensitivity to assumptions on the age pattern of mortality and the mean age of childbearing.
* **Metric Flexibility:** Calculates multiple indicators simultaneously:
    * `45q15`: Probability of dying between ages 15 and 60 (Standard).
    * `30q30`: Probability of dying between ages 30 and 60 (Robust).
    * `e30`: Life expectancy at age 30 (Policy-relevant).
* **Interactive Dashboard:** A built-in Shiny application for non-coders to perform analyses and sensitivity checks via a GUI.

## 📦 Installation

You can install the development version of HOME directly from GitHub:

```r
# If you do not have devtools installed:
# install.packages("devtools")

devtools::install_github("tamara-vaz/HOME")
```

## Quick Start: Estimating Mortality

```r
# Input data: Respondent age, Proportion of mothers alive (Sn), Mean age of childbearing (Mn)
df_input <- data.frame(
  age_n = c(20, 25, 30, 35, 40, 45, 50, 55, 60),
  Sn = c(0.9916,0.9814,0.9663,0.9414,0.9009,0.7884,0.6333,0.3856,0.1248),
  Mn = rep(25.5,9)
)
# Run the estimation using the Luy (2012) method
# This creates an 'OrphanhoodEstimate' object containing metadata and inputs
est <- om_estimate_index(
  method = "luy",              
  sex_parent = "Female",
  age_respondent = df_input$age_n,
  p_surv = df_input$Sn,
  mean_age_parent = df_input$Mn,
  surv_date = 2024.75,         
  model_family = "General", std_level = 60  
)

# Plots the logit-transformed residuals (Alpha) across age groups
om_plot_linearity(est)
# Re-runs the model with perturbed M values
sens_m <- om_sensitivity(est, range_m = seq(-1.5, 1.5, 0.5))
plot(sens_m, index = "e30")

# Re-runs the model using all UN families (General, South Asian, etc.)
sens_fam <- om_sensitivity_family(est, type = "UN")
plot(sens_fam, index = "30q30")

# Generates a composite view of linearity and sensitivity
om_dashboard(est, index = "e30", family_type = "UN")
df_input <- data.table(
  age_n = seq(20, 50, by = 5),
  Sn    = c(0.97, 0.96, 0.95, 0.85, 0.78, 0.69, 0.58),
  Mn    = rep(26.2, 7) 
)
```

## Interactive Dashboard
```r
library(HOME)

# This will open the dashboard in your default web browser
app_HOME()
```


## 📄 Methodology & Diagnostics

The package operationalizes the diagnostic framework described in the accompanying working paper. It addresses the trade-off between stability (favored by bounded metrics like 30q30|45q15) and communicability (favored by e30).
Key diagnostic plots included in the package allow users to assess

  1. Internal Consistency: The linearity of logit-transformed residuals ($\alpha$) across age groups.
  1. Parameter Sensitivity: How errors in the Mean Age of Childbearing ($\bar{M}$) affect final life expectancy estimates.
  
* If you use HOME in your research, please cite the working paper: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

