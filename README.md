# HOME: Harmonized Orphanhood Mortality Estimation

<!-- badges: start -->
<!-- badges: end -->

**HOME** is an R package for indirect adult mortality estimation from orphanhood data. It provides a unified and reproducible framework for implementing classical and modern orphanhood methods, harmonizing estimates into common mortality indices, and evaluating the sensitivity of results to key structural assumptions.

The package emphasizes **diagnostic transparency** by exposing methodological assumptions directly to the analyst and providing tools for internal consistency checks and sensitivity analysis.

HOME implements three major orphanhood estimation approaches:

- **Brass & Hill (1973)** — classical weighted survivorship estimator.
- **Timaeus (1992)** — regression-based adaptation designed to reduce sensitivity to mortality schedules.
- **Luy (2012)** — calibrated for low- and moderate-mortality settings using empirically derived weighting schemes.

---

## Features

### Unified Estimation Framework

HOME harmonizes orphanhood-derived survivorship ratios into a common relational logit framework using a one-parameter Brass relational model. This allows direct comparison across methods and mortality schedules.

### Multiple Mortality Indicators

The package simultaneously estimates:

- `30q30` — probability of dying between ages 30 and 60.
- `45q15` — probability of dying between ages 15 and 60.
- `e30` — life expectancy at age 30.

### Diagnostic Sensitivity Analysis

HOME includes built-in diagnostics for evaluating:

- Internal consistency of orphanhood estimates across respondent age groups.
- Sensitivity to assumptions on:
  - mean age at childbearing,
  - model life table family,
  - mortality schedules,
  - estimation method.

### Interactive Shiny Dashboard

An interactive GUI allows users to:

- upload survey data,
- run all estimation methods,
- compare mortality schedules,
- inspect diagnostic plots,
- export results tables.

---

# Installation

Install the development version directly from GitHub:

```{r eval=FALSE}
# install.packages("remotes")

remotes::install_github("tamaravaz/HOME")

library(HOME)
```

---

# Core Workflow

The main estimation function is:

```{r eval=FALSE}
om_estimate_index()
```

It returns an object of class:

```{r eval=FALSE}
OrphanhoodEstimate
```

containing:

- estimated survivorship ratios,
- reference dates,
- Brass relational logit levels (`Alpha`),
- mortality indices,
- metadata,
- original inputs for reproducibility.

---

# Quick Start

## Input Data

```{r}
library(HOME)

df_input <- data.frame(
  age_n = seq(15, 60, 5),
  Sn = c(
    0.95772787, 0.94418605, 0.89402174,
    0.84395199, 0.77974435, 0.67717391,
    0.49225268, 0.33670034, 0.20071685,
    0.09517426
  ),
  Mn = c(
    24.52, 24.20, 23.70, 23.48, 23.26,
    23.44, 22.00, 23.37, 21.07, 19.93
  )
)

survey_date <- 2024.753
```

Where:

- `age_n` = lower bound of respondent age group,
- `Sn` = proportion with surviving parent,
- `Mn` = mean age at childbearing.

---

## Estimation

### Luy (2012)

```{r eval=FALSE}
est_luy <- om_estimate_index(
  method = "luy",
  sex_parent = "Female",
  age_respondent = df_input$age_n,
  p_surv = df_input$Sn,
  mean_age_parent = df_input$Mn,
  surv_date = survey_date,
  model_family = "General"
)
```

### Brass & Hill (1973)

```{r eval=FALSE}
est_brass <- om_estimate_index(
  method = "brass",
  sex_parent = "Female",
  age_respondent = df_input$age_n,
  p_surv = df_input$Sn,
  mean_age_parent = df_input$Mn,
  surv_date = survey_date,
  model_family = "General"
)
```

### Timaeus (1992)

```{r eval=FALSE}
est_timaeus <- om_estimate_index(
  method = "timaeus",
  sex_parent = "Female",
  age_respondent = df_input$age_n,
  p_surv = df_input$Sn,
  mean_age_parent = df_input$Mn,
  surv_date = survey_date,
  model_family = "General"
)
```

---

# Summary and Diagnostics

## Summary

```{r eval=FALSE}
summary(est_luy)
```

Example output:

```text
Summary for Female parent mortality (index: 30q30):

Range: 0.1164 – 0.2168
Mean: 0.1614
Median: 0.1463
```

---

# Internal Consistency Diagnostics

The sequence of Brass relational logit parameters (`Alpha`) provides an internal consistency diagnostic across respondent age groups. Under smooth secular mortality decline, estimates should vary gradually across age groups.

```{r eval=FALSE}
om_plot_linearity(est_luy)
```

Returns a `ggplot2` object.

---

# Sensitivity Analysis

## Sensitivity to Mean Age at Childbearing

```{r eval=FALSE}
sens_m <- om_sensitivity(
  est_luy,
  range_m = seq(-2, 2, by = 0.5)
)

plot(sens_m, index = "30q30")
```

This evaluates how uncertainty in fertility timing propagates into mortality estimates.

---

## Sensitivity to Model Life Table Family

```{r eval=FALSE}
sens_fam <- om_sensitivity_family(
  est_luy,
  type = "All"
)

plot(sens_fam, index = "e30")
```

Available family groups:

- `"UN"`
- `"CD"` (Coale–Demeny)
- `"All"`

---

# Combined Diagnostic Dashboard

```{r eval=FALSE}
om_dashboard(
  est_luy,
  index = "30q30",
  family_type = "UN"
)
```

This combines:

- linearity diagnostics,
- family sensitivity analysis,
- fertility timing sensitivity analysis,

into a single visualization panel.

---

# Interactive Application

Launch the Shiny application:

```{r eval=FALSE}
app_HOME()
```

The dashboard includes:

- interactive trend plots,
- downloadable tables,
- diagnostic tabs,
- method comparison panels,
- support for `.csv` and `.xlsx` uploads.

Expected input columns:

| Column | Description |
|---|---|
| `n` | Lower bound of respondent age group |
| `sn` | Proportion with parent alive |
| `mn` | Mean age at childbearing |

---

# Conceptual Framework

HOME treats orphanhood estimation as a **sensitivity-based inferential workflow** rather than a single deterministic estimator.

The package is designed to support:

- reproducible demographic workflows,
- transparent reporting of structural uncertainty,
- comparison across mortality schedules,
- evaluation of methodological robustness.

The package is particularly useful for populations lacking reliable mortality registration systems, including:

- marginalized populations,
- ethnic minorities,
- migrant populations,
- conflict-affected populations,
- populations absent from routine administrative statistics.

---

# Citation

If you use HOME in research, please cite:

```bibtex
@article{vaz2025home,
  title   = {HOME: An R Package for Orphanhood-Based Adult Mortality Estimation with Diagnostic Sensitivity Analysis},
  author  = {Vaz, Tamara and Shen, Tianyu},
  journal = {NA},
  year    = {2026},
  note    = {Forthcoming}
}
```

---

# References

- Brass W, Hill K (1973). *Estimating Adult Mortality from Orphanhood*.
- Timaeus IM (1992). *Estimation of Adult Mortality from Paternal Orphanhood*.
- Luy M (2012). *Estimating Mortality Differences in Developed Countries from Survey Information on Maternal and Paternal Orphanhood*.
