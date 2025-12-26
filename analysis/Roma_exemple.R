# ==============================================================================
# WORKING PAPER
#
# HOME: An R Package for Indirect Adult Mortality
#  Estimation from Orphanhood Data
#
# Application: Female Adult Mortality, Romania (FRA Survey 2024)
#
# This script reproduces all empirical results and figures presented in the
# working paper. Adult mortality is estimated using three indirect orphanhood
# methods (Brass, Timæus, and Luy), harmonized through a relational logit model
# and implemented in the R package HOME.
#
# ------------------------------------------------------------------------------
# AUTHORS:
# Tamara Vaz
# Tianyu Shen
#
# ------------------------------------------------------------------------------
# SOFTWARE AND REPRODUCIBILITY
#
# All mortality estimates are produced using the open-source R package HOME
# (Household Orphanhood Mortality Estimation).
#
# The package source code, documentation, and replication materials are
# publicly available at:
#
#   https://github.com/tamarava/HOME
#
# This script corresponds to the analysis/ directory of the repository and is
# not part of the CRAN package build. It is provided solely for transparency and
# full reproducibility of the working paper results.
#
# ------------------------------------------------------------------------------
# CITATION
#
# ==============================================================================


# ==============================================================================
# 1. SETUP
# ==============================================================================

library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)

library(HOME)

# ==============================================================================
# 2. PUBLICATION THEME
# ==============================================================================

theme_TVMS <- function(base_size = 11) {
  theme_bw(base_size = base_size, base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1, hjust = 0),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.justification = "center",
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.tag = element_text(
        family = "serif",
        face = "bold",
        size = 12)
    )
}

set_theme(theme_TVMS())

save_figure <- function(p, file, w, h) {
  ggsave(paste0(file, ".svg"), p, width = w, height = h)
}

# ==============================================================================
# 3. DATA LOADING AND PREPARATION
# ==============================================================================

dt <- fread(
  "C:/Users/tvaz/OneDrive - Österreichische Akademie der Wissenschaften/RG Health & Longevity - Life Expectancy of Roma in Europe - Life Expectancy of Roma in Europe/le_roma_vid/data/processed_data/FRA_2019_2021_2024_Adult_mortality_unweighted.csv"
)

dt_ro <- dt[country == "RO" &
              FRA_survey == "2024" &
              sex == "Both" &
              age != "Total", .(
                age_n = as.numeric(substr(age, 1, 2)),
                Sn = mom_alive / (mom_alive + mom_dead),
                Mn = avg_age_mom_alive - avg_age_resp_mom_age
              )][age_n <= 60]

survey_date <- 2024.74

# ==============================================================================
# 4. ESTIMATION WRAPPER
# ==============================================================================

estimate_method <- function(method, ages = dt_ro$age_n, Mn = dt_ro$Mn, Sn= dt_ro$Sn) {
  dt <- dt_ro[age_n %in% ages]
  om_estimate_index(
    method = method,
    sex_parent = "Female",
    age_respondent = ages,
    p_surv = dt$Sn,
    mean_age_parent = dt$Mn,
    surv_date = survey_date,
    model_family = "General"
  )$estimates %>%  as.data.table()
}

# ==============================================================================
# 5. BASE ESTIMATES
# ==============================================================================

df_base <- rbindlist(list(
  estimate_method("luy"),
  estimate_method("brass"),
  estimate_method("timaeus")
))

# ==============================================================================
# 6. SENSITIVITY: MODEL LIFE TABLE FAMILY
# ==============================================================================

families <- c("General", "Latin", "Chilean", "South_Asian", "Far_East_Asian")
methods  <- c("luy", "brass", "timaeus")

df_sens <- rbindlist(lapply(methods, function(m) {
  rbindlist(lapply(families, function(f) {
    om_estimate_index(
      method = m,
      sex_parent = "Female",
      age_respondent = dt_ro$age_n,
      p_surv = dt_ro$Sn,
      mean_age_parent = dt_ro$Mn,
      surv_date = survey_date,
      model_family = f
    )$estimates  %>%
      setDT() %>%
      .[, Family := f]
  }))
}))

df_sens_long <- melt(
  df_sens,
  id.vars = c("Method", "RefTime", "Family"),
  measure.vars = c("e30", "30q30")
)


# ------------------------------------------------------------------------------
# FIGURE 1 – Adult female mortality (30q30)
# ------------------------------------------------------------------------------

p1 <- ggplot(df_base,
             aes(
               x = RefTime,
               y = `30q30`,
               linetype = Method,
               shape = Method,
               group = Method )) +
  geom_line(color = "black", linewidth = 0.3) +
  geom_point(color = "black", size = 2) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_x_continuous(breaks = seq(2000, 2024, 4),
                     limits = c(2000, 2016)) +
  labs(x = NULL, y = "Probability of dying between ages 30 and 60", ) +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# FIGURE 2 – Life expectancy at age 30 (e30)
# ------------------------------------------------------------------------------

p2 <- ggplot(df_base,
             aes(
               x = RefTime,
               y = e30,
               linetype = Method,
               shape = Method,
               group = Method
             )) +
  geom_line(color = "black", linewidth = 0.3) +
  geom_point(color = "black", size = 2) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_y_continuous(breaks = seq(30, 55, 2)) +
  scale_x_continuous(breaks = seq(2000, 2024, 4),
                     limits = c(2000, 2016)) +
  labs(x = NULL, y = "Life expectancy at age 30") +
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# FIGURE 3 – Sensitivity of e30 to model life table family
# ------------------------------------------------------------------------------

p3 <- ggplot(df_sens_long[RefTime >= 2000 & variable == "e30"],
             aes(
               x = RefTime,
               y = value,
               linetype = Family,
               group = Family
             )) +
  geom_line(color = "black", linewidth = 0.3) +
  facet_wrap( ~ Method, nrow = 3) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_continuous(breaks = seq(2000, 2024, 4),
                     limits = c(2000, 2016)) +
  scale_y_continuous(limits = c(35, 50)) +
  labs(x = NULL, y = "Life expectancy at age 30", linetype = "Model family") +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# FIGURE 4 – Stability of 30q30 across model families
# ------------------------------------------------------------------------------

p4 <- ggplot(df_sens_long[RefTime >= 2000 & variable == "30q30"],
             aes(
               x = RefTime,
               y = value,
               linetype = Family,
               group = Family
             )) +
  geom_line(color = "black", linewidth = 0.3) +
  facet_wrap( ~ Method, nrow = 3) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_continuous(breaks = seq(2000, 2024, 4),
                     limits = c(2000, 2016)) +
  scale_y_continuous(limits = c(0.10, 0.45)) +
  labs(x = NULL, y = "Probability of dying between ages 30 and 60", linetype = NULL) +
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# FIGURE 5 – Sensitivity of e30 to M
# ------------------------------------------------------------------------------

offsets <- seq(-1.5, 1.5, by = 0.1)
methods <- c("brass", "timaeus", "luy")

df_sens_M_all <- rbindlist(lapply(methods, function(m) {
  rbindlist(lapply(offsets, function(off) {
    dt_tmp <- copy(dt_ro)
    dt_tmp[, Mn_adj := Mn + off]

    dt_use <- dt_tmp
    dt_est <- as.data.table(
      om_estimate_index(
        method = m,
        sex_parent = "Female",
        age_respondent = dt_use$age_n,
        p_surv = dt_use$Sn,
        mean_age_parent = dt_use$Mn_adj,
        surv_date = survey_date,
        model_family = "General"
      )$estimates
    )
    dt_est[, `:=`(Offset_M = off, Method   = m)]

    dt_est
  }))
}))

# Relabel methods for publication
method_labels <- c(
  brass   = "Brass (1973)",
  timaeus = "Timaeus (1991)",
  luy     = "Luy (2012)"
)

df_sens_M_all[, Method := method_labels[Method]]

p5 <- ggplot(df_sens_M_all[RefTime >= 2000],
             aes(
               x = RefTime,
               y = e30,
               group = interaction(Offset_M, Method),
               color = Offset_M
             )) +
  geom_line(linewidth = 0.2) +
  facet_wrap(~ Method, nrow = 3) +
  scale_color_gradient(low = "gray90", high = "black", ) +
  scale_x_continuous(breaks = seq(2000, 2024, 4),
                     limits = c(2000, 2016)) +
  labs(color = NULL,
       y = "Life expectancy at age 30",
       y = NULL,
       x = "") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))


# ------------------------------------------------------------------------------
# FIGURE 6 – Sensitivity of 30q30 to M
# ------------------------------------------------------------------------------

p6 <- ggplot(df_sens_M_all[RefTime >= 2000],
             aes(
               x = RefTime,
               y = `30q30`,
               group = interaction(Offset_M, Method),
               color = Offset_M
             )) +
  geom_line(linewidth = 0.2) +
  facet_wrap(~ Method, nrow = 3) +
  scale_color_gradient(low = "gray90", high = "black", ) +
  scale_x_continuous(breaks = seq(2000, 2024, 4),
                     limits = c(2000, 2016)) +
  labs(y = "Probability of dying between ages 30 and 60",
       color = NULL,
       y = NULL,
       x = "") +
  theme(
    legend.key.width = unit(1.5, "cm")
  )

# ------------------------------------------------------------------------------
# COMBINED PANELS
# ------------------------------------------------------------------------------

fig1 <- (p2 | p1) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A")

fig2 <- (p3 | p4) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A")

fig3 <- (p5 | p6) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A")

# ------------------------------------------------------------------------------
# SAVE FIGURES
# ------------------------------------------------------------------------------

save_figure(fig1, "analysis/Figure1_PanelsAB", 7.2, 4)
save_figure(fig2, "analysis/Figure2_PanelsAB", 7.0, 5.5)
save_figure(fig3, "analysis/Figure3_PanelsAB", 7.0, 5.5)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
