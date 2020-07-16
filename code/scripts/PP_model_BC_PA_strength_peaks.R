# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))
source(here("code", "functions", "bonferroni.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))
# Set contrasts of variable group to sum
contrasts(df$attend_cat) <- matrix(rev(contr.sum(3)), ncol = 2)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
# Number of acceleration peaks
n_peaks_data <- read_csv(
  here("data", "acc.csv"),
  col_types = cols(
    time = col_factor(1:4),
    group = col_factor(c("Control", "Exercise"))
  )
) %>% 
  dplyr::select(subj, time, above_thrsh, above_thrsh_adjust) %>% 
  left_join(df, by = c("subj", "time")) %>% 
  dplyr::select(
    subj, time, attend_cat, above_thrsh, above_thrsh_adjust, 
    BMI_adjust, surgery, age, menopause, diabetes, thiazides, smoker
  )
contrasts(n_peaks_data$time) <- contr.poly(4)

# Body composition
body_mass_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, body_mass, body_mass_adjust , BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
BMI_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, BMI, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
fat_mass_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, whole_body_fat_mass, whole_body_fat_mass_adjust,
    BMI_adjust, surgery, age, menopause, diabetes, thiazides, smoker
  )
lean_mass_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, whole_body_lean_mass, whole_body_lean_mass_adjust,
    BMI_adjust, surgery, age, menopause, diabetes, thiazides, smoker
  )

# Physical activity
steps_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, steps, steps_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
SB_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, SB_h, SB_h_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
LPA_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, LPA_h, LPA_h_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
MVPA_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, MVPA_min, MVPA_min_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Isokinetic strength
pt_ext_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, peak_torque_knee_ext_60ds, 
    peak_torque_knee_ext_60ds_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
pt_fle_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, peak_torque_knee_fle_60ds, 
    peak_torque_knee_fle_60ds_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
pt_ext_rel_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, peak_torque_knee_ext_60ds_body_mass, 
    peak_torque_knee_ext_60ds_body_mass_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
pt_fle_rel_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, peak_torque_knee_fle_60ds_body_mass, 
    peak_torque_knee_fle_60ds_body_mass_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    var, " ~ 1 + attend_cat + time + attend_cat:time + ", var, 
    "_adjust + BMI_adjust + surgery + age + menopause + 
    diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

# ** Number of acceleration peaks -----------------------------------------

n_peaks_LMM <- lmer(formula = build_formula("above_thrsh"), data = n_peaks_data)

# R-squared
rsquared(n_peaks_LMM)

# Fixed effects test
anova(n_peaks_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(n_peaks_LMM)

# Estimated marginal means for group
group_n_peaks_emm <- emmeans(n_peaks_LMM, ~ attend_cat)

# Estimated marginal means for time
time_n_peaks_emm <- emmeans(n_peaks_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_n_peaks_emm  <- emmeans(n_peaks_LMM, ~ attend_cat:time)

# Post hocs
ph_n_peaks_none <- pairs(interaction_n_peaks_emm, adjust = "none")
ph_n_peaks_bonf <- bonferroni(ph_n_peaks_none, 3)

# ** Body composition variables -------------------------------------------

# **** Body mass ----------------------------------------------------------

body_mass_LMM <- lmer(formula = build_formula("body_mass"), data = body_mass_data)

# R-squared
rsquared(body_mass_LMM)

# Fixed effects test
anova(body_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(body_mass_LMM)

# Estimated marginal means for group
group_body_mass_emm <- emmeans(body_mass_LMM, ~ attend_cat)

# Estimated marginal means for time
time_body_mass_emm <- emmeans(body_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_body_mass_emm  <- emmeans(body_mass_LMM, ~ attend_cat:time)

# Post hocs
ph_body_mass_none <- pairs(interaction_body_mass_emm, adjust = "none")
ph_body_mass_bonf <- bonferroni(ph_body_mass_none, 3)

# **** BMI ----------------------------------------------------------------

BMI_LMM <- lmer(
  formula = BMI ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + surgery + age + 
    menopause + diabetes + thiazides + smoker + (1 | subj),
  data = BMI_data
)

# R-squared
rsquared(BMI_LMM)

# Fixed effects test
anova(BMI_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(BMI_LMM)

# Estimated marginal means for group
group_BMI_emm <- emmeans(BMI_LMM, ~ attend_cat)

# Estimated marginal means for time
time_BMI_emm <- emmeans(BMI_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_BMI_emm  <- emmeans(BMI_LMM, ~ attend_cat:time)

# Post hocs
ph_BMI_none <- pairs(interaction_BMI_emm, adjust = "none")
ph_BMI_bonf <- bonferroni(ph_BMI_none, 3)

# **** Fat mass -----------------------------------------------------------

fat_mass_LMM <- lmer(formula = build_formula("whole_body_fat_mass"), data = fat_mass_data)

# R-squared
rsquared(fat_mass_LMM)

# Fixed effects test
anova(fat_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(fat_mass_LMM)

# Estimated marginal means for group
group_fat_mass_emm <- emmeans(fat_mass_LMM, ~ attend_cat)

# Estimated marginal means for time
time_fat_mass_emm <- emmeans(fat_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_fat_mass_emm  <- emmeans(fat_mass_LMM, ~ attend_cat:time)

# Post hocs
ph_fat_mass_none <- pairs(interaction_fat_mass_emm, adjust = "none")
ph_fat_mass_bonf <- bonferroni(ph_fat_mass_none, 3)

# **** Lean mass ----------------------------------------------------------

lean_mass_LMM <- lmer(formula = build_formula("whole_body_lean_mass"), data = lean_mass_data)

# R-squared
rsquared(lean_mass_LMM)

# Fixed effects test
anova(lean_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(lean_mass_LMM)

# Estimated marginal means for group
group_lean_mass_emm <- emmeans(lean_mass_LMM, ~ attend_cat)

# Estimated marginal means for time
time_lean_mass_emm <- emmeans(lean_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_lean_mass_emm  <- emmeans(lean_mass_LMM, ~ attend_cat:time)

# Post hocs
ph_lean_mass_none <- pairs(interaction_lean_mass_emm, adjust = "none")
ph_lean_mass_bonf <- bonferroni(ph_lean_mass_none, 3)

# ** Physical activity variables ------------------------------------------

# **** Steps --------------------------------------------------------------

steps_LMM <- lmer(formula = build_formula("steps"), data = steps_data)

# R-squared
rsquared(steps_LMM)

# Fixed effects test
anova(steps_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(steps_LMM)

# Estimated marginal means for group
group_steps_emm <- emmeans(steps_LMM, ~ attend_cat)

# Estimated marginal means for time
time_steps_emm <- emmeans(steps_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_steps_emm  <- emmeans(steps_LMM, ~ attend_cat:time)

# Post hocs
ph_steps_none <- pairs(interaction_steps_emm, adjust = "none")
ph_steps_bonf <- bonferroni(ph_steps_none, 3)

# **** Sedentary behavior -------------------------------------------------

SB_LMM <- lmer(formula = build_formula("SB_h"), data = SB_data)

# R-squared
rsquared(SB_LMM)

# Fixed effects test
anova(SB_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(SB_LMM)

# Estimated marginal means for group
group_SB_emm <- emmeans(SB_LMM, ~ attend_cat)

# Estimated marginal means for time
time_SB_emm <- emmeans(SB_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_SB_emm  <- emmeans(SB_LMM, ~ attend_cat:time)

# Post hocs
ph_SB_none <- pairs(interaction_SB_emm, adjust = "none")
ph_SB_bonf <- bonferroni(ph_SB_none, 3)

# **** Light physical activity --------------------------------------------

LPA_LMM <- lmer(formula = build_formula("LPA_h"), data = LPA_data)

# R-squared
rsquared(LPA_LMM)

# Fixed effects test
anova(LPA_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(LPA_LMM)

# Estimated marginal means for group
group_LPA_emm <- emmeans(LPA_LMM, ~ attend_cat)

# Estimated marginal means for time
time_LPA_emm <- emmeans(LPA_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_LPA_emm  <- emmeans(LPA_LMM, ~ attend_cat:time)

# Post hocs
ph_LPA_none <- pairs(interaction_LPA_emm, adjust = "none")
ph_LPA_bonf <- bonferroni(ph_LPA_none, 3)

# **** Moderate-to-vigorous physical activity -----------------------------

MVPA_LMM <- lmer(formula = build_formula("MVPA_min"), data = MVPA_data)

# R-squared
rsquared(MVPA_LMM)

# Fixed effects test
anova(MVPA_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(MVPA_LMM)

# Estimated marginal means for group
group_MVPA_emm <- emmeans(MVPA_LMM, ~ attend_cat)

# Estimated marginal means for time
time_MVPA_emm <- emmeans(MVPA_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_MVPA_emm  <- emmeans(MVPA_LMM, ~ attend_cat:time)

# Post hocs
ph_MVPA_none <- pairs(interaction_MVPA_emm, adjust = "none")
ph_MVPA_bonf <- bonferroni(ph_MVPA_none, 3)

# ** Isokinetic strength variables ----------------------------------------

# **** Peak torque - extension --------------------------------------------

pt_ext_LMM <- lmer(
  formula = build_formula("peak_torque_knee_ext_60ds"), 
  data = pt_ext_data
)

# R-squared
rsquared(pt_ext_LMM)

# Fixed effects test
anova(pt_ext_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(pt_ext_LMM)

# Estimated marginal means for group
group_pt_ext_emm <- emmeans(pt_ext_LMM, ~ attend_cat)

# Estimated marginal means for time
time_pt_ext_emm <- emmeans(pt_ext_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_ext_emm  <- emmeans(pt_ext_LMM, ~ attend_cat:time)

# Post hocs
ph_pt_ext_none <- pairs(interaction_pt_ext_emm, adjust = "none")
ph_pt_ext_bonf <- bonferroni(ph_pt_ext_none, 3)

# **** Peak torque - flexion ----------------------------------------------

pt_fle_LMM <- lmer(
  formula = build_formula("peak_torque_knee_fle_60ds"), 
  data = pt_fle_data
)

# R-squared
rsquared(pt_fle_LMM)

# Fixed effects test
anova(pt_fle_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(pt_fle_LMM)

# Estimated marginal means for group
group_pt_fle_emm <- emmeans(pt_fle_LMM, ~ attend_cat)

# Estimated marginal means for time
time_pt_fle_emm <- emmeans(pt_fle_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_fle_emm  <- emmeans(pt_fle_LMM, ~ attend_cat:time)

# Post hocs
ph_pt_fle_none <- pairs(interaction_pt_fle_emm, adjust = "none")
ph_pt_fle_bonf <- bonferroni(ph_pt_fle_none, 3)

# **** Relative peak torque - extension -----------------------------------

pt_ext_rel_LMM <- lmer(
  formula = build_formula("peak_torque_knee_ext_60ds_body_mass"), 
  data = pt_ext_rel_data
)

# R-squared
rsquared(pt_ext_rel_LMM)

# Fixed effects test
anova(pt_ext_rel_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(pt_ext_rel_LMM)

# Estimated marginal means for group
group_pt_ext_rel_emm <- emmeans(pt_ext_rel_LMM, ~ attend_cat)

# Estimated marginal means for time
time_pt_ext_rel_emm <- emmeans(pt_ext_rel_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_ext_rel_emm  <- emmeans(pt_ext_rel_LMM, ~ attend_cat:time)

# Post hocs
ph_pt_ext_rel_none <- pairs(interaction_pt_ext_rel_emm, adjust = "none")
ph_pt_ext_rel_bonf <- bonferroni(ph_pt_ext_rel_none, 3)

# **** Relative peak torque - flexion -------------------------------------

pt_fle_rel_LMM <- lmer(
  formula = build_formula("peak_torque_knee_fle_60ds_body_mass"), 
  data = pt_fle_rel_data
)

# R-squared
rsquared(pt_fle_rel_LMM)

# Fixed effects test
anova(pt_fle_rel_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(pt_fle_rel_LMM)

# Estimated marginal means for group
group_pt_fle_rel_emm <- emmeans(pt_fle_rel_LMM, ~ attend_cat)

# Estimated marginal means for time
time_pt_fle_rel_emm <- emmeans(pt_fle_rel_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_fle_rel_emm  <- emmeans(pt_fle_rel_LMM, ~ attend_cat:time)

# Post hocs
ph_pt_fle_rel_none <- pairs(interaction_pt_fle_rel_emm, adjust = "none")
ph_pt_fle_rel_bonf <- bonferroni(ph_pt_fle_rel_none, 3)
