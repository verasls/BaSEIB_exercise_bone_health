# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))
# Set contrasts of variable group to sum
contrasts(df$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
# Body composition
body_mass_data <- df %>% 
  dplyr::select(
    subj, time, group, body_mass, body_mass_adjust , BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
BMI_data <- df %>% 
  dplyr::select(
    subj, time, group, BMI, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
fat_mass_data <- df %>% 
  dplyr::select(
    subj, time, group, whole_body_fat_mass, whole_body_fat_mass_adjust,
    BMI_adjust, surgery, age, menopause, diabetes, thiazides, smoker
  )
lean_mass_data <- df %>% 
  dplyr::select(
    subj, time, group, whole_body_lean_mass, whole_body_lean_mass_adjust,
    BMI_adjust, surgery, age, menopause, diabetes, thiazides, smoker
  )

# Physical activity
steps_data <- df %>% 
  dplyr::select(
    subj, time, group, steps, steps_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
SB_data <- df %>% 
  dplyr::select(
    subj, time, group, SB_h, SB_h_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
LPA_data <- df %>% 
  dplyr::select(
    subj, time, group, LPA_h, LPA_h_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
MVPA_data <- df %>% 
  dplyr::select(
    subj, time, group, MVPA_min, MVPA_min_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Isokinetic strength
pt_ext_data <- df %>% 
  dplyr::select(
    subj, time, group, peak_torque_knee_ext_60ds, 
    peak_torque_knee_ext_60ds_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
pt_fle_data <- df %>% 
  dplyr::select(
    subj, time, group, peak_torque_knee_fle_60ds, 
    peak_torque_knee_fle_60ds_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
pt_ext_rel_data <- df %>% 
  dplyr::select(
    subj, time, group, peak_torque_knee_ext_60ds_body_mass, 
    peak_torque_knee_ext_60ds_body_mass_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
pt_fle_rel_data <- df %>% 
  dplyr::select(
    subj, time, group, peak_torque_knee_fle_60ds_body_mass, 
    peak_torque_knee_fle_60ds_body_mass_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    var, " ~ 1 + group + time + group:time + ", var, 
    "_adjust + BMI_adjust + surgery + age + menopause + 
    diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

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
group_body_mass_emm <- emmeans(body_mass_LMM, ~ group)

# Estimated marginal means for time
time_body_mass_emm <- emmeans(body_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_body_mass_emm  <- emmeans(body_mass_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_body_mass_emm_df <- as.data.frame(interaction_body_mass_emm)
write_csv(interaction_body_mass_emm_df, here("output", "interaction_body_mass_emm.csv"))

# Post hocs
ph_body_mass_none <- pairs(interaction_body_mass_emm, adjust = "none")

# **** BMI ----------------------------------------------------------------

BMI_LMM <- lmer(
  formula = BMI ~ 1 + group + time + group:time + BMI_adjust + surgery + age + 
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
group_BMI_emm <- emmeans(BMI_LMM, ~ group)

# Estimated marginal means for time
time_BMI_emm <- emmeans(BMI_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_BMI_emm  <- emmeans(BMI_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_BMI_emm_df <- as.data.frame(interaction_BMI_emm)
write_csv(interaction_BMI_emm_df, here("output", "interaction_BMI_emm.csv"))

# Post hocs
ph_BMI_none <- pairs(interaction_BMI_emm, adjust = "none")

# **** Fat mass -----------------------------------------------------------

fat_mass_LMM <- lmer(formula = build_formula("whole_body_fat_mass"), data = fat_mass_data)

# R-squared
rsquared(fat_mass_LMM)

# Fixed effects test
anova(fat_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(fat_mass_LMM)

# Estimated marginal means for group
group_fat_mass_emm <- emmeans(fat_mass_LMM, ~ group)

# Estimated marginal means for time
time_fat_mass_emm <- emmeans(fat_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_fat_mass_emm  <- emmeans(fat_mass_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_fat_mass_emm_df <- as.data.frame(interaction_fat_mass_emm)
write_csv(interaction_fat_mass_emm_df, here("output", "interaction_fat_mass_emm.csv"))

# Post hocs
ph_fat_mass_none <- pairs(interaction_fat_mass_emm, adjust = "none")

# **** Lean mass ----------------------------------------------------------

lean_mass_LMM <- lmer(formula = build_formula("whole_body_lean_mass"), data = lean_mass_data)

# R-squared
rsquared(lean_mass_LMM)

# Fixed effects test
anova(lean_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(lean_mass_LMM)

# Estimated marginal means for group
group_lean_mass_emm <- emmeans(lean_mass_LMM, ~ group)

# Estimated marginal means for time
time_lean_mass_emm <- emmeans(lean_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_lean_mass_emm  <- emmeans(lean_mass_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_lean_mass_emm_df <- as.data.frame(interaction_lean_mass_emm)
write_csv(interaction_lean_mass_emm_df, here("output", "interaction_lean_mass_emm.csv"))

# Post hocs
ph_lean_mass_none <- pairs(interaction_lean_mass_emm, adjust = "none")

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
group_steps_emm <- emmeans(steps_LMM, ~ group)

# Estimated marginal means for time
time_steps_emm <- emmeans(steps_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_steps_emm  <- emmeans(steps_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_steps_emm_df <- as.data.frame(interaction_steps_emm)
write_csv(interaction_steps_emm_df, here("output", "interaction_steps_emm.csv"))

# Post hocs
ph_steps_none <- pairs(interaction_steps_emm, adjust = "none")

# **** Sedentary behavior -------------------------------------------------

SB_LMM <- lmer(formula = build_formula("SB_h"), data = SB_data)

# R-squared
rsquared(SB_LMM)

# Fixed effects test
anova(SB_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(SB_LMM)

# Estimated marginal means for group
group_SB_emm <- emmeans(SB_LMM, ~ group)

# Estimated marginal means for time
time_SB_emm <- emmeans(SB_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_SB_emm  <- emmeans(SB_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_SB_emm_df <- as.data.frame(interaction_SB_emm)
write_csv(interaction_SB_emm_df, here("output", "interaction_SB_emm.csv"))

# Post hocs
ph_SB_none <- pairs(interaction_SB_emm, adjust = "none")

# **** Light physical activity --------------------------------------------

LPA_LMM <- lmer(formula = build_formula("LPA_h"), data = LPA_data)

# R-squared
rsquared(LPA_LMM)

# Fixed effects test
anova(LPA_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(LPA_LMM)

# Estimated marginal means for group
group_LPA_emm <- emmeans(LPA_LMM, ~ group)

# Estimated marginal means for time
time_LPA_emm <- emmeans(LPA_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_LPA_emm  <- emmeans(LPA_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_LPA_emm_df <- as.data.frame(interaction_LPA_emm)
write_csv(interaction_LPA_emm_df, here("output", "interaction_LPA_emm.csv"))

# Post hocs
ph_LPA_none <- pairs(interaction_LPA_emm, adjust = "none")

# **** Moderate-to-vigorous physical activity -----------------------------

MVPA_LMM <- lmer(formula = build_formula("MVPA_min"), data = MVPA_data)

# R-squared
rsquared(MVPA_LMM)

# Fixed effects test
anova(MVPA_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(MVPA_LMM)

# Estimated marginal means for group
group_MVPA_emm <- emmeans(MVPA_LMM, ~ group)

# Estimated marginal means for time
time_MVPA_emm <- emmeans(MVPA_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_MVPA_emm  <- emmeans(MVPA_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_MVPA_emm_df <- as.data.frame(interaction_MVPA_emm)
write_csv(interaction_MVPA_emm_df, here("output", "interaction_MVPA_emm.csv"))

# Post hocs
ph_MVPA_none <- pairs(interaction_MVPA_emm, adjust = "none")

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
group_pt_ext_emm <- emmeans(pt_ext_LMM, ~ group)

# Estimated marginal means for time
time_pt_ext_emm <- emmeans(pt_ext_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_ext_emm  <- emmeans(pt_ext_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_pt_ext_emm_df <- as.data.frame(interaction_pt_ext_emm)
write_csv(interaction_pt_ext_emm_df, here("output", "interaction_pt_ext_emm_df.csv"))

# Post hocs
ph_pt_ext_none <- pairs(interaction_pt_ext_emm, adjust = "none")

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
group_pt_fle_emm <- emmeans(pt_fle_LMM, ~ group)

# Estimated marginal means for time
time_pt_fle_emm <- emmeans(pt_fle_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_fle_emm  <- emmeans(pt_fle_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_pt_fle_emm_df <- as.data.frame(interaction_pt_fle_emm)
write_csv(interaction_pt_fle_emm_df, here("output", "interaction_pt_fle_emm_df.csv"))

# Post hocs
ph_pt_fle_none <- pairs(interaction_pt_fle_emm, adjust = "none")

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
group_pt_ext_rel_emm <- emmeans(pt_ext_rel_LMM, ~ group)

# Estimated marginal means for time
time_pt_ext_rel_emm <- emmeans(pt_ext_rel_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_ext_rel_emm  <- emmeans(pt_ext_rel_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_pt_ext_rel_emm_df <- as.data.frame(interaction_pt_ext_rel_emm)
write_csv(interaction_pt_ext_rel_emm_df, here("output", "interaction_pt_ext_rel_emm_df.csv"))

# Post hocs
ph_pt_ext_rel_none <- pairs(interaction_pt_ext_rel_emm, adjust = "none")

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
group_pt_fle_rel_emm <- emmeans(pt_fle_rel_LMM, ~ group)

# Estimated marginal means for time
time_pt_fle_rel_emm <- emmeans(pt_fle_rel_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_pt_fle_rel_emm  <- emmeans(pt_fle_rel_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_pt_fle_rel_emm_df <- as.data.frame(interaction_pt_fle_rel_emm)
write_csv(interaction_pt_fle_rel_emm_df, here("output", "interaction_pt_fle_rel_emm_df.csv"))

# Post hocs
ph_pt_fle_rel_none <- pairs(interaction_pt_fle_rel_emm, adjust = "none")
