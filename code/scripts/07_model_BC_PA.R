# Load packages and functions ---------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source("code/functions/adjust_variable.R")
source("code/functions/center_variable.R")
source("code/functions/bonferroni.R")

# Load and prepare data ---------------------------------------------------

source("code/scripts/01_tidy_data.R")
# Set contrasts of variable group to deviation
contrasts(df$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
# Body composition
body_mass_data <- df %>% select(subj, time, attend_cat, delta_body_mass, BMI_adjust)
BMI_data       <- df %>% select(subj, time, attend_cat, delta_BMI, BMI_adjust)
fat_mass_data  <- df %>% select(subj, time, attend_cat, delta_whole_body_fat_mass, BMI_adjust)
lean_mass_data <- df %>% select(subj, time, attend_cat, delta_whole_body_lean_mass, BMI_adjust)

# Isokinetic muscle strength
PT_ext_data           <- df %>% select(subj, time, attend_cat, delta_peak_torque_knee_ext_60ds, BMI_adjust)
PT_fle_data           <- df %>% select(subj, time, attend_cat, delta_peak_torque_knee_fle_60ds, BMI_adjust)
PT_ext_body_mass_data <- df %>% select(subj, time, attend_cat, delta_peak_torque_knee_ext_60ds_body_mass, BMI_adjust)
PT_fle_body_mass_data <- df %>% select(subj, time, attend_cat, delta_peak_torque_knee_fle_60ds_body_mass, BMI_adjust)

# Physical activity
steps_data <- df %>% select(subj, time, attend_cat, delta_steps, BMI_adjust)
SB_data    <- df %>% select(subj, time, attend_cat, delta_SB_h, BMI_adjust)
LPA_data   <- df %>% select(subj, time, attend_cat, delta_LPA_h, BMI_adjust)
MVPA_data  <- df %>% select(subj, time, attend_cat, delta_MVPA_min, BMI_adjust)


# Build models ------------------------------------------------------------

# ** Body composition variables -------------------------------------------

# **** Body mass ----------------------------------------------------------

body_mass_LMM <- lmer(
  formula = delta_body_mass ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = body_mass_data
)

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
# Save into a data frame to build the plots
interaction_body_mass_emm_df <- as.data.frame(interaction_body_mass_emm)
write_csv(interaction_body_mass_emm_df, "output/interaction_body_mass_emm.csv")

# Post hocs
ph_body_mass_none <- pairs(interaction_body_mass_emm, adjust = "none")

# **** BMI ----------------------------------------------------------------

BMI_LMM <- lmer(
  formula = delta_BMI ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
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
# Save into a data frame to build the plots
interaction_BMI_emm_df <- as.data.frame(interaction_BMI_emm)
write_csv(interaction_BMI_emm_df, "output/interaction_BMI_emm.csv")

# Post hocs
ph_BMI_none <- pairs(interaction_BMI_emm, adjust = "none")

# **** Fat mass -----------------------------------------------------------

fat_mass_LMM <- lmer(
  formula = delta_whole_body_fat_mass ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = fat_mass_data
)

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
# Save into a data frame to build the plots
interaction_fat_mass_emm_df <- as.data.frame(interaction_fat_mass_emm)
write_csv(interaction_fat_mass_emm_df, "output/interaction_fat_mass_emm.csv")

# Post hocs
ph_fat_mass_none <- pairs(interaction_fat_mass_emm, adjust = "none")

# **** Lean mass ----------------------------------------------------------

whole_body_lean_mass_LMM <- lmer(
  formula = delta_whole_body_lean_mass ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = lean_mass_data
)

# R-squared
rsquared(whole_body_lean_mass_LMM)

# Fixed effects test
anova(whole_body_lean_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(whole_body_lean_mass_LMM)

# Estimated marginal means for group
group_lean_mass_emm <- emmeans(whole_body_lean_mass_LMM, ~ attend_cat)

# Estimated marginal means for time
time_lean_mass_emm <- emmeans(whole_body_lean_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_lean_mass_emm  <- emmeans(whole_body_lean_mass_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_lean_mass_emm_df <- as.data.frame(interaction_lean_mass_emm)
write_csv(interaction_lean_mass_emm_df, "output/interaction_lean_mass_emm.csv")

# Post hocs
ph_lean_mass_none <- pairs(interaction_lean_mass_emm, adjust = "none")

# ** Isokinetic muscle strength variables ---------------------------------

# **** Peak torque knee extension -----------------------------------------

PT_ext_LMM <- lmer(
  formula = delta_peak_torque_knee_ext_60ds ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = PT_ext_data
)

# R-squared
rsquared(PT_ext_LMM)

# Fixed effects test
anova(PT_ext_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(PT_ext_LMM)

# Estimated marginal means for group
group_PT_ext_emm <- emmeans(PT_ext_LMM, ~ attend_cat)

# Estimated marginal means for time
time_PT_ext_emm <- emmeans(PT_ext_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_PT_ext_emm  <- emmeans(PT_ext_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_PT_ext_emm_df <- as.data.frame(interaction_PT_ext_emm)
write_csv(interaction_PT_ext_emm_df, "output/interaction_PT_ext_emm.csv")

# Post hocs
ph_PT_ext_none <- pairs(interaction_PT_ext_emm, adjust = "none")

# **** Peak torque knee flexion -------------------------------------------

PT_fle_LMM <- lmer(
  formula = delta_peak_torque_knee_fle_60ds ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = PT_fle_data
)

# R-squared
rsquared(PT_fle_LMM)

# Fixed effects test
anova(PT_fle_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(PT_fle_LMM)

# Estimated marginal means for group
group_PT_fle_emm <- emmeans(PT_fle_LMM, ~ attend_cat)

# Estimated marginal means for time
time_PT_fle_emm <- emmeans(PT_fle_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_PT_fle_emm  <- emmeans(PT_fle_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_PT_fle_emm_df <- as.data.frame(interaction_PT_fle_emm)
write_csv(interaction_PT_fle_emm_df, "output/interaction_PT_fle_emm.csv")

# Post hocs
ph_PT_fle_none <- pairs(interaction_PT_fle_emm, adjust = "none")

# **** Peak torque knee extension / body mass -----------------------------

PT_ext_body_mass_LMM <- lmer(
  formula = delta_peak_torque_knee_ext_60ds_body_mass ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = PT_ext_body_mass_data
)

# R-squared
rsquared(PT_ext_body_mass_LMM)

# Fixed effects test
anova(PT_ext_body_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(PT_ext_body_mass_LMM)

# Estimated marginal means for group
group_PT_ext_body_mass_emm <- emmeans(PT_ext_body_mass_LMM, ~ attend_cat)

# Estimated marginal means for time
time_PT_ext_body_mass_emm <- emmeans(PT_ext_body_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_PT_ext_body_mass_emm  <- emmeans(PT_ext_body_mass_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_PT_ext_body_mass_emm_df <- as.data.frame(interaction_PT_ext_body_mass_emm)
write_csv(interaction_PT_ext_body_mass_emm_df, "output/interaction_PT_ext_body_mass_emm.csv")

# Post hocs
ph_PT_ext_body_mass_none <- pairs(interaction_PT_ext_body_mass_emm, adjust = "none")

# **** Peak torque knee flexion / body mass -------------------------------

PT_fle_body_mass_LMM <- lmer(
  formula = delta_peak_torque_knee_fle_60ds_body_mass ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = PT_fle_body_mass_data
)

# R-squared
rsquared(PT_fle_body_mass_LMM)

# Fixed effects test
anova(PT_fle_body_mass_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(PT_fle_body_mass_LMM)

# Estimated marginal means for group
group_PT_fle_body_mass_emm <- emmeans(PT_fle_body_mass_LMM, ~ attend_cat)

# Estimated marginal means for time
time_PT_fle_body_mass_emm <- emmeans(PT_fle_body_mass_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_PT_fle_body_mass_emm  <- emmeans(PT_fle_body_mass_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_PT_fle_body_mass_emm_df <- as.data.frame(interaction_PT_fle_body_mass_emm)
write_csv(interaction_PT_fle_body_mass_emm_df, "output/interaction_PT_fle_body_mass_emm.csv")

# Post hocs
ph_PT_fle_body_mass_none <- pairs(interaction_PT_fle_body_mass_emm, adjust = "none")

# ** Physical activity variables ------------------------------------------

# **** Steps --------------------------------------------------------------

steps_LMM <- lmer(
  formula = delta_steps ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = steps_data
)

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
# Save into a data frame to build the plots
interaction_steps_emm_df <- as.data.frame(interaction_steps_emm)
write_csv(interaction_steps_emm_df, "output/interaction_steps_emm.csv")

# Post hocs
ph_steps_none <- pairs(interaction_steps_emm, adjust = "none")

# **** Sedentary behaviour ------------------------------------------------

SB_LMM <- lmer(
  formula = delta_SB_h ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = SB_data
)

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
# Save into a data frame to build the plots
interaction_SB_emm_df <- as.data.frame(interaction_SB_emm)
write_csv(interaction_SB_emm_df, "output/interaction_SB_emm.csv")

# Post hocs
ph_SB_none <- pairs(interaction_SB_emm, adjust = "none")

# **** Light physical activity --------------------------------------------

LPA_LMM <- lmer(
  formula = delta_LPA_h ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = LPA_data
)

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
# Save into a data frame to build the plots
interaction_LPA_emm_df <- as.data.frame(interaction_LPA_emm)
write_csv(interaction_LPA_emm_df, "output/interaction_LPA_emm.csv")

# Post hocs
ph_LPA_none <- pairs(interaction_LPA_emm, adjust = "none")

# **** Moderate-to-vigorous physical activity -----------------------------

MVPA_LMM <- lmer(
  formula = delta_MVPA_min ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = MVPA_data
)

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
# Save into a data frame to build the plots
interaction_MVPA_emm_df <- as.data.frame(interaction_MVPA_emm)
write_csv(interaction_MVPA_emm_df, "output/interaction_MVPA_emm.csv")

# Post hocs
ph_MVPA_none <- pairs(interaction_MVPA_emm, adjust = "none")