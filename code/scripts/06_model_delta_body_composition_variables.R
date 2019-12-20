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
body_mass_data <- df %>% select(subj, time, attend_cat, delta_body_mass, BMI_adjust)
BMI_data       <- df %>% select(subj, time, attend_cat, delta_BMI, BMI_adjust)
fat_mass_data  <- df %>% select(subj, time, attend_cat, delta_whole_body_fat_mass, BMI_adjust)
lean_mass_data <- df %>% select(subj, time, attend_cat, delta_whole_body_lean_mass, BMI_adjust)

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