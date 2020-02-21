# Load packages and functions ---------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source("code/functions/center_variable.R")
source("code/functions/bonferroni.R")
source("code/functions/classify_attendance.R")

# Load and prepare data ---------------------------------------------------

source("code/scripts/01_tidy_data.R")
# Set contrasts of variable group to deviation
contrasts(df$attend_cat) <- matrix(rev(contr.sum(3)), ncol = 2)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
TH_delta_data <- df %>% 
  dplyr::select(subj, time, group, attend_2nd_3rd, attend_2nd_4th, delta_TH_BMD, BMI_adjust)
FN_delta_data <- df %>% 
  dplyr::select(subj, time, group, attend_2nd_3rd, attend_2nd_4th, delta_FN_BMD, BMI_adjust)
LS_delta_data <- df %>% 
  dplyr::select(subj, time, group, attend_2nd_3rd, attend_2nd_4th, delta_LS_BMD, BMI_adjust)
TR_delta_data <- df %>% 
  dplyr::select(subj, time, group, attend_2nd_3rd, attend_2nd_4th, delta_TR_BMD, BMI_adjust)

# Build models 25% attendance cutoff --------------------------------------

# Classify attendance
TH_delta_data_25 <- classify_attendance(TH_delta_data, 25)
FN_delta_data_25 <- classify_attendance(FN_delta_data, 25)
LS_delta_data_25 <- classify_attendance(LS_delta_data, 25)
TR_delta_data_25 <- classify_attendance(TR_delta_data, 25)

# ** delta_TH_BMD ---------------------------------------------------------

delta_TH_LMM_25 <- lmer(
  formula = delta_TH_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = TH_delta_data_25
)

# R-squared
rsquared(delta_TH_LMM_25)

# Fixed effects test
anova(delta_TH_LMM_25, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TH_LMM_25)

# Estimated marginal means for group
group_delta_TH_emm_25 <- emmeans(delta_TH_LMM_25, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm_25 <- emmeans(delta_TH_LMM_25, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TH_emm_25  <- emmeans(delta_TH_LMM_25, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_TH_emm_25_df <- as.data.frame(interaction_delta_TH_emm_25)
write_csv(interaction_delta_TH_emm_25_df, "output/interaction_delta_TH_emm_25.csv")

# Post hocs
ph_delta_TH_25_none <- pairs(interaction_delta_TH_emm_25, adjust = "none")
ph_delta_TH_25_bonf <- bonferroni(as.data.frame(ph_delta_TH_25_none), 3)

# ** delta_FN_BMD ---------------------------------------------------------

delta_FN_LMM_25 <- lmer(
  formula = delta_FN_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = FN_delta_data_25
)

# R-squared
rsquared(delta_FN_LMM_25)

# Fixed effects test
anova(delta_FN_LMM_25, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_FN_LMM_25)

# Estimated marginal means for group
group_delta_FN_emm_25 <- emmeans(delta_FN_LMM_25, ~ attend_cat)

# Estimated marginal means for time
time_delta_FN_emm_25 <- emmeans(delta_FN_LMM_25, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_FN_emm_25  <- emmeans(delta_FN_LMM_25, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_FN_emm_25_df <- as.data.frame(interaction_delta_FN_emm_25)
write_csv(interaction_delta_FN_emm_25_df, "output/interaction_delta_FN_emm_25.csv")

# Post hocs
ph_delta_FN_25_none <- pairs(interaction_delta_FN_emm_25, adjust = "none")
ph_delta_FN_25_bonf <- bonferroni(as.data.frame(ph_delta_FN_25_none), 3)

# ** delta_LS_BMD ---------------------------------------------------------

delta_LS_LMM_25 <- lmer(
  formula = delta_LS_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = LS_delta_data_25
)

# R-squared
rsquared(delta_LS_LMM_25)

# Fixed effects test
anova(delta_LS_LMM_25, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_LS_LMM_25)

# Estimated marginal means for group
group_delta_LS_emm_25 <- emmeans(delta_LS_LMM_25, ~ attend_cat)

# Estimated marginal means for time
time_delta_LS_emm_25 <- emmeans(delta_LS_LMM_25, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_LS_emm_25  <- emmeans(delta_LS_LMM_25, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_LS_emm_25_df <- as.data.frame(interaction_delta_LS_emm_25)
write_csv(interaction_delta_LS_emm_25_df, "output/interaction_delta_LS_emm_25.csv")

# Post hocs
ph_delta_LS_25_none <- pairs(interaction_delta_LS_emm_25, adjust = "none")
ph_delta_LS_25_bonf <- bonferroni(as.data.frame(ph_delta_LS_25_none), 3)

# ** delta_TR_BMD ---------------------------------------------------------

delta_TR_LMM_25 <- lmer(
  formula = delta_TR_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = TR_delta_data_25
)

# R-squared
rsquared(delta_TR_LMM_25)

# Fixed effects test
anova(delta_TR_LMM_25, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TR_LMM_25)

# Estimated marginal means for group
group_delta_TR_emm_25 <- emmeans(delta_TR_LMM_25, ~ attend_cat)

# Estimated marginal means for time
time_delta_TR_emm_25 <- emmeans(delta_TR_LMM_25, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TR_emm_25  <- emmeans(delta_TR_LMM_25, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_TR_emm_25_df <- as.data.frame(interaction_delta_TR_emm_25)
write_csv(interaction_delta_TR_emm_25_df, "output/interaction_delta_TR_emm_25.csv")

# Post hocs
ph_delta_TR_25_none <- pairs(interaction_delta_TR_emm_25, adjust = "none")
ph_delta_TR_25_bonf <- bonferroni(as.data.frame(ph_delta_TR_25_none), 3)

# Build models 75% attendance cutoff --------------------------------------

# Classify attendance
TH_delta_data_75 <- classify_attendance(TH_delta_data, 75)
FN_delta_data_75 <- classify_attendance(FN_delta_data, 75)
LS_delta_data_75 <- classify_attendance(LS_delta_data, 75)
TR_delta_data_75 <- classify_attendance(TR_delta_data, 75)

# ** delta_TH_BMD ---------------------------------------------------------

delta_TH_LMM_75 <- lmer(
  formula = delta_TH_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = TH_delta_data_75
)

# R-squared
rsquared(delta_TH_LMM_75)

# Fixed effects test
anova(delta_TH_LMM_75, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TH_LMM_75)

# Estimated marginal means for group
group_delta_TH_emm_75 <- emmeans(delta_TH_LMM_75, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm_75 <- emmeans(delta_TH_LMM_75, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TH_emm_75  <- emmeans(delta_TH_LMM_75, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_TH_emm_75_df <- as.data.frame(interaction_delta_TH_emm_75)
write_csv(interaction_delta_TH_emm_75_df, "output/interaction_delta_TH_emm_75.csv")

# Post hocs
ph_delta_TH_75_none <- pairs(interaction_delta_TH_emm_75, adjust = "none")
ph_delta_TH_75_bonf <- bonferroni(as.data.frame(ph_delta_TH_75_none), 3)

# ** delta_FN_BMD ---------------------------------------------------------

delta_FN_LMM_75 <- lmer(
  formula = delta_FN_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = FN_delta_data_75
)

# R-squared
rsquared(delta_FN_LMM_75)

# Fixed effects test
anova(delta_FN_LMM_75, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_FN_LMM_75)

# Estimated marginal means for group
group_delta_FN_emm_75 <- emmeans(delta_FN_LMM_75, ~ attend_cat)

# Estimated marginal means for time
time_delta_FN_emm_75 <- emmeans(delta_FN_LMM_75, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_FN_emm_75  <- emmeans(delta_FN_LMM_75, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_FN_emm_75_df <- as.data.frame(interaction_delta_FN_emm_75)
write_csv(interaction_delta_FN_emm_75_df, "output/interaction_delta_FN_emm_75.csv")

# Post hocs
ph_delta_FN_75_none <- pairs(interaction_delta_FN_emm_75, adjust = "none")
ph_delta_FN_75_bonf <- bonferroni(as.data.frame(ph_delta_FN_75_none), 3)

# ** delta_LS_BMD ---------------------------------------------------------

delta_LS_LMM_75 <- lmer(
  formula = delta_LS_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = LS_delta_data_75
)

# R-squared
rsquared(delta_LS_LMM_75)

# Fixed effects test
anova(delta_LS_LMM_75, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_LS_LMM_75)

# Estimated marginal means for group
group_delta_LS_emm_75 <- emmeans(delta_LS_LMM_75, ~ attend_cat)

# Estimated marginal means for time
time_delta_LS_emm_75 <- emmeans(delta_LS_LMM_75, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_LS_emm_75  <- emmeans(delta_LS_LMM_75, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_LS_emm_75_df <- as.data.frame(interaction_delta_LS_emm_75)
write_csv(interaction_delta_LS_emm_75_df, "output/interaction_delta_LS_emm_75.csv")

# Post hocs
ph_delta_LS_75_none <- pairs(interaction_delta_LS_emm_75, adjust = "none")
ph_delta_LS_75_bonf <- bonferroni(as.data.frame(ph_delta_LS_75_none), 3)

# ** delta_TR_BMD ---------------------------------------------------------

delta_TR_LMM_75 <- lmer(
  formula = delta_TR_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = TR_delta_data_75
)

# R-squared
rsquared(delta_TR_LMM_75)

# Fixed effects test
anova(delta_TR_LMM_75, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TR_LMM_75)

# Estimated marginal means for group
group_delta_TR_emm_75 <- emmeans(delta_TR_LMM_75, ~ attend_cat)

# Estimated marginal means for time
time_delta_TR_emm_75 <- emmeans(delta_TR_LMM_75, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TR_emm_75  <- emmeans(delta_TR_LMM_75, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_TR_emm_75_df <- as.data.frame(interaction_delta_TR_emm_75)
write_csv(interaction_delta_TR_emm_75_df, "output/interaction_delta_TR_emm_75.csv")

# Post hocs
ph_delta_TR_75_none <- pairs(interaction_delta_TR_emm_75, adjust = "none")
ph_delta_TR_75_bonf <- bonferroni(as.data.frame(ph_delta_TR_75_none), 3)