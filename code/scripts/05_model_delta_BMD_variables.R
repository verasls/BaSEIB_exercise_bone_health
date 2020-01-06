# Load packages and functions ---------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source("code/functions/center_variable.R")
source("code/functions/bonferroni.R")

# Load and prepare data ---------------------------------------------------

source("code/scripts/01_tidy_data.R")
# Set contrasts of variable group to deviation
contrasts(df$attend_cat) <- matrix(rev(contr.sum(3)), ncol = 2)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
TH_delta_data <- df %>% dplyr::select(subj, time, attend_cat, delta_TH_BMD, BMI_adjust)
FN_delta_data <- df %>% dplyr::select(subj, time, attend_cat, delta_FN_BMD, BMI_adjust)
LS_delta_data <- df %>% dplyr::select(subj, time, attend_cat, delta_LS_BMD, BMI_adjust)
TR_delta_data <- df %>% dplyr::select(subj, time, attend_cat, delta_TR_BMD, BMI_adjust)

# Build models ------------------------------------------------------------

# ** delta_TH_BMD ---------------------------------------------------------

delta_TH_LMM <- lmer(
  formula = delta_TH_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = TH_delta_data
)

# R-squared
rsquared(delta_TH_LMM)

# Fixed effects test
anova(delta_TH_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TH_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_TH_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_TH_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TH_emm  <- emmeans(delta_TH_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_TH_emm_df <- as.data.frame(interaction_delta_TH_emm)
write_csv(interaction_delta_TH_emm_df, "output/interaction_delta_TH_emm.csv")

# Post hocs
ph_delta_TH_none <- pairs(interaction_delta_TH_emm, adjust = "none")
ph_delta_TH_bonf <- bonferroni(as.data.frame(ph_delta_TH_none), 3)

# ** delta_FN_BMD ---------------------------------------------------------

delta_FN_LMM <- lmer(
  formula = delta_FN_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = FN_delta_data
)

# R-squared
rsquared(delta_FN_LMM)

# Fixed effects test
anova(delta_FN_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_FN_LMM)

# Estimated marginal means for group
group_delta_FN_emm <- emmeans(delta_FN_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_FN_emm <- emmeans(delta_FN_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_FN_emm  <- emmeans(delta_FN_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_FN_emm_df <- as.data.frame(interaction_delta_FN_emm)
write_csv(interaction_delta_FN_emm_df, "output/interaction_delta_FN_emm.csv")

# Post hocs
ph_delta_FN_none <- pairs(interaction_delta_FN_emm, adjust = "none")
ph_delta_FN_bonf <- bonferroni(as.data.frame(ph_delta_FN_none), 3)

# ** delta_LS_BMD ---------------------------------------------------------

delta_LS_LMM <- lmer(
  formula = delta_LS_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = LS_delta_data
)

# R-squared
rsquared(delta_LS_LMM)

# Fixed effects test
anova(delta_LS_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_LS_LMM)

# Estimated marginal means for group
group_delta_LS_emm <- emmeans(delta_LS_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_LS_emm <- emmeans(delta_LS_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_LS_emm  <- emmeans(delta_LS_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_LS_emm_df <- as.data.frame(interaction_delta_LS_emm)
write_csv(interaction_delta_LS_emm_df, "output/interaction_delta_LS_emm.csv")

# Post hocs
ph_delta_LS_none <- pairs(interaction_delta_LS_emm, adjust = "none")
ph_delta_LS_bonf <- bonferroni(as.data.frame(ph_delta_LS_none), 3)

# ** delta_TR_BMD ---------------------------------------------------------

delta_TR_LMM <- lmer(
  formula = delta_TR_BMD ~ 1 + attend_cat + time + attend_cat:time + BMI_adjust + (1 | subj),
  data = TR_delta_data
)

# R-squared
rsquared(delta_TR_LMM)

# Fixed effects test
anova(delta_TR_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TR_LMM)

# Estimated marginal means for group
group_delta_TR_emm <- emmeans(delta_TR_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TR_emm <- emmeans(delta_TR_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TR_emm  <- emmeans(delta_TR_LMM, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_delta_TR_emm_df <- as.data.frame(interaction_delta_TR_emm)
write_csv(interaction_delta_TR_emm_df, "output/interaction_delta_TR_emm.csv")

# Post hocs
ph_delta_TR_none <- pairs(interaction_delta_TR_emm, adjust = "none")
ph_delta_TR_bonf <- bonferroni(as.data.frame(ph_delta_TR_none), 3)