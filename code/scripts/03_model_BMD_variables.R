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
contrasts(df$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
TH_data <- df %>% select(subj, time, group, TH_BMD, TH_BMD_adjust)
FN_data <- df %>% select(subj, time, group, FN_BMD, FN_BMD_adjust)
LS_data <- df %>% select(subj, time, group, LS_BMD, LS_BMD_adjust)
TR_data <- df %>% select(subj, time, group, TR_BMD, TR_BMD_adjust)

# Center variables
LS_data <- center_variable(LS_data, "LS_BMD_adjust")
TH_data <- center_variable(TH_data, "TH_BMD_adjust")
FN_data <- center_variable(FN_data, "FN_BMD_adjust")
TR_data <- center_variable(TR_data, "TR_BMD_adjust")

# Build models ------------------------------------------------------------

# ** TH_BMD ---------------------------------------------------------------

TH_LMM <- lmer(
  formula = TH_BMD ~ 1 + group + time + group:time + TH_BMD_adjust_centered + (1 | subj),
  data = TH_data
)

# R-squared
rsquared(TH_LMM)

# Fixed effects test
anova(TH_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(TH_LMM)

# Estimated marginal means for group
group_TH_emm <- emmeans(TH_LMM, ~ group)

# Estimated marginal means for time
time_TH_emm <- emmeans(TH_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_TH_emm  <- emmeans(TH_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_TH_emm_df <- as.data.frame(interaction_TH_emm)
write_csv(interaction_TH_emm_df, "output/interaction_TH_emm.csv")

# Post hocs
ph_TH_none <- pairs(interaction_TH_emm, adjust = "none")
ph_TH_bonf <- bonferroni(as.data.frame(ph_TH_none), 16)

# ** FN_BMD ---------------------------------------------------------------

FN_LMM <- lmer(
  formula = FN_BMD ~ 1 + group + time + group:time + FN_BMD_adjust_centered + (1 | subj),
  data = FN_data
)

# R-squared
rsquared(FN_LMM)

# Fixed effects test
anova(FN_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(FN_LMM)

# Estimated marginal means for group
group_FN_emm <- emmeans(FN_LMM, ~ group)

# Estimated marginal means for time
time_FN_emm <- emmeans(FN_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_FN_emm  <- emmeans(FN_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_FN_emm_df <- as.data.frame(interaction_FN_emm)
write_csv(interaction_FN_emm_df, "output/interaction_FN_emm.csv")

# Post hocs
ph_FN_none <- pairs(interaction_FN_emm, adjust = "none")
ph_FN_bonf <- bonferroni(as.data.frame(ph_FN_none), 16)

# ** LS_BMD ---------------------------------------------------------------

LS_LMM <- lmer(
  formula = LS_BMD ~ 1 + group + time + group:time + LS_BMD_adjust_centered + (1 | subj),
  data = LS_data
)

# R-squared
rsquared(LS_LMM)

# Fixed effects test
anova(LS_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(LS_LMM)

# Estimated marginal means for group
group_LS_emm <- emmeans(LS_LMM, ~ group)

# Estimated marginal means for time
time_LS_emm <- emmeans(LS_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_LS_emm  <- emmeans(LS_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_LS_emm_df <- as.data.frame(interaction_LS_emm)
write_csv(interaction_LS_emm_df, "output/interaction_LS_emm.csv")

# Post hocs
ph_LS_none <- pairs(interaction_LS_emm, adjust = "none")
ph_LS_bonf <- bonferroni(as.data.frame(ph_LS_none), 16)

# ** TR_BMD ---------------------------------------------------------------
  
  TR_LMM <- lmer(
    formula = TR_BMD ~ 1 + group + time + group:time + TR_BMD_adjust_centered + (1 | subj),
    data = TR_data
  )

# R-squared
rsquared(TR_LMM)

# Fixed effects test
anova(TR_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(TR_LMM)

# Estimated marginal means for group
group_TR_emm <- emmeans(TR_LMM, ~ group)

# Estimated marginal means for time
time_TR_emm <- emmeans(TR_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_TR_emm  <- emmeans(TR_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_TR_emm_df <- as.data.frame(interaction_TR_emm)
write_csv(interaction_TR_emm_df, "output/interaction_TR_emm.csv")

# Post hocs
ph_TR_none <- pairs(interaction_TR_emm, adjust = "none")
ph_TR_bonf <- bonferroni(as.data.frame(ph_TR_none), 16)