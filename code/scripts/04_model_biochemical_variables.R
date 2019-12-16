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
P1NP_data       <- df %>% select(subj, time, group, P1NP, P1NP_adjust)
CTX_data        <- df %>% select(subj, time, group, CTX, CTX_adjust)
PTH_data        <- df %>% select(subj, time, group, PTH, PTH_adjust)
vitD_data       <- df %>% select(subj, time, group, vitD, vitD_adjust)
sclerostin_data <- df %>% select(subj, time, group, sclerostin, sclerostin_adjust)
BMSi            <- df %>% select(subj, time, group, BMSi, BMSi_adjust)

# Center variables
P1NP_data       <- center_variable(P1NP_data, "P1NP_adjust")
CTX_data        <- center_variable(CTX_data, "CTX_adjust")
PTH_data        <- center_variable(PTH_data, "PTH_adjust")
vitD_data       <- center_variable(vitD_data, "vitD_adjust")
sclerostin_data <- center_variable(sclerostin_data, "sclerostin_adjust")
BMSi_data       <- center_variable(BMSi, "BMSi_adjust")

# Build models ------------------------------------------------------------

# ** P1NP_BMD ---------------------------------------------------------------

P1NP_LMM <- lmer(
  formula = P1NP ~ 1 + group + time + group:time + P1NP_adjust_centered + (1 | subj),
  data = P1NP_data
)

# R-squared
rsquared(P1NP_LMM)

# Fixed effects test
anova(P1NP_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(P1NP_LMM)

# Estimated marginal means for group
group_P1NP_emm <- emmeans(P1NP_LMM, ~ group)

# Estimated marginal means for time
time_P1NP_emm <- emmeans(P1NP_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_P1NP_emm  <- emmeans(P1NP_LMM, ~ group:time)

# Post hocs
ph_P1NP_none <- pairs(interaction_P1NP_emm, adjust = "none")
ph_P1NP_bonf <- bonferroni(as.data.frame(ph_P1NP_none), 16)

# ** CTX_BMD ---------------------------------------------------------------

CTX_LMM <- lmer(
  formula = CTX ~ 1 + group + time + group:time + CTX_adjust_centered + (1 | subj),
  data = CTX_data
)

# R-squared
rsquared(CTX_LMM)

# Fixed effects test
anova(CTX_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(CTX_LMM)

# Estimated marginal means for group
group_CTX_emm <- emmeans(CTX_LMM, ~ group)

# Estimated marginal means for time
time_CTX_emm <- emmeans(CTX_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_CTX_emm  <- emmeans(CTX_LMM, ~ group:time)

# Post hocs
ph_CTX_none <- pairs(interaction_CTX_emm, adjust = "none")
ph_CTX_bonf <- bonferroni(as.data.frame(ph_CTX_none), 16)

# ** PTH_BMD ---------------------------------------------------------------

PTH_LMM <- lmer(
  formula = PTH ~ 1 + group + time + group:time + PTH_adjust_centered + (1 | subj),
  data = PTH_data
)

# R-squared
rsquared(PTH_LMM)

# Fixed effects test
anova(PTH_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(PTH_LMM)

# Estimated marginal means for group
group_PTH_emm <- emmeans(PTH_LMM, ~ group)

# Estimated marginal means for time
time_PTH_emm <- emmeans(PTH_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_PTH_emm  <- emmeans(PTH_LMM, ~ group:time)

# Post hocs
ph_PTH_none <- pairs(interaction_PTH_emm, adjust = "none")
ph_PTH_bonf <- bonferroni(as.data.frame(ph_PTH_none), 16)

# ** vitD_BMD ---------------------------------------------------------------

vitD_LMM <- lmer(
  formula = vitD ~ 1 + group + time + group:time + vitD_adjust_centered + (1 | subj),
  data = vitD_data
)

# R-squared
rsquared(vitD_LMM)

# Fixed effects test
anova(vitD_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(vitD_LMM)

# Estimated marginal means for group
group_vitD_emm <- emmeans(vitD_LMM, ~ group)

# Estimated marginal means for time
time_vitD_emm <- emmeans(vitD_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_vitD_emm  <- emmeans(vitD_LMM, ~ group:time)

# Post hocs
ph_vitD_none <- pairs(interaction_vitD_emm, adjust = "none")
ph_vitD_bonf <- bonferroni(as.data.frame(ph_vitD_none), 16)

# ** sclerostin_BMD ---------------------------------------------------------------

sclerostin_LMM <- lmer(
  formula = sclerostin ~ 1 + group + time + group:time + sclerostin_adjust_centered + (1 | subj),
  data = sclerostin_data
)

# R-squared
rsquared(sclerostin_LMM)

# Fixed effects test
anova(sclerostin_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(sclerostin_LMM)

# Estimated marginal means for group
group_sclerostin_emm <- emmeans(sclerostin_LMM, ~ group)

# Estimated marginal means for time
time_sclerostin_emm <- emmeans(sclerostin_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_sclerostin_emm  <- emmeans(sclerostin_LMM, ~ group:time)

# Post hocs
ph_sclerostin_none <- pairs(interaction_sclerostin_emm, adjust = "none")
ph_sclerostin_bonf <- bonferroni(as.data.frame(ph_sclerostin_none), 16)

# ** BMSi -----------------------------------------------------------------

BMSi_LMM <- lmer(
  formula = BMSi ~ 1 + group + time + group:time + BMSi_adjust_centered + (1 | subj),
  data = BMSi_data
)

# R-squared
rsquared(BMSi_LMM)

# Fixed effects test
anova(BMSi_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(BMSi_LMM)

# Estimated marginal means for group
group_BMSi_emm <- emmeans(BMSi_LMM, ~ group)

# Estimated marginal means for time
time_BMSi_emm <- emmeans(BMSi_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_BMSi_emm  <- emmeans(BMSi_LMM, ~ group:time)

# Post hocs
ph_BMSi_none <- pairs(interaction_BMSi_emm, adjust = "none")
ph_BMSi_bonf <- bonferroni(as.data.frame(ph_BMSi_none), 16)