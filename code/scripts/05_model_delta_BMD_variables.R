# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))
source(here("code", "functions", "bonferroni.R"))
source(here("code", "functions", "mean_difference.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))
# Set contrasts of variable group to sum
contrasts(df$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
LS_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_LS_BMD, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
TR_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_TR_BMD, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
TH_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_TH_BMD, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
FN_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_FN_BMD, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    "delta_", var, "_BMD ~ 1 + attend_cat + time + attend_cat:time + ", 
    "BMI_adjust + surgery + menopause + 
    age + diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

# ** delta_LS_BMD ---------------------------------------------------------

delta_LS_LMM <- lmer(formula = build_formula("LS"), data = LS_delta_data)

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
write_csv(interaction_delta_LS_emm_df, here("output", "interaction_delta_LS_emm.csv"))

# Post hocs
ph_delta_LS_none <- pairs(interaction_delta_LS_emm, adjust = "none")
ph_delta_LS_bonf <- bonferroni(as.data.frame(ph_delta_LS_none), 3)

# Mean difference
mean_difference_delta(ph_delta_LS_none)

# ** delta_TR_BMD ---------------------------------------------------------

delta_TR_LMM <- lmer(formula = build_formula("TR"), data = TR_delta_data)

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
write_csv(interaction_delta_TR_emm_df, here("output", "interaction_delta_TR_emm.csv"))

# Post hocs
ph_delta_TR_none <- pairs(interaction_delta_TR_emm, adjust = "none")
ph_delta_TR_bonf <- bonferroni(as.data.frame(ph_delta_TR_none), 3)

# Mean difference
mean_difference_delta(ph_delta_TR_none)

# ** delta_TH_BMD ---------------------------------------------------------

delta_TH_LMM <- lmer(formula = build_formula("TH"), data = TH_delta_data)

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
write_csv(interaction_delta_TH_emm_df, here("output", "interaction_delta_TH_emm.csv"))

# Post hocs
ph_delta_TH_none <- pairs(interaction_delta_TH_emm, adjust = "none")
ph_delta_TH_bonf <- bonferroni(as.data.frame(ph_delta_TH_none), 3)

# Mean difference
mean_difference_delta(ph_delta_TH_none)

# ** delta_FN_BMD ---------------------------------------------------------

delta_FN_LMM <- lmer(formula = build_formula("FN"), data = FN_delta_data)

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
write_csv(interaction_delta_FN_emm_df, here("output", "interaction_delta_FN_emm.csv"))

# Post hocs
ph_delta_FN_none <- pairs(interaction_delta_FN_emm, adjust = "none")
ph_delta_FN_bonf <- bonferroni(as.data.frame(ph_delta_FN_none), 3)

# Mean difference
mean_difference_delta(ph_delta_FN_none)
mean_difference_delta_2(ph_delta_FN_none)
