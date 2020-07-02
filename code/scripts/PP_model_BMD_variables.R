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
contrasts(df$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
TH_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, TH_BMD, TH_BMD_adjust, BMI, surgery,
    age, menopause, diabetes, thiazides, smoker
  )
FN_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, FN_BMD, FN_BMD_adjust, BMI, surgery,
    age, menopause, diabetes, thiazides, smoker
  )
LS_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, LS_BMD, LS_BMD_adjust, BMI, surgery,
    age, menopause, diabetes, thiazides, smoker
  )
TR_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, TR_BMD, TR_BMD_adjust, BMI, surgery,
    age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    var, "_BMD ~ 1 + attend_cat + time + attend_cat:time + ", var, 
    "_BMD_adjust + BMI + surgery + menopause + 
    age + diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

# ** LS_BMD ---------------------------------------------------------------

LS_LMM_PP <- lmer(formula = build_formula("LS"), data = LS_data)

# R-squared
rsquared(LS_LMM_PP)

# Fixed effects test
anova(LS_LMM_PP, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(LS_LMM_PP)

# Estimated marginal means for group
group_LS_PP_emm <- emmeans(LS_LMM_PP, ~ attend_cat)

# Estimated marginal means for time
time_LS_PP_emm <- emmeans(LS_LMM_PP, ~ time)

# Estimated marginal means for group x time interaction
interaction_LS_PP_emm  <- emmeans(LS_LMM_PP, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_LS_PP_emm_df <- as.data.frame(interaction_LS_PP_emm)
write_csv(interaction_LS_PP_emm_df, here("output", "interaction_LS_PP_emm_df.csv"))

# Post hocs
ph_LS_none <- pairs(interaction_LS_PP_emm, adjust = "none")
ph_LS_PP_bonf <- bonferroni(as.data.frame(ph_LS_none), 3)

# ** TR_BMD ---------------------------------------------------------------

TR_LMM_PP <- lmer(formula = build_formula("TR"), data = TR_data)

# R-squared
rsquared(TR_LMM_PP)

# Fixed effects test
anova(TR_LMM_PP, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(TR_LMM_PP)

# Estimated marginal means for group
group_TR_PP_emm <- emmeans(TR_LMM_PP, ~ attend_cat)

# Estimated marginal means for time
time_TR_PP_emm <- emmeans(TR_LMM_PP, ~ time)

# Estimated marginal means for group x time interaction
interaction_TR_PP_emm  <- emmeans(TR_LMM_PP, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_TR_PP_emm_df <- as.data.frame(interaction_TR_PP_emm)
write_csv(interaction_TR_PP_emm_df, here("output", "interaction_TR_PP_emm_df.csv"))

# Post hocs
ph_TR_none <- pairs(interaction_TR_PP_emm, adjust = "none")
ph_TR_PP_bonf <- bonferroni(as.data.frame(ph_TR_none), 3)

# ** TH_BMD ---------------------------------------------------------------

TH_LMM_PP <- lmer(formula = build_formula("TH"), data = TH_data)

# R-squared
rsquared(TH_LMM_PP)

# Fixed effects test
anova(TH_LMM_PP, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(TH_LMM_PP)

# Estimated marginal means for group
group_TH_PP_emm <- emmeans(TH_LMM_PP, ~ attend_cat)

# Estimated marginal means for time
time_TH_PP_emm <- emmeans(TH_LMM_PP, ~ time)

# Estimated marginal means for group x time interaction
interaction_TH_PP_emm  <- emmeans(TH_LMM_PP, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_TH_PP_emm_df <- as.data.frame(interaction_TH_PP_emm)
write_csv(interaction_TH_PP_emm_df, here("output", "interaction_TH_PP_emm_df.csv"))

# Post hocs
ph_TH_none <- pairs(interaction_TH_PP_emm, adjust = "none")
ph_TH_PP_bonf <- bonferroni(as.data.frame(ph_TH_none), 3)

# ** FN_BMD ---------------------------------------------------------------

FN_LMM_PP <- lmer(formula = build_formula("FN"), data = FN_data)

# R-squared
rsquared(FN_LMM_PP)

# Fixed effects test
anova(FN_LMM_PP, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(FN_LMM_PP)

# Estimated marginal means for group
group_FN_PP_emm <- emmeans(FN_LMM_PP, ~ attend_cat)

# Estimated marginal means for time
time_FN_PP_emm <- emmeans(FN_LMM_PP, ~ time)

# Estimated marginal means for group x time interaction
interaction_FN_PP_emm  <- emmeans(FN_LMM_PP, ~ attend_cat:time)
# Save into a data frame to build the plots
interaction_FN_PP_emm_df <- as.data.frame(interaction_FN_PP_emm)
write_csv(interaction_FN_PP_emm_df, here("output", "interaction_FN_PP_emm_df.csv"))

# Post hocs
ph_FN_none <- pairs(interaction_FN_PP_emm, adjust = "none")
ph_FN_PP_bonf <- bonferroni(as.data.frame(ph_FN_none), 3)
