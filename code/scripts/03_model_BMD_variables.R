# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))
source(here("code", "functions", "mean_difference.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))

# Set contrasts of variable group to sum
contrasts(df$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
LS_data <- df %>% 
  dplyr::select(
    subj, time, group, LS_BMD, LS_BMD_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
TR_data <- df %>% 
  dplyr::select(
    subj, time, group, TR_BMD, TR_BMD_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
TH_data <- df %>% 
  dplyr::select(
    subj, time, group, TH_BMD, TH_BMD_adjust, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
FN_data <- df %>% 
  dplyr::select(
    subj, time, group, FN_BMD, FN_BMD_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    var, "_BMD ~ 1 + group + time + group:time + ", var, 
    "_BMD_adjust + BMI_adjust + surgery + menopause + 
    age + diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

# ** LS_BMD ---------------------------------------------------------------

LS_LMM <- lmer(formula = build_formula("LS"), data = LS_data)

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
write_csv(interaction_LS_emm_df, here("output", "interaction_LS_emm.csv"))

# Post hoc
ph_LS_none <- pairs(interaction_LS_emm, adjust = "none")

# Mean difference
mean_difference(ph_LS_none)
eff_size(
  interaction_LS_emm,
  sigma = sigma(LS_LMM),
  edf = 137
)

# ** TR_BMD ---------------------------------------------------------------

TR_LMM <- lmer(formula = build_formula("TR"), data = TR_data)

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
write_csv(interaction_TR_emm_df, here("output", "interaction_TR_emm.csv"))

# Post hoc
ph_TR_none <- pairs(interaction_TR_emm, adjust = "none")

# Mean difference
mean_difference(ph_TR_none)
eff_size(
  interaction_TR_emm,
  sigma = sigma(TR_LMM),
  edf = 171
)

# ** TH_BMD ---------------------------------------------------------------

TH_LMM <- lmer(formula = build_formula("TH"), data = TH_data)

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
write_csv(interaction_TH_emm_df, here("output", "interaction_TH_emm.csv"))

# Post hoc
ph_TH_none <- pairs(interaction_TH_emm, adjust = "none")

# Mean difference
mean_difference(ph_TH_none)
eff_size(
  interaction_TH_emm,
  sigma = sigma(TH_LMM),
  edf = 134
)

# ** FN_BMD ---------------------------------------------------------------

FN_LMM <- lmer(formula = build_formula("FN"), data = FN_data)

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
write_csv(interaction_FN_emm_df, here("output", "interaction_FN_emm.csv"))

# Post hoc
ph_FN_none <- pairs(interaction_FN_emm, adjust = "none")

# Mean difference
mean_difference(ph_FN_none)
eff_size(
  interaction_FN_emm,
  sigma = sigma(FN_LMM),
  edf = 137
)