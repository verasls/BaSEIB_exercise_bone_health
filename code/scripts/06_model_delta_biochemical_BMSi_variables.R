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
P1NP_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_P1NP, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
CTX_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_CTX, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
PTH_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_PTH, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
vitD_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_vitD, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
sclerostin_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_sclerostin, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
BMSi_delta_data <- df %>% 
  dplyr::select(
    subj, time, attend_cat, delta_BMSi, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    "delta_", var, " ~ 1 + attend_cat + time + attend_cat:time + ",
    "BMI_adjust + surgery + age + menopause +
    diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

# ** delta_P1NP -----------------------------------------------------------

delta_P1NP_LMM <- lmer(
  formula = build_formula("P1NP"),
  data = P1NP_delta_data
)

# R-squared
rsquared(delta_P1NP_LMM)

# Fixed effects test
anova(delta_P1NP_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_P1NP_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_P1NP_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_P1NP_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_P1NP_emm  <- emmeans(delta_P1NP_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_P1NP_none <- pairs(interaction_delta_P1NP_emm, adjust = "none")
ph_delta_P1NP_bonf <- bonferroni(as.data.frame(ph_delta_P1NP_none), 3)

# ** delta_CTX ------------------------------------------------------------

delta_CTX_LMM <- lmer(
  formula = build_formula("CTX"),
  data = CTX_delta_data
)

# R-squared
rsquared(delta_CTX_LMM)

# Fixed effects test
anova(delta_CTX_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_CTX_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_CTX_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_CTX_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_CTX_emm  <- emmeans(delta_CTX_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_CTX_none <- pairs(interaction_delta_CTX_emm, adjust = "none")
ph_delta_CTX_bonf <- bonferroni(as.data.frame(ph_delta_CTX_none), 3)

# ** delta_PTH ------------------------------------------------------------

delta_PTH_LMM <- lmer(
  formula = build_formula("PTH"),
  data = PTH_delta_data
)

# R-squared
rsquared(delta_PTH_LMM)

# Fixed effects test
anova(delta_PTH_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_PTH_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_PTH_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_PTH_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_PTH_emm  <- emmeans(delta_PTH_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_PTH_none <- pairs(interaction_delta_PTH_emm, adjust = "none")
ph_delta_PTH_bonf <- bonferroni(as.data.frame(ph_delta_PTH_none), 3)

# ** delta_vitD -----------------------------------------------------------

delta_vitD_LMM <- lmer(
  formula = build_formula("vitD"),
  data = vitD_delta_data
)

# R-squared
rsquared(delta_vitD_LMM)

# Fixed effects test
anova(delta_vitD_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_vitD_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_vitD_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_vitD_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_vitD_emm  <- emmeans(delta_vitD_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_vitD_none <- pairs(interaction_delta_vitD_emm, adjust = "none")
ph_delta_vitD_bonf <- bonferroni(as.data.frame(ph_delta_vitD_none), 3)

# ** delta_sclerostin -----------------------------------------------------

delta_sclerostin_LMM <- lmer(
  formula = build_formula("sclerostin"),
  data = sclerostin_delta_data
)

# R-squared
rsquared(delta_sclerostin_LMM)

# Fixed effects test
anova(delta_sclerostin_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_sclerostin_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_sclerostin_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_sclerostin_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_sclerostin_emm  <- emmeans(delta_sclerostin_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_sclerostin_none <- pairs(interaction_delta_sclerostin_emm, adjust = "none")
ph_delta_sclerostin_bonf <- bonferroni(as.data.frame(ph_delta_sclerostin_none), 3)

# ** delta_BMSi -----------------------------------------------------------

delta_BMSi_LMM <- lmer(
  formula = build_formula("BMSi"),
  data = BMSi_delta_data
)

# R-squared
rsquared(delta_BMSi_LMM)

# Fixed effects test
anova(delta_BMSi_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_BMSi_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_BMSi_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_BMSi_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_BMSi_emm  <- emmeans(delta_BMSi_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_BMSi_none <- pairs(interaction_delta_BMSi_emm, adjust = "none")
ph_delta_BMSi_bonf <- bonferroni(as.data.frame(ph_delta_BMSi_none), 3)
