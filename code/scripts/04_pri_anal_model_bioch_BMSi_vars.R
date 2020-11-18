# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))
# Set contrasts of variable group to sum
contrasts(df$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Select variables
P1NP_data <- df %>% 
  dplyr::select(
    subj, time, group, P1NP, P1NP_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
CTX_data <- df %>% 
  dplyr::select(
    subj, time, group, CTX, CTX_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
PTH_data <- df %>% 
  dplyr::select(
    subj, time, group, PTH, PTH_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
vitD_data <- df %>% 
  dplyr::select(
    subj, time, group, vitD, vitD_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
sclerostin_data <- df %>% 
  dplyr::select(
    subj, time, group, sclerostin, sclerostin_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
BMSi_data <- df %>% 
  dplyr::select(
    subj, time, group, BMSi, BMSi_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    var, " ~ 1 + group + time + group:time + ", var, 
    "_adjust + BMI_adjust + surgery + age + menopause +
    diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

# ** P1NP_LMM ---------------------------------------------------------------

P1NP_LMM <- lmer(formula = build_formula("P1NP"), data = P1NP_data)

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

# Post hoc
ph_P1NP_none <- pairs(interaction_P1NP_emm, adjust = "none")

# ** CTX_LMM-------------------------------------------------------------

CTX_LMM <- lmer(formula = build_formula("CTX"), data = CTX_data)

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

# Post hoc
ph_CTX_none <- pairs(interaction_CTX_emm, adjust = "none")

# ** PTH_LMM ---------------------------------------------------------------

PTH_LMM <- lmer(formula = build_formula("PTH"), data = PTH_data)

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

# Post hoc
ph_PTH_none <- pairs(interaction_PTH_emm, adjust = "none")

# ** vitD_LMM ---------------------------------------------------------------

vitD_LMM <- lmer(formula = build_formula("vitD"), data = vitD_data)

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

# Post hoc
ph_vitD_none <- pairs(interaction_vitD_emm, adjust = "none")

# ** sclerostin_LMM ---------------------------------------------------------------

sclerostin_LMM <- lmer(formula = build_formula("sclerostin"), data = sclerostin_data)

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

# Post hoc
ph_sclerostin_none <- pairs(interaction_sclerostin_emm, adjust = "none")

# ** BMSi -----------------------------------------------------------------

BMSi_LMM <- lmer(formula = build_formula("BMSi"), data = BMSi_data)

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

# Post hoc
ph_BMSi_none <- pairs(interaction_BMSi_emm, adjust = "none")
