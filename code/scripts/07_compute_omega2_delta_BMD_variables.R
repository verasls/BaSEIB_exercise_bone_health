# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(sjstats)

# Load and prepare data ---------------------------------------------------

source(here("code", "scripts", "01_tidy_data.R"))
# Create an intermediate df
lm_df <- df %>% filter(
  time == 4 & attend_cat %in% c("Control", "Over 50% training attendance")
)
# Drop unused level
lm_df$attend_cat <- factor(lm_df$attend_cat)
# Set contrasts of variable group to sum
contrasts(lm_df$attend_cat) <- matrix(rev(contr.sum(2)), ncol = 1)

# Select variables
TH_lm_data <- lm_df %>% dplyr::select(subj, time, attend_cat, delta_TH_BMD, BMI_adjust)
FN_lm_data <- lm_df %>% dplyr::select(subj, time, attend_cat, delta_FN_BMD, BMI_adjust)
LS_lm_data <- lm_df %>% dplyr::select(subj, time, attend_cat, delta_LS_BMD, BMI_adjust)
TR_lm_data <- lm_df %>% dplyr::select(subj, time, attend_cat, delta_TR_BMD, BMI_adjust)

# TH model ----------------------------------------------------------------

TH_lm <- lm(
  delta_TH_BMD ~ attend_cat + BMI_adjust,
  data = TH_lm_data
)
summary(TH_lm)
omega_sq(TH_lm)

# FN model ----------------------------------------------------------------

FN_lm <- lm(
  delta_FN_BMD ~ attend_cat + BMI_adjust,
  data = FN_lm_data
)
summary(FN_lm)
omega_sq(FN_lm)

# LS model ----------------------------------------------------------------

LS_lm <- lm(
  delta_LS_BMD ~ attend_cat + BMI_adjust,
  data = LS_lm_data
)
summary(LS_lm)
omega_sq(LS_lm)

# TR model ----------------------------------------------------------------

TR_lm <- lm(
  delta_TR_BMD ~ attend_cat + BMI_adjust,
  data = TR_lm_data
)
summary(TR_lm)
omega_sq(TR_lm)