# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(sjstats)
source(here("code", "functions", "read_data.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))
# Select variables
LS_lm_data <- df %>% 
  filter(time == 4) %>% 
  dplyr::select(
    subj, time, group, LS_BMD, LS_BMD_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
TR_lm_data <- df %>% 
  filter(time == 4) %>% 
  dplyr::select(
    subj, time, group, TR_BMD, TR_BMD_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )
TH_lm_data <- df %>% 
  filter(time == 4) %>% 
  dplyr::select(
    subj, time, group, TH_BMD, TH_BMD_adjust, BMI_adjust, 
    surgery, age, menopause, diabetes, thiazides, smoker
  )
FN_lm_data <- df %>% 
  filter(time == 4) %>% 
  dplyr::select(
    subj, time, group, FN_BMD, FN_BMD_adjust, BMI_adjust,
    surgery, age, menopause, diabetes, thiazides, smoker
  )

# Build models ------------------------------------------------------------

build_formula <- function(var) {
  f <- paste0(
    var, "_BMD ~ 1 + group + ", var, 
    "_BMD_adjust + BMI_adjust + surgery + menopause + 
    age + diabetes + thiazides + smoker + (1 | subj)"
  )
  as.formula(f)
}

# LS model ----------------------------------------------------------------

LS_lm <- lm(build_formula("LS"), data = LS_lm_data)
summary(LS_lm)
omega_sq(LS_lm)

# TH model ----------------------------------------------------------------

TH_lm <- lm(build_formula("TH"), data = TH_lm_data)
summary(TH_lm)
omega_sq(TH_lm)

# FN model ----------------------------------------------------------------

FN_lm <- lm(build_formula("FN"), data = FN_lm_data)
summary(FN_lm)
omega_sq(FN_lm)

# TR model ----------------------------------------------------------------

TR_lm <- lm(build_formula("TR"), data = TR_lm_data)
summary(TR_lm)
omega_sq(TR_lm)