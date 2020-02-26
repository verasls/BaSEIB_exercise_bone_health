# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lavaan)
source(here("code", "functions", "get_lavaan_model.R"))

# Load and prepare data ---------------------------------------------------

source(here("code", "scripts", "01_tidy_data.R"))
# Select variables
FN_delta_data <- df %>% 
  dplyr::select(subj, time, group, attend_cat, delta_FN_BMD, delta_whole_body_lean_mass, BMI_adjust) %>% 
  filter(time == 4) %>% 
  filter(attend_cat == "Control" | attend_cat == "Over 50% training attendance") %>% 
  na.omit()
LS_delta_data <- df %>% 
  dplyr::select(subj, time, group, attend_cat, delta_LS_BMD, delta_whole_body_lean_mass, BMI_adjust) %>% 
  filter(time == 4) %>% 
  filter(attend_cat == "Control" | attend_cat == "Over 50% training attendance") %>% 
  na.omit()
TR_delta_data <- df %>% 
  dplyr::select(subj, time, group, attend_cat, delta_TR_BMD, delta_whole_body_lean_mass, BMI_adjust) %>% 
  filter(time == 4) %>% 
  filter(attend_cat == "Control" | attend_cat == "Over 50% training attendance") %>% 
  na.omit()

# Recode group variable into numerical
FN_delta_data$group <- as.double(FN_delta_data$group)
LS_delta_data$group <- as.double(LS_delta_data$group)
TR_delta_data$group <- as.double(TR_delta_data$group)

# Mediation analysis ------------------------------------------------------

# ** FN_BMD ---------------------------------------------------------------

FN_model <- get_lavaan_model(
  outcome = "delta_FN_BMD",
  predictor = "group",
  mediator = "delta_whole_body_lean_mass",
  covariate = "BMI_adjust"
)
FN_mediation <- sem(data = FN_delta_data, model = FN_model, se = "bootstrap", bootstrap = 5000)
summary(FN_mediation)
parameterEstimates(FN_mediation)

# ** LS_BMD ---------------------------------------------------------------

LS_model <- get_lavaan_model(
  outcome = "delta_LS_BMD",
  predictor = "group",
  mediator = "delta_whole_body_lean_mass",
  covariate = "BMI_adjust"
)
LS_mediation <- sem(data = LS_delta_data, model = LS_model, se = "bootstrap", bootstrap = 5000)
summary(LS_mediation)
parameterEstimates(LS_mediation)

# ** TR_BMD ---------------------------------------------------------------

TR_model <- get_lavaan_model(
  outcome = "delta_TR_BMD",
  predictor = "group",
  mediator = "delta_whole_body_lean_mass",
  covariate = "BMI_adjust"
)
TR_mediation <- sem(data = TR_delta_data, model = TR_model, se = "bootstrap", bootstrap = 5000)
summary(TR_mediation)
parameterEstimates(TR_mediation)