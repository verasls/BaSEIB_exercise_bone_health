# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lavaan)
source(here("code", "functions", "read_data.R"))
source(here("code", "functions", "get_lavaan_model.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))

acc <- read_csv(
  here("data", "acc.csv"),
  col_types = cols(
    time = col_factor(1:4),
    group = col_factor(c("Control", "Exercise"))
  )
) %>% 
  dplyr::select(subj, time, group, above_thrsh)

# Select variables
LS_data <- df %>% 
  dplyr::select(subj, time, group, LS_BMD, BMI_adjust) %>% 
  filter(time == 4) %>% 
  left_join(acc, by = c("subj", "time", "group")) %>%
  mutate(group = as.double(group)) %>% 
  na.omit()
FN_data <- df %>%
  dplyr::select(subj, time, group, FN_BMD, BMI_adjust) %>% 
  filter(time == 4) %>% 
  left_join(acc, by = c("subj", "time", "group")) %>%
  mutate(group = as.double(group)) %>% 
  na.omit()
TR_data <- df %>% 
  dplyr::select(subj, time, group, TR_BMD, BMI_adjust) %>% 
  filter(time == 4) %>% 
  left_join(acc, by = c("subj", "time", "group")) %>%
  mutate(group = as.double(group)) %>% 
  na.omit()

# Mediation analysis ------------------------------------------------------

# ** LS_BMD ---------------------------------------------------------------

LS_model <- get_lavaan_model(
  outcome = "LS_BMD",
  predictor = "group",
  mediator = "above_thrsh",
  covariate = "BMI_adjust"
)
LS_mediation <- sem(data = LS_data, model = LS_model, se = "bootstrap", bootstrap = 5000)
summary(LS_mediation)
parameterEstimates(LS_mediation)

# ** FN_BMD ---------------------------------------------------------------

FN_model <- get_lavaan_model(
  outcome = "FN_BMD",
  predictor = "group",
  mediator = "above_thrsh",
  covariate = "BMI_adjust"
)
FN_mediation <- sem(data = FN_data, model = FN_model, se = "bootstrap", bootstrap = 5000)
summary(FN_mediation)
parameterEstimates(FN_mediation)

# ** TR_BMD ---------------------------------------------------------------

TR_model <- get_lavaan_model(
  outcome = "TR_BMD",
  predictor = "group",
  mediator = "above_thrsh",
  covariate = "BMI_adjust"
)
TR_mediation <- sem(data = TR_data, model = TR_model, se = "bootstrap", bootstrap = 5000)
summary(TR_mediation)
parameterEstimates(TR_mediation)