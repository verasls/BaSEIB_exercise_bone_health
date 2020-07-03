# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(sjmisc)
library(lavaan)
source(here("code", "functions", "read_data.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))
df <- df %>%
  to_dummy(
    group, surgery, menopause, diabetes, thiazides, smoker, suffix = "label"
  ) %>% 
  bind_cols(df) %>% 
  filter(time == 4) %>% 
  select(
    subj, whole_body_lean_mass, steps, BMI_adjust, age, 
    LS_BMD, TR_BMD, group_Control, surgery_RYGB, menopause_Yes,
    menopause_Male, diabetes_Yes, smoker_Yes
  ) %>% 
  as_tibble()

acc <- read_csv(
  here("data", "acc.csv"),
  col_types = cols(
    time = col_factor(1:4),
    group = col_factor(c("Control", "Exercise"))
  )
) %>% 
  filter(time == 4) %>% 
  dplyr::select(subj, n_peaks = above_thrsh)
  

# Select variables
LS_data <-  df %>%
  left_join(acc, by = "subj") %>%
  select(subj, LS_BMD, everything()) %>% 
  na.omit()
TR_data <-  df %>%
  left_join(acc, by = "subj") %>%
  select(subj, TR_BMD, everything()) %>% 
  na.omit()

# Mediation analysis ------------------------------------------------------

# ** LS_BMD ---------------------------------------------------------------

LS_model <- "
  # Mediator
  LS_BMD ~ c*group_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    BMI_adjust + surgery_RYGB + age + menopause_Yes + menopause_Male + 
    diabetes_Yes + diabetes_Yes + smoker_Yes
  whole_body_lean_mass ~ a1*group_Control + BMI_adjust + surgery_RYGB + age + 
    menopause_Yes + menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  n_peaks ~ a2*group_Control + BMI_adjust + surgery_RYGB + age + 
    menopause_Yes + menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes

  # Direct
  direct := c
  
  # Indirect
  indirect1 := a1*b1
  indirect2 := a2*b2
  
  # Total
  total := c + (a1*b1) + (a2*b2)
  
  # Covariates
  whole_body_lean_mass ~~ n_peaks
  
  # Contrasts
  con := indirect1 - indirect2
"

LS_mediation <- sem(
  data = LS_data, 
  model = LS_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(LS_mediation)
parameterEstimates(LS_mediation)


# ** TR_BMD ---------------------------------------------------------------

TR_model <- "
  # Mediator
  TR_BMD ~ c*group_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    BMI_adjust + surgery_RYGB + age + menopause_Yes + menopause_Male + 
    diabetes_Yes + diabetes_Yes + smoker_Yes
  whole_body_lean_mass ~ a1*group_Control + BMI_adjust + surgery_RYGB + age + 
    menopause_Yes + menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  n_peaks ~ a2*group_Control + BMI_adjust + surgery_RYGB + age + 
    menopause_Yes + menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes

  # Direct
  direct := c
  
  # Indirect
  indirect1 := a1*b1
  indirect2 := a2*b2
  
  # Total
  total := c + (a1*b1) + (a2*b2)
  
  # Covariates
  whole_body_lean_mass ~~ n_peaks
  
  # Contrasts
  con := indirect1 - indirect2
"

TR_mediation <- sem(
  data = TR_data, 
  model = TR_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(TR_mediation)
parameterEstimates(TR_mediation)
