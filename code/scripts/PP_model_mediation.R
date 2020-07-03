# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(sjmisc)
library(lavaan)
source(here("code", "functions", "read_data.R"))

# Load and prepare data ---------------------------------------------------

acc <- read_csv(
  here("data", "acc.csv"),
  col_types = cols(
    time = col_factor(1:4),
    group = col_factor(c("Control", "Exercise"))
  )
) %>% 
  filter(time == 4) %>% 
  dplyr::select(subj, n_peaks = above_thrsh)

df <- read_data(here("data", "df.csv")) %>% 
  filter(attend_cat != "Under 50% training attendance") %>% 
  droplevels()

PP_mediation_df <- df %>%
  to_dummy(
    attend_cat, surgery, menopause, diabetes, thiazides, smoker, suffix = "label"
  ) %>% 
  bind_cols(df) %>% 
  filter(time == 4) %>%
  select(
    subj, whole_body_lean_mass, BMI_adjust, age, 
    LS_BMD, TR_BMD, FN_BMD,
    LS_BMD_adjust, TR_BMD_adjust, FN_BMD_adjust, 
    attend_cat_Control, surgery_RYGB, menopause_Yes, 
    menopause_Male, diabetes_Yes, smoker_Yes
  ) %>% 
  left_join(acc, by = "subj") %>% 
  as_tibble()

# Mediation analysis ------------------------------------------------------

# ** LS_BMD ---------------------------------------------------------------

PP_LS_model <- "
  # Mediator
  LS_BMD ~ c*attend_cat_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    LS_BMD_adjust + BMI_adjust + surgery_RYGB + age + menopause_Yes + 
    menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  whole_body_lean_mass ~ a1*attend_cat_Control + BMI_adjust + surgery_RYGB + age + 
    menopause_Yes + menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  n_peaks ~ a2*attend_cat_Control + BMI_adjust + surgery_RYGB + age + 
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

PP_LS_mediation <- sem(
  data = PP_mediation_df, 
  model = PP_LS_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(PP_LS_mediation)
parameterEstimates(PP_LS_mediation)


# ** TR_BMD ---------------------------------------------------------------

PP_TR_model <- "
  # Mediator
  TR_BMD ~ c*attend_cat_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    TR_BMD_adjust + BMI_adjust + surgery_RYGB + age + menopause_Yes + 
    menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  whole_body_lean_mass ~ a1*attend_cat_Control + BMI_adjust + surgery_RYGB + age + 
    menopause_Yes + menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  n_peaks ~ a2*attend_cat_Control + BMI_adjust + surgery_RYGB + age + 
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

PP_TR_mediation <- sem(
  data = PP_mediation_df, 
  model = PP_TR_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(PP_TR_mediation)
parameterEstimates(PP_TR_mediation)

# ** FN_BMD ---------------------------------------------------------------

PP_FN_model <- "
  # Mediator
  FN_BMD ~ c*attend_cat_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    FN_BMD_adjust + BMI_adjust + surgery_RYGB + age + menopause_Yes + 
    menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  whole_body_lean_mass ~ a1*attend_cat_Control + BMI_adjust + surgery_RYGB + age + 
    menopause_Yes + menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
  n_peaks ~ a2*attend_cat_Control + BMI_adjust + surgery_RYGB + age + 
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

PP_FN_mediation <- sem(
  data = PP_mediation_df, 
  model = PP_FN_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(PP_FN_mediation)
parameterEstimates(PP_FN_mediation)
