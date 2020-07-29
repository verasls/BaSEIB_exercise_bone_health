# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(sjmisc)
library(lavaan)
source(here("code", "functions", "read_data.R"))
source(here("code", "functions", "bootstrap_ab_ps.R"))

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

df <- read_data(here("data", "df.csv"))

mediation_df <- df %>%
  to_dummy(
    group, surgery, menopause, diabetes, thiazides, smoker, suffix = "label"
  ) %>% 
  bind_cols(df) %>% 
  filter(time == 4) %>% 
  select(
    subj, whole_body_lean_mass, BMI_adjust, age, 
    LS_BMD, TR_BMD, LS_BMD_adjust, TR_BMD_adjust,
    FN_BMD, FN_BMD_adjust,
    group_Control, surgery_RYGB, menopause_Yes,
    menopause_Male, diabetes_Yes, smoker_Yes
  ) %>% 
  left_join(acc, by = "subj") %>% 
  as_tibble()

# Mediation analysis ------------------------------------------------------

# ** FN_BMD ---------------------------------------------------------------

FN_model <- "
  # Mediator
  FN_BMD ~ c*group_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    FN_BMD_adjust + BMI_adjust + surgery_RYGB + age + menopause_Yes +
    menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
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

FN_mediation <- sem(
  data = mediation_df, 
  model = FN_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(FN_mediation)
parameterEstimates(FN_mediation)
FN_ab_ps_ind_1 <- bootstrap_ab_ps(mediation_df, "FN_BMD", - 0.006)
FN_ab_ps_ind_2 <- bootstrap_ab_ps(mediation_df, "FN_BMD", 0.006)
FN_ab_ps_ind_tot <- bootstrap_ab_ps(mediation_df, "FN_BMD", - 0.006 + 0.006)

# ** LS_BMD ---------------------------------------------------------------

LS_model <- "
  # Mediator
  LS_BMD ~ c*group_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    LS_BMD_adjust + BMI_adjust + surgery_RYGB + age + menopause_Yes + 
    menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
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
  data = mediation_df, 
  model = LS_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(LS_mediation)
parameterEstimates(LS_mediation)
LS_ab_ps_ind_1 <- bootstrap_ab_ps(mediation_df, "LS_BMD", - 0.007)
LS_ab_ps_ind_2 <- bootstrap_ab_ps(mediation_df, "LS_BMD", 0.009)
LS_ab_ps_ind_tot <- bootstrap_ab_ps(mediation_df, "LS_BMD", - 0.007 + 0.009)

# ** TR_BMD ---------------------------------------------------------------

TR_model <- "
  # Mediator
  TR_BMD ~ c*group_Control + b1*whole_body_lean_mass + b2*n_peaks + 
    TR_BMD_adjust + BMI_adjust + surgery_RYGB + age + menopause_Yes +
    menopause_Male + diabetes_Yes + diabetes_Yes + smoker_Yes
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
  data = mediation_df, 
  model = TR_model, 
  se = "bootstrap", 
  bootstrap = 5000
)
summary(TR_mediation)
parameterEstimates(TR_mediation)
TR_ab_ps_ind_1 <- bootstrap_ab_ps(mediation_df, "TR_BMD", 0.000)
TR_ab_ps_ind_2 <- bootstrap_ab_ps(mediation_df, "TR_BMD", 0.002)
TR_ab_ps_ind_tot <- bootstrap_ab_ps(mediation_df, "TR_BMD", 0.000 + 0.002)
