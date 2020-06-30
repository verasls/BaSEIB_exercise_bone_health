# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
source(here("code", "functions", "repeat_baseline_values.R"))

# Rename variables --------------------------------------------------------

df <- read_csv(here("data", "raw", "database.csv"))

df <- df %>%
  select(
    subj = Subj, time = Time, group = Group, surgery = Surgery, sex = Sex, 
    age = Age, menopause = Menopause, pre_diabetes = `Pre-diabetes`, 
    diabetes = Diabetes, thiazides = Thiazides, smoker = Smoker, BMSi, 
    FN_BMD = Neck_BMD, TH_BMD = Hip__BMD, LS_BMD = LS__BMD, 
    TR_BMD = One_third__BMD, P1NP, CTX, PTH, vitD = VitaminD,
    sclerostin = Sclerostin, body_mass = Weight, BMI, 
    whole_body_fat_mass = Whole_Body__Fat_mass, 
    whole_body_lean_mass = Whole_Body__Lean_mass,
    whole_body_total_mass = Whole_Body__Total_mass,
    peak_torque_knee_ext_60ds = PT__Knee__60DS__Exten,
    peak_torque_knee_fle_60ds = PT__Knee__60DS__Flexi,
    peak_torque_knee_ext_60ds_body_mass = PT__Knee__60DS__Exten__Divided__Whole_Body__Total_mass,
    peak_torque_knee_fle_60ds_body_mass = PT__Knee__60DS__Flexi__Divided__Whole_Body__Total_mass,
    steps = Steps_day, PAEE, SB_h = SB_hours_per_day, 
    LPA_h = LPA_hours_per_day, MVPA_min = MVPA_min_per_day, 
    delta_FN_BMD = Delta__Neck__BMD, delta_TH_BMD = Delta__Hip__BMD, 
    delta_LS_BMD = Delta__LS__BMD, delta_TR_BMD = Delta__One_third__BMD, 
    delta_whole_body_BMD = Delta__Whole_Body__BMD, delta_BMSi = Delta__BMSi, 
    delta_P1NP = Delta__P1NP, delta_CTX = Delta__CTX, 
    delta_PTH = Delta__PTH, delta_vitD = Delta__VitaminaD, 
    delta_sclerostin = Delta__Sclerostin,
    delta_body_mass = Delta__Weight, delta_BMI = Delta__BMI, 
    delta_whole_body_fat_mass = Delta__Whole_Body__Fat_mass, 
    delta_whole_body_lean_mass = Delta__Whole_Body__Lean_mass, 
    delta_whole_body_total_mass = Delta__Whole_Body__Total_mass, 
    delta_peak_torque_knee_ext_60ds = Delta__PT__Knee__60DS__Exten, 
    delta_peak_torque_knee_fle_60ds = Delta__PT__Knee__60DS__Flexi,
    delta_peak_torque_knee_ext_60ds_body_mass = Delta__PT__Knee__60DS__Exten__Divided__Whole_Body, 
    delta_peak_torque_knee_fle_60ds_body_mass = Delta__PT__Knee__60DS__Flexi__Divided__Whole_Body,
    delta_steps = Delta__Steps, delta_PAEE = Delta__PAEE, delta_SB_h = Delta__SB, 
    delta_LPA_h = Delta__LPA, delta_MVPA_min = Delta__MVPA,
    attend_2nd_3rd = Attendance_rate__between_2nd_to_3rd, 
    attend_3rd_4th = Attendance_rate__between_3rd_to_4th,
    attend_2nd_4th = Attendance_rate__between_2nd_to_4th, attend_cat = Attendance__Cat, 
    age_adjust = Age_adjust, body_mass_adjust = Weight_adjust, BMI_adjust, BMSi_adjust, 
    FN_BMD_adjust = Neck_BMD_adjust, TH_BMD_adjust = Hip__BMD_adjust, 
    LS_BMD_adjust = LS__BMD_adjust, TR_BMD_adjust = One_third__BMD_adjust,
    whole_body_BMD_adjust = Whole_Body__BMD_adjust, P1NP_adjust, CTX_adjust, PTH_adjust, 
    vitD_adjust = VitaminD_adjust, sclerostin_adjust = Sclerostin_adjust, BMSi_Cat
  ) %>% 
  mutate(
    time = as.factor(time),
    group = recode(as.factor(group), "0" = "Control", "1" = "Exercise"),
    surgery = recode(as.factor(surgery), "0" = "RYGB", "1" = "Sleeve"),
    sex = recode(as.factor(sex), "0" = "Female", "1" = "Male"),
    menopause = ifelse(sex == "Male", 2, menopause),
    menopause = recode(as.factor(menopause), "0" = "No", "1" = "Yes", "2" = "Male"),
    pre_diabetes = recode(as.factor(pre_diabetes), "0" = "No", "1" = "Yes"),
    diabetes = recode(as.factor(diabetes), "0" = "No", "1" = "Yes"),
    thiazides = recode(as.factor(thiazides), "0" = "No", "1" = "Yes"),
    smoker = recode(as.factor(smoker), "0" = "No", "1" = "Yes"),
    attend_cat = recode(
      as.factor(attend_cat),
      "0" = "Control",
      "1" = "Under 50% training attendance",
      "2" = "Over 50% training attendance"
    )
  )

# Create baseline adjustment variables ------------------------------------

df <- df %>% 
  mutate(
    whole_body_fat_mass_adjust = repeat_baseline_values(
      df, whole_body_fat_mass, subj, time, 1
    ),
    whole_body_lean_mass_adjust = repeat_baseline_values(
      df, whole_body_lean_mass, subj, time, 1
    ),
    steps_adjust = repeat_baseline_values(
      df, steps, subj, time, 1
    ),
    SB_h_adjust = repeat_baseline_values(
      df, SB_h, subj, time, 1
    ),
    LPA_h_adjust = repeat_baseline_values(
      df, LPA_h, subj, time, 1
    ),
    MVPA_min_adjust = repeat_baseline_values(
      df, MVPA_min, subj, time, 1
    ),
    peak_torque_knee_ext_60ds_adjust = repeat_baseline_values(
      df, peak_torque_knee_ext_60ds, subj, time, 1
    ),
    peak_torque_knee_fle_60ds_adjust = repeat_baseline_values(
      df, peak_torque_knee_fle_60ds, subj, time, 1
    ),
    
    peak_torque_knee_ext_60ds_body_mass_adjust = repeat_baseline_values(
      df, peak_torque_knee_ext_60ds_body_mass, subj, time, 1
    ),
    peak_torque_knee_fle_60ds_body_mass_adjust = repeat_baseline_values(
      df, peak_torque_knee_fle_60ds_body_mass, subj, time, 1
    )
  )
  
# Prepare data frame for baseline comparisons -----------------------------

baseline_df <- df %>% filter(time == 1)

df_wide <- read_csv(here("data", "raw", "Database__Wide_format.csv"))
df_wide <- df_wide %>%
  select(
    subj = ID, group = Group, height = Height, 
    waist_circunference = Waist_circunference__1st,
    hip_circunference = Hip_circunference__1st, 
    waist_hip_ratio = Waist_to_Hip_ratio
  ) %>%
  mutate(
    group = recode(as.factor(group), "0" = "Control", "1" = "Exercise")
  )

baseline_df <- baseline_df %>% 
  full_join(
    df_wide, by = c("subj", "group")
  )

# Exclude subjects --------------------------------------------------------

# Subjects to exclude due to not having at least one follow-up assessment
exclude <- c(
  13, 15, 18, 19, 25, 32, 33, 35, 37, 40, 42, 48,
  53, 57, 59, 64, 65, 66, 74, 76, 82, 84, 86
)

'%!in%' <- function(x, table) !(x %in% table)
df <- df %>% filter(subj %!in% exclude)

baseline_df <- baseline_df %>% 
  mutate(
    exclude = ifelse(subj %in% exclude, "Yes", "No")
  )

# Prepare accelerometer data ----------------------------------------------

df_join <- df %>% select(subj, time, group, BMI_adjust)

acc <- read_csv(here("data", "raw", "summary_GRF_data.csv")) %>% 
  dplyr::select(
    subj = ID, time = eval, duration, n_days, n_peaks, above_thrsh = n_g_4.9
  ) %>% 
  mutate(
    time = recode(
      as.factor(time),
      "1st" = "1", "2nd" = "2", "3rd" = "3", "4th" = "4"
    )
  ) %>% 
  left_join(df_join, by = c("subj", "time")) %>% 
  dplyr::select(subj, time, group, everything()) %>% 
  arrange(subj)

acc <- acc %>% 
  mutate(
    above_thrsh_adjust = repeat_baseline_values(
      acc, above_thrsh, subj, time, 1
    )
  )

# Write data --------------------------------------------------------------

write_csv(df, here("data", "df.csv"))
write_csv(baseline_df, here("data", "baseline_df.csv"))
write_csv(acc, here("data", "acc.csv"))
