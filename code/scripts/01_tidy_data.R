# Load packages and functions ---------------------------------------------

library(tidyverse)

# Rename variables --------------------------------------------------------

df <- read_csv("data/database.csv") %>% 
  select(
    subj = Subj, time = Time, group = Group, surgery = Surgery, sex = Sex, age = Age, 
    menopause = Menopause, pre_diabetes = `Pre-diabetes`, diabetes = Diabetes,
    thiazides = Thiazides, smoker = Smoker, BMSi, FN_BMD = Neck_BMD,
    TH_BMD = Hip__BMD, LS_BMD = LS__BMD, TR_BMD = One_third__BMD, 
    whole_body_BMD = Whole_Body__BMD, P1NP, CTX, PTH, vitD = VitaminD, 
    sclerostin = Sclerostin, body_mass = Weight, BMI, 
    whole_body_fat_mass = Whole_Body__Fat_mass, 
    whole_body_lean_mass = Whole_Body__Lean_mass,
    whole_body_total_mass = Whole_Body__Total_mass,
    peak_torque_knee_ext_60ds = PT__Knee__60DS__Exten,
    peak_torque_knee_fle_60ds = PT__Knee__60DS__Flexi,
    peak_torque_knee_ext_60ds_body_mass = PT__Knee__60DS__Exten__Divided__Whole_Body__Total_mass,
    peak_torque_knee_fle_60ds_body_mass = PT__Knee__60DS__Flexi__Divided__Whole_Body__Total_mass,
    steps = Steps_day, PAEE, SB_h = SB_hours_per_day, LPA_h = LPA_hours_per_day, 
    MVPA_min = MVPA_min_per_day, 
    delta_FN_BMD = Delta__Neck__BMD, delta_TH_BMD = Delta__Hip__BMD, delta_LS_BMD = Delta__LS__BMD,
    delta_TR_BMD = Delta__One_third__BMD, delta_whole_body_BMD = Delta__Whole_Body__BMD,
    delta_BMSi = Delta__BMSi, delta_P1NP = Delta__P1NP, delta_CTX = Delta__CTX, 
    delta_PTH = Delta__PTH, delta_vitD = Delta__VitaminaD, delta_sclerostin = Delta__Sclerostin,
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
    whole_body_MBD_adjust = Whole_Body__BMD_adjust, P1NP_adjust, CTX_adjust, PTH_adjust, 
    vitD_adjust = VitaminD_adjust, sclerostin_adjust = Sclerostin_adjust, BMSi_Cat,
    time_continuous = Continuous_Time, time_continuous_0123 = Continuous_Time_0123
  )