# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(gmodels)
source(here("code", "functions", "read_data.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv"))
baseline_df <- read_data(here("data", "baseline_df.csv"))
control_df <- filter(baseline_df, group == "Control" & exclude == "No")
exercise_df <- filter(baseline_df, group == "Exercise" & exclude == "No")
exercise_under_50_df <- filter(baseline_df, attend_cat == "Under 50% training attendance" & exclude == "No")
exercise_over_50_df <- filter(baseline_df, attend_cat == "Over 50% training attendance" & exclude == "No")
baseline_inc_df <- filter(baseline_df, exclude == "No")
baseline_exc_df <- filter(baseline_df, exclude == "Yes")

# Descriptives ------------------------------------------------------------

# Control group
control_descriptives <- control_df %>% 
  summarise(
    across(
      c(
        age, body_mass, height, BMI, waist_circunference,
        hip_circunference, waist_hip_ratio
      ), 
      list(mean = mean, sd = sd), 
      .names = "{fn}_{col}"
    )
  )
control_sex       <- prop.table(table(control_df$sex))
control_surgery   <- prop.table(table(control_df$surgery))
control_menopause <- prop.table(table(control_df$menopause))
control_diabetes  <- prop.table(table(control_df$diabetes))
control_thiazides <- prop.table(table(control_df$thiazides))
control_smoker    <- prop.table(table(control_df$smoker))

# Exercise group
exercise_descriptives <- exercise_df %>% 
  summarise(
    across(
      c(
        age, body_mass, height, BMI, waist_circunference,
        hip_circunference, waist_hip_ratio
      ), 
      list(mean = mean, sd = sd), 
      .names = "{fn}_{col}"
    )
  )
exercise_sex       <- prop.table(table(exercise_df$sex))
exercise_surgery   <- prop.table(table(exercise_df$surgery))
exercise_menopause <- prop.table(table(exercise_df$menopause))
exercise_diabetes  <- prop.table(table(exercise_df$diabetes))
exercise_thiazides <- prop.table(table(exercise_df$thiazides))
exercise_smoker    <- prop.table(table(exercise_df$smoker))

# Exercise group (under 50% training attendance)
exercise_under_50_descriptives <- exercise_under_50_df %>% 
  summarise(
    across(
      c(
        age, body_mass, height, BMI, waist_circunference,
        hip_circunference, waist_hip_ratio
      ), 
      list(mean = mean, sd = sd), 
      .names = "{fn}_{col}"
    )
  )
exercise_under_50_sex       <- prop.table(table(exercise_under_50_df$sex))
exercise_under_50_surgery   <- prop.table(table(exercise_under_50_df$surgery))
exercise_under_50_menopause <- prop.table(table(exercise_under_50_df$menopause))
exercise_under_50_diabetes  <- prop.table(table(exercise_under_50_df$diabetes))
exercise_under_50_thiazides <- prop.table(table(exercise_under_50_df$thiazides))
exercise_under_50_smoker    <- prop.table(table(exercise_under_50_df$smoker))

# Exercise group (over 50% training attendance)
exercise_over_50_descriptives <- exercise_over_50_df %>% 
  summarise(
    across(
      c(
        age, body_mass, height, BMI, waist_circunference,
        hip_circunference, waist_hip_ratio
      ), 
      list(mean = mean, sd = sd), 
      .names = "{fn}_{col}"
    )
  )
exercise_over_50_sex       <- prop.table(table(exercise_over_50_df$sex))
exercise_over_50_surgery   <- prop.table(table(exercise_over_50_df$surgery))
exercise_over_50_menopause <- prop.table(table(exercise_over_50_df$menopause))
exercise_over_50_diabetes  <- prop.table(table(exercise_over_50_df$diabetes))
exercise_over_50_thiazides <- prop.table(table(exercise_over_50_df$thiazides))
exercise_over_50_smoker    <- prop.table(table(exercise_over_50_df$smoker))

# Included subjects
baseline_inc_descriptives <- baseline_inc_df %>% 
  summarise(
    across(
      c(
        age, body_mass, height, BMI, waist_circunference,
        hip_circunference, waist_hip_ratio
      ), 
      list(mean = mean, sd = sd), 
      .names = "{fn}_{col}"
    )
  )
baseline_inc_sex       <- prop.table(table(baseline_inc_df$sex))
baseline_inc_surgery   <- prop.table(table(baseline_inc_df$surgery))
baseline_inc_menopause <- prop.table(table(baseline_inc_df$menopause))
baseline_inc_diabetes  <- prop.table(table(baseline_inc_df$diabetes))
baseline_inc_thiazides <- prop.table(table(baseline_inc_df$thiazides))
baseline_inc_smoker    <- prop.table(table(baseline_inc_df$smoker))

# Excluded subjects
baseline_exc_descriptives <- baseline_exc_df %>% 
  summarise(
    across(
      c(
        age, body_mass, height, BMI, waist_circunference,
        hip_circunference, waist_hip_ratio
      ), 
      list(mean = mean, sd = sd), 
      .names = "{fn}_{col}"
    )
  )
baseline_exc_sex       <- prop.table(table(baseline_exc_df$sex))
baseline_exc_surgery   <- prop.table(table(baseline_exc_df$surgery))
baseline_exc_menopause <- prop.table(table(baseline_exc_df$menopause))
baseline_exc_diabetes  <- prop.table(table(baseline_exc_df$diabetes))
baseline_exc_thiazides <- prop.table(table(baseline_exc_df$thiazides))
baseline_exc_smoker    <- prop.table(table(baseline_exc_df$smoker))

# Included X Excluded subjects --------------------------------------------

# ** Continuous variables -------------------------------------------------

# Normality tests included subjects
shapiro.test(baseline_inc_df$age)
shapiro.test(baseline_inc_df$height)
shapiro.test(baseline_inc_df$body_mass)
shapiro.test(baseline_inc_df$BMI)
shapiro.test(baseline_inc_df$waist_circunference)
shapiro.test(baseline_inc_df$hip_circunference)
shapiro.test(baseline_inc_df$waist_hip_ratio)

# Normality tests excluded subjects
shapiro.test(baseline_exc_df$age)
shapiro.test(baseline_exc_df$height)
shapiro.test(baseline_exc_df$body_mass)
shapiro.test(baseline_exc_df$BMI)
shapiro.test(baseline_exc_df$waist_circunference)
shapiro.test(baseline_exc_df$hip_circunference)
shapiro.test(baseline_exc_df$waist_hip_ratio)

# Independent samples t-test
t.test(age ~ exclude, data = baseline_df, paired = FALSE)
t.test(height ~ exclude, data = baseline_df, paired = FALSE)
t.test(body_mass ~ exclude, data = baseline_df, paired = FALSE)
t.test(BMI ~ exclude, data = baseline_df, paired = FALSE)
t.test(waist_circunference ~ exclude, data = baseline_df, paired = FALSE)
t.test(hip_circunference ~ exclude, data = baseline_df, paired = FALSE)
t.test(waist_hip_ratio ~ exclude, data = baseline_df, paired = FALSE)

# ** Categorical variables ------------------------------------------------

CrossTable(
  baseline_df$sex, baseline_df$exclude, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$surgery, baseline_df$exclude, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$menopause, baseline_df$exclude, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$diabetes, baseline_df$exclude, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$thiazides, baseline_df$exclude, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$smoker, baseline_df$exclude, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)
