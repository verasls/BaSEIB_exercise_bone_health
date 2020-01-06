# Load packages and functions ---------------------------------------------

library(tidyverse)
library(gmodels)

# Load and prepare data ---------------------------------------------------

source("code/scripts/01_tidy_data.R")
control_df <- filter(baseline_df, group == "Control")
exercise_df <- filter(baseline_df, group == "Exercise")

# Continuous variables ----------------------------------------------------

# Normality tests control group
shapiro.test(control_df$age)
shapiro.test(control_df$height)
shapiro.test(control_df$body_mass)
shapiro.test(control_df$BMI)
shapiro.test(control_df$waist_circunference)
shapiro.test(control_df$hip_circunference)

# Normality tests exercise group
shapiro.test(exercise_df$age)
shapiro.test(exercise_df$height)
shapiro.test(exercise_df$body_mass)
shapiro.test(exercise_df$BMI)
shapiro.test(exercise_df$waist_circunference)
shapiro.test(exercise_df$hip_circunference)

# Independent samples t-test
t.test(age ~ group, data = baseline_df, paired = FALSE)
t.test(height ~ group, data = baseline_df, paired = FALSE)
t.test(body_mass ~ group, data = baseline_df, paired = FALSE)
t.test(BMI ~ group, data = baseline_df, paired = FALSE)
t.test(waist_circunference ~ group, data = baseline_df, paired = FALSE)
t.test(hip_circunference ~ group, data = baseline_df, paired = FALSE)

# Categorical variables ---------------------------------------------------

CrossTable(
  baseline_df$sex, baseline_df$group, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$surgery, baseline_df$group, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$menopause, baseline_df$group, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$diabetes, baseline_df$group, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$thiazides, baseline_df$group, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$smoker, baseline_df$group, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)