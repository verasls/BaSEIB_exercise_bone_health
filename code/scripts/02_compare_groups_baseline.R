# Load packages and functions ---------------------------------------------

library(tidyverse)
library(gmodels)

# Load and prepare data ---------------------------------------------------

source("code/scripts/01_tidy_data.R")
baseline_inc_df <- filter(baseline_df, exclude == "No")
baseline_exc_df <- filter(baseline_df, exclude == "Yes")
control_df <- filter(baseline_df, group == "Control" & exclude == "No")
exercise_df <- filter(baseline_df, group == "Exercise" & exclude == "No")
exercise_under_50_df <- filter(baseline_df, attend_cat == "Under 50% training attendance" & exclude == "No")
exercise_over_50_df <- filter(baseline_df, attend_cat == "Over 50% training attendance" & exclude == "No")

# Compare 2 groups --------------------------------------------------------

# ** Continuous variables -------------------------------------------------

# Normality tests control group
shapiro.test(control_df$age)
shapiro.test(control_df$height)
shapiro.test(control_df$body_mass)
shapiro.test(control_df$BMI)
shapiro.test(control_df$waist_circunference)
shapiro.test(control_df$hip_circunference)
shapiro.test(control_df$waist_hip_ratio)

# Normality tests exercise group
shapiro.test(exercise_df$age)
shapiro.test(exercise_df$height)
shapiro.test(exercise_df$body_mass)
shapiro.test(exercise_df$BMI)
shapiro.test(exercise_df$waist_circunference)
shapiro.test(exercise_df$hip_circunference)
shapiro.test(exercise_df$waist_hip_ratio)

# Independent samples t-test
t.test(age ~ group, data = baseline_df, paired = FALSE)
t.test(height ~ group, data = baseline_df, paired = FALSE)
t.test(body_mass ~ group, data = baseline_df, paired = FALSE)
t.test(BMI ~ group, data = baseline_df, paired = FALSE)
t.test(waist_circunference ~ group, data = baseline_df, paired = FALSE)
t.test(hip_circunference ~ group, data = baseline_df, paired = FALSE)
t.test(waist_hip_ratio ~ group, data = baseline_df, paired = FALSE)

# ** Categorical variables ------------------------------------------------

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

# Compare 3 groups --------------------------------------------------------

# ** Continuous variables -------------------------------------------------

# Normality tests exercise group under 50% training attendance
shapiro.test(exercise_under_50_df$age)
shapiro.test(exercise_under_50_df$height)
shapiro.test(exercise_under_50_df$body_mass)
shapiro.test(exercise_under_50_df$BMI)
shapiro.test(exercise_under_50_df$waist_circunference)
shapiro.test(exercise_under_50_df$hip_circunference)
shapiro.test(exercise_under_50_df$waist_hip_ratio)

# Normality tests exercise group over 50% training attendance
shapiro.test(exercise_over_50_df$age)
shapiro.test(exercise_over_50_df$height)
shapiro.test(exercise_over_50_df$body_mass)
shapiro.test(exercise_over_50_df$BMI)
shapiro.test(exercise_over_50_df$waist_circunference)
shapiro.test(exercise_over_50_df$hip_circunference)
shapiro.test(exercise_over_50_df$waist_hip_ratio)

# ANOVA
age_model <- aov(age ~ attend_cat, data = baseline_df)
height_model <- aov(height ~ attend_cat, data = baseline_df)
body_mass_model <- aov(body_mass ~ attend_cat, data = baseline_df)
BMI_model <- aov(BMI ~ attend_cat, data = baseline_df)
waist_circunference_model <- aov(waist_circunference ~ attend_cat, data = baseline_df)
hip_circunference_model <- aov(hip_circunference ~ attend_cat, data = baseline_df)
waist_hip_ratio_model <- aov(waist_hip_ratio ~ attend_cat, data = baseline_df)

summary(age_model)
summary(height_model)
summary(body_mass_model)
summary(BMI_model)
summary(waist_circunference_model)
summary(hip_circunference_model)
summary(waist_hip_ratio_model)

pairwise.t.test(baseline_df$BMI, baseline_df$attend_cat, p.adjust.method = "bonferroni")

# ** Categorical variables ------------------------------------------------

CrossTable(
  baseline_df$sex, baseline_df$attend_cat, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$surgery, baseline_df$attend_cat, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$menopause, baseline_df$attend_cat, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$diabetes, baseline_df$attend_cat, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$thiazides, baseline_df$attend_cat, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

CrossTable(
  baseline_df$smoker, baseline_df$attend_cat, 
  fisher = TRUE, chisq = TRUE, format = "SPSS"
)

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

# Descriptives ------------------------------------------------------------

# Control group
control_descriptives <- summarise(
  .data = control_df,
  age_mean       = round(mean(age), digits = 1),
  age_sd         = round(sd(age), digits = 1),
  body_mass_mean = round(mean(body_mass), digits = 1),
  body_mass_sd   = round(sd(body_mass), digits = 1),
  height_mean    = round(mean(height), digits = 2),
  height_sd      = round(sd(height), digits = 2),
  BMI_mean       = round(mean(BMI), digits = 1),
  BMI_sd         = round(sd(BMI), digits = 1),
  waist_mean     = round(mean(waist_circunference), digits = 1),
  waist_sd       = round(sd(waist_circunference), digits = 1),
  hip_mean       = round(mean(hip_circunference), digits = 1),
  hip_sd         = round(sd(hip_circunference), digits = 1),
  ratio_mean     = round(mean(waist_hip_ratio), digits = 2),
  ratio_sd       = round(sd(waist_hip_ratio), digits = 2)
)
control_sex       <- table(control_df$sex)
control_surgery   <- table(control_df$surgery)
control_menopause <- table(control_df$menopause)
control_diabetes  <- table(control_df$diabetes)
control_thiazides <- table(control_df$thiazides)
control_smoker    <- table(control_df$smoker)

# Exercise group
exercise_descriptives <- summarise(
  .data = exercise_df,
  age_mean       = round(mean(age), digits = 1),
  age_sd         = round(sd(age), digits = 1),
  body_mass_mean = round(mean(body_mass), digits = 1),
  body_mass_sd   = round(sd(body_mass), digits = 1),
  height_mean    = round(mean(height), digits = 2),
  height_sd      = round(sd(height), digits = 2),
  BMI_mean       = round(mean(BMI), digits = 1),
  BMI_sd         = round(sd(BMI), digits = 1),
  waist_mean     = round(mean(waist_circunference), digits = 1),
  waist_sd       = round(sd(waist_circunference), digits = 1),
  hip_mean       = round(mean(hip_circunference), digits = 1),
  hip_sd         = round(sd(hip_circunference), digits = 1),
  ratio_mean     = round(mean(waist_hip_ratio), digits = 2),
  ratio_sd       = round(sd(waist_hip_ratio), digits = 2)
)
exercise_sex       <- table(exercise_df$sex)
exercise_surgery   <- table(exercise_df$surgery)
exercise_menopause <- table(exercise_df$menopause)
exercise_diabetes  <- table(exercise_df$diabetes)
exercise_thiazides <- table(exercise_df$thiazides)
exercise_smoker    <- table(exercise_df$smoker)

# Included subjects
included_descriptives <- summarise(
  .data = baseline_inc_df,
  age_mean       = round(mean(age, na.rm = TRUE), digits = 1),
  age_sd         = round(sd(age, na.rm = TRUE), digits = 1),
  body_mass_mean = round(mean(body_mass, na.rm = TRUE), digits = 1),
  body_mass_sd   = round(sd(body_mass, na.rm = TRUE), digits = 1),
  height_mean    = round(mean(height, na.rm = TRUE), digits = 2),
  height_sd      = round(sd(height, na.rm = TRUE), digits = 2),
  BMI_mean       = round(mean(BMI, na.rm = TRUE), digits = 1),
  BMI_sd         = round(sd(BMI, na.rm = TRUE), digits = 1),
  waist_mean     = round(mean(waist_circunference, na.rm = TRUE), digits = 1),
  waist_sd       = round(sd(waist_circunference, na.rm = TRUE), digits = 1),
  hip_mean       = round(mean(hip_circunference, na.rm = TRUE), digits = 1),
  hip_sd         = round(sd(hip_circunference, na.rm = TRUE), digits = 1),
  ratio_mean     = round(mean(waist_hip_ratio, na.rm = TRUE), digits = 2),
  ratio_sd       = round(sd(waist_hip_ratio, na.rm = TRUE), digits = 2)
)
included_sex       <- table(baseline_inc_df$sex)
included_surgery   <- table(baseline_inc_df$surgery)
included_menopause <- table(baseline_inc_df$menopause)
included_diabetes  <- table(baseline_inc_df$diabetes)
included_thiazides <- table(baseline_inc_df$thiazides)
included_smoker    <- table(baseline_inc_df$smoker)

# Excluded subjects
excluded_descriptives <- summarise(
  .data = baseline_exc_df,
  age_mean       = round(mean(age, na.rm = TRUE), digits = 1),
  age_sd         = round(sd(age, na.rm = TRUE), digits = 1),
  body_mass_mean = round(mean(body_mass, na.rm = TRUE), digits = 1),
  body_mass_sd   = round(sd(body_mass, na.rm = TRUE), digits = 1),
  height_mean    = round(mean(height, na.rm = TRUE), digits = 2),
  height_sd      = round(sd(height, na.rm = TRUE), digits = 2),
  BMI_mean       = round(mean(BMI, na.rm = TRUE), digits = 1),
  BMI_sd         = round(sd(BMI, na.rm = TRUE), digits = 1),
  waist_mean     = round(mean(waist_circunference, na.rm = TRUE), digits = 1),
  waist_sd       = round(sd(waist_circunference, na.rm = TRUE), digits = 1),
  hip_mean       = round(mean(hip_circunference, na.rm = TRUE), digits = 1),
  hip_sd         = round(sd(hip_circunference, na.rm = TRUE), digits = 1),
  ratio_mean     = round(mean(waist_hip_ratio, na.rm = TRUE), digits = 2),
  ratio_sd       = round(sd(waist_hip_ratio, na.rm = TRUE), digits = 2)
)
excluded_sex       <- table(baseline_exc_df$sex)
excluded_surgery   <- table(baseline_exc_df$surgery)
excluded_menopause <- table(baseline_exc_df$menopause)
excluded_diabetes  <- table(baseline_exc_df$diabetes)
excluded_thiazides <- table(baseline_exc_df$thiazides)
excluded_smoker    <- table(baseline_exc_df$smoker)