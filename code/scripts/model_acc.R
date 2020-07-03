# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv")) %>% 
  dplyr::select(
    subj, time, group, surgery, age, 
    menopause, diabetes, thiazides, smoker
  )
acc <- read_csv(
  here("data", "acc.csv"),
  col_types = cols(
    time = col_factor(1:4),
    group = col_factor(c("Control", "Exercise"))
  )
) %>% 
  left_join(df, by = c("subj", "time", "group"))

# Set contrasts of variable group to sum
contrasts(acc$group) <- matrix(rev(contr.sum(2)), ncol = 1)
# Set contrasts of variable time to polynomial
contrasts(acc$time) <- contr.poly(4)

# Model -------------------------------------------------------------------

peaks_LMM <- lmer(
  formula = above_thrsh ~ 1 + group + time + group:time + above_thrsh_adjust + BMI_adjust + 
    surgery + age + menopause + diabetes + thiazides + smoker + (1 | subj),
  data = acc
)

# R-squared
rsquared(peaks_LMM)

# Fixed effects test
anova(peaks_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(peaks_LMM)

# Estimated marginal means for group
group_peaks_emm <- emmeans(peaks_LMM, ~ group)

# Estimated marginal means for time
time_peaks_emm <- emmeans(peaks_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_peaks_emm  <- emmeans(peaks_LMM, ~ group:time)
# Save into a data frame to build the plots
interaction_peaks_emm_df <- as.data.frame(interaction_peaks_emm)
write_csv(interaction_peaks_emm_df, here("output", "interaction_peaks_emm.csv"))

# Post hoc
pairs(interaction_peaks_emm, adjust = "none")
