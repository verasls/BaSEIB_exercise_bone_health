# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))
source(here("code", "functions", "mean_difference.R"))

# Load and prepare data ---------------------------------------------------

acc <- read_csv(
  here("data", "acc.csv"),
  col_types = cols(
    time = col_factor(1:4),
    group = col_factor(c("Control", "Exercise"))
  )
) %>% 
  select(subj, time, group, n_peaks = above_thrsh)

df <- read_data(here("data", "df.csv")) %>% 
  left_join(acc, by = c("subj", "time", "group"))

# Set contrasts of variable time to polynomial
contrasts(df$time) <- contr.poly(4)

# Transform variables
df$LS_BMD <- log(df$LS_BMD + 1)
df$TR_BMD <- log(df$TR_BMD + 1)
df$FN_BMD <- log(df$FN_BMD + 1)
df$whole_body_lean_mass <- log(df$whole_body_lean_mass + 1)
df$n_peaks <- log(df$n_peaks + 1)

# ** LS_BMD ---------------------------------------------------------------

LS_lean_mass <- lmer(
  formula = LS_BMD ~ whole_body_lean_mass + (1 | subj) + (1 | time), 
  data = df
)

LS_n_peaks <- lmer(
  formula = LS_BMD ~ n_peaks + (1 | subj) + (1 | time), 
  data = df
)

# Random components and fixed effects parameters estimates
summary(LS_lean_mass)
summary(LS_n_peaks)

# ** TR_BMD ---------------------------------------------------------------

TR_lean_mass <- lmer(
  formula = TR_BMD ~ whole_body_lean_mass + (1 | subj) + (1 | time), 
  data = df
)

TR_n_peaks <- lmer(
  formula = TR_BMD ~ n_peaks + (1 | subj) + (1 | time), 
  data = df
)

# Random components and fixed effects parameters estimates
summary(TR_lean_mass)
summary(TR_n_peaks)

# ** FN_BMD ---------------------------------------------------------------

FN_lean_mass <- lmer(
  formula = FN_BMD ~ whole_body_lean_mass + (1 | subj) + (1 | time), 
  data = df
)

FN_n_peaks <- lmer(
  formula = FN_BMD ~ n_peaks + (1 | subj) + (1 | time), 
  data = df
)

# Random components and fixed effects parameters estimates
summary(FN_lean_mass)
summary(FN_n_peaks)