# Load packages and functions ---------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source("code/functions/center_variable.R")
source("code/functions/bonferroni.R")

# Load and prepare data ---------------------------------------------------

data <- read_csv("data/database.csv")
# Code the group and time variables as factors
data$attend_cat <- as.factor(data$attend_cat)
data$time <- as.factor(data$time)
# Set contrasts of variable group to deviation
contrasts(data$attend_cat) <- matrix(rev(contr.sum(3)), ncol = 2)
# Set contrasts of variable time to polynomial
contrasts(data$time) <- contr.poly(4)

# Select variables
LS_data <- data %>% select(subj, time, attend_cat, delta_LS_BMD)
TH_data <- data %>% select(subj, time, attend_cat, delta_TH_BMD)
FN_data <- data %>% select(subj, time, attend_cat, delta_FN_BMD)
TR_data <- data %>% select(subj, time, attend_cat, delta_TR_BMD)

# Build models ------------------------------------------------------------

# ** delta_LS_BMD ---------------------------------------------------------

delta_LS_LMM <- lmer(
  formula = delta_LS_BMD ~ 1 + attend_cat + time + attend_cat:time + (1 | subj),
  data = LS_data
)

# R-squared
rsquared(delta_LS_LMM)

# Fixed effects test
anova(delta_LS_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_LS_LMM)

# Estimated marginal means for group
group_delta_LS_emm <- emmeans(delta_LS_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_LS_emm <- emmeans(delta_LS_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_LS_emm  <- emmeans(delta_LS_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_LS_none <- pairs(interaction_delta_LS_emm, adjust = "none")
ph_delta_LS_bonf <- bonferroni(as.data.frame(ph_delta_LS_none), 16)

# ** delta_TH_BMD ---------------------------------------------------------

delta_TH_LMM <- lmer(
  formula = delta_TH_BMD ~ 1 + attend_cat + time + attend_cat:time + (1 | subj),
  data = TH_data
)

# R-squared
rsquared(delta_TH_LMM)

# Fixed effects test
anova(delta_TH_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TH_LMM)

# Estimated marginal means for group
group_delta_TH_emm <- emmeans(delta_TH_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TH_emm <- emmeans(delta_TH_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TH_emm  <- emmeans(delta_TH_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_TH_none <- pairs(interaction_delta_TH_emm, adjust = "none")
ph_delta_TH_bonf <- bonferroni(as.data.frame(ph_delta_TH_none), 16)

# ** delta_FN_BMD ---------------------------------------------------------

delta_FN_LMM <- lmer(
  formula = delta_FN_BMD ~ 1 + attend_cat + time + attend_cat:time + (1 | subj),
  data = FN_data
)

# R-squared
rsquared(delta_FN_LMM)

# Fixed effects test
anova(delta_FN_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_FN_LMM)

# Estimated marginal means for group
group_delta_FN_emm <- emmeans(delta_FN_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_FN_emm <- emmeans(delta_FN_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_FN_emm  <- emmeans(delta_FN_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_FN_none <- pairs(interaction_delta_FN_emm, adjust = "none")
ph_delta_FN_bonf <- bonferroni(as.data.frame(ph_delta_FN_none), 16)

# ** delta_TR_BMD ---------------------------------------------------------

delta_TR_LMM <- lmer(
  formula = delta_TR_BMD ~ 1 + attend_cat + time + attend_cat:time + (1 | subj),
  data = TR_data
)

# R-squared
rsquared(delta_TR_LMM)

# Fixed effects test
anova(delta_TR_LMM, type = 3, test = "F")

# Random components and fixed effects parameters estimates
summary(delta_TR_LMM)

# Estimated marginal means for group
group_delta_TR_emm <- emmeans(delta_TR_LMM, ~ attend_cat)

# Estimated marginal means for time
time_delta_TR_emm <- emmeans(delta_TR_LMM, ~ time)

# Estimated marginal means for group x time interaction
interaction_delta_TR_emm  <- emmeans(delta_TR_LMM, ~ attend_cat:time)

# Post hocs
ph_delta_TR_none <- pairs(interaction_delta_TR_emm, adjust = "none")
ph_delta_TR_bonf <- bonferroni(as.data.frame(ph_delta_TR_none), 16)

# Plot models -------------------------------------------------------------

dodge <- position_dodge(0.4)

# Put the interaction emmeans into a data frame
interaction_delta_LS_emm_df <- interaction_delta_LS_emm %>% 
  as.data.frame() %>% 
  filter(time == 4) %>% 
  mutate(region = "Lumbar spine")
interaction_delta_FN_emm_df <- interaction_delta_FN_emm %>% 
  as.data.frame() %>% 
  filter(time == 4) %>% 
  mutate(region = "Femoral neck")
interaction_delta_TH_emm_df <- interaction_delta_TH_emm %>% 
  as.data.frame() %>% 
  filter(time == 4) %>% 
  mutate(region = "Total hip")
interaction_delta_TR_emm_df <- interaction_delta_TR_emm %>% 
  as.data.frame() %>% 
  filter(time == 4) %>% 
  mutate(region = "One-third radius")

# Combine data frames and recode variables
interaction_df <- interaction_delta_LS_emm_df %>% 
  rbind(
    interaction_delta_FN_emm_df,
    interaction_delta_TH_emm_df,
    interaction_delta_TR_emm_df
  ) %>% 
  select(
    group = attend_cat, region, time, emmean, lower.CL, upper.CL
  )
interaction_df$region <- factor(interaction_df$region, levels = c("Total hip", "Femoral neck", "Lumbar spine", "One-third radius"))
interaction_df$group <- dplyr::recode(
  interaction_df$group,
  "0" = "Control",
  "1" = "Under 50% training attendance",
  "2" = "Over 50% training attendance",
)

# Plot
delta_plot <- ggplot(data = interaction_df) +
  geom_point(
    aes(x = region, y = emmean, colour = group),
    position = dodge, size = 2
  ) +
  geom_errorbar(
    aes(x = region, ymin = lower.CL, ymax = upper.CL, colour = group),
    position = dodge, size = 1, width = 0.4
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#D55E00")) +
  labs(
    x = "",
    y = "% bone mineral density change",
    colour = "Group"
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(breaks = seq(-10, 4, 2)) +
  theme_classic()