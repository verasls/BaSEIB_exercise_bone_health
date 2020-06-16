# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(emmeans)
source(here("code", "functions", "read_data.R"))
source(here("code", "functions", "center_variable.R"))
source(here("code", "functions", "bonferroni.R"))
source(here("code", "functions", "repeat_baseline_values.R"))

# Load and prepare data ---------------------------------------------------

df <- read_data(here("data", "df.csv")) %>% select(subj, time, group, BMI_adjust)

acc <- read_csv(here("data", "raw", "summary_GRF_data.csv")) %>%
  dplyr::select(
    subj = ID, time = eval, duration, n_days, n_peaks, above_thrsh = n_g_4.9
  ) %>% 
  mutate(
    time = recode(
      as.factor(time),
      "1st" = "1", "2nd" = "2", "3rd" = "3", "4th" = "4"
    ),
    above_thrsh_adjust = repeat_baseline_values(
      acc, above_thrsh, subj, time, 1
    )
  ) %>% 
  left_join(df, by = c("subj", "time")) %>% 
  dplyr::select(subj, time, group, everything()) %>% 
  arrange(subj)

# Model -------------------------------------------------------------------

m <- lmer(
  formula = above_thrsh ~ 1 + group + time + group:time + above_thrsh_adjust + BMI_adjust + (1 | subj),
  data = acc
)

anova(m, type = 3, test = "F")

interaction_emm  <- emmeans(m, ~ group:time)
interaction_emm_df <- as.data.frame(interaction_emm)

pairs(interaction_emm, adjust = "none")

# Plot --------------------------------------------------------------------

ggplot(data = interaction_emm_df) +
  geom_point(
    aes(x = time, y = emmean, shape = group, colour = group),
    position = position_dodge(0.2), size = 4
  ) +
  geom_line(
    aes(x = time, y = emmean, linetype = group, group = group, colour = group),
    position = position_dodge(0.2), size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, group = group, colour = group),
    position = position_dodge(0.2), size = 1, width = 0.1
  ) +
  labs(
    x = "time",
    y = "n peaks > 4.9 g"
  )