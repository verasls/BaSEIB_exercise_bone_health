mean_difference <- function(data, emm) {
  # Compute the difference and 95% confidence interval between two group means
  #
  # Args:
  #   data: A data frame
  #   emm: The estimated marginal means as a data frame
  #
  # Returns:
  #   A data frame with the mean difference, lower and upper confidence interval
  #   values
  
  # Get sample sizes
  n <- data %>% 
    filter(time == 4) %>% 
    select(subj, group, ends_with("_BMD")) %>% 
    na.omit() %>% 
    group_by(group) %>% 
    summarise(n = n(), .groups = "drop")
  
  n1 <- as.double(n[which(n$group == "Exercise"), 2])
  n2 <- as.double(n[which(n$group == "Control"), 2])
  
  # Get mean difference
  emm1 <- emm[which(emm$group == "Exercise" & emm$time == 4), 3]
  emm2 <- emm[which(emm$group == "Control" & emm$time == 4), 3]
  mean_diff <- emm1 - emm2
  
  # Get standard deviations
  sd1 <- emm[which(emm$group == "Exercise" & emm$time == 4), 4] * sqrt(n1)
  sd2 <- emm[which(emm$group == "Control" & emm$time == 4), 4] * sqrt(n1)
  
  # Get pooled standard deviation
  sd_pooled <- sqrt(((n1 - 1) * sd1 + (n2 - 1) * sd2) / (n1 + n2 - 2)) 
  
  # Get pooled standard error
  se_pooled <- sd_pooled * sqrt((1 / n1) + (1 / n2))
  
  # Get confidence interval
  lower_ci <- mean_diff - se_pooled * qt(0.975, df = n1 + n2 - 2)
  upper_ci <- mean_diff + se_pooled * qt(0.975, df = n1 + n2 - 2)
  
  # Return the mean difference and confidence interval
  data.frame(mean_diff, lower_ci, upper_ci)
}
