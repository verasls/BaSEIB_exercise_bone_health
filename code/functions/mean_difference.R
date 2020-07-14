mean_difference <- function(data) {
  # Compute the difference and 95% confidence interval between two group means
  #
  # Args:
  #   data: A emmGrid object with the contrasts
  #
  # Returns:
  #   A data frame with the mean difference, lower and upper confidence
  # interval and p value
  
  p_value <- round(as.data.frame(data)[28, 6], 3)
  mean_diff <- round(- as.data.frame(data)[28, 2], 3)
  se <- round(as.data.frame(data)[28, 3], 3)
  lower_ci <- round(mean_diff - 1.96 * se, 3)
  upper_ci <- round(mean_diff + 1.96 * se, 3)
  data.frame(mean_diff, lower_ci, upper_ci, p_value)
}

mean_difference_delta <- function(data) {
  p_value <- round(as.data.frame(data)[65, 6] * 3, 3)
  mean_diff <- round(- as.data.frame(data)[65, 2], 1)
  se <- round(as.data.frame(data)[65, 3], 1)
  lower_ci <- round(mean_diff - 1.96 * se, 1)
  upper_ci <- round(mean_diff + 1.96 * se, 1)
  data.frame(mean_diff, lower_ci, upper_ci, p_value)
}

mean_difference_delta_2 <- function(data) {
  p_value <- round(as.data.frame(data)[66, 6] * 3, 3)
  mean_diff <- round(- as.data.frame(data)[66, 2], 1)
  se <- round(as.data.frame(data)[66, 3], 1)
  lower_ci <- round(mean_diff - 1.96 * se, 1)
  upper_ci <- round(mean_diff + 1.96 * se, 1)
  data.frame(mean_diff, lower_ci, upper_ci, p_value)
}