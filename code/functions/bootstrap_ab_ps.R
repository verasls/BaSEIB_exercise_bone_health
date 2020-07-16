bootstrap_ab_ps <- function(data, outcome, indirect_effect, times = 5000) {
  # Compute the bootstrapped partially standardized indirect effect and
  # confidence intervals
  #
  # Args:
  #   data: A data frame
  #   outcome: A character string with the name of the column containing the
  #      outcome variable
  #   indirect_effect: A numeric scalar with the indirect effect coefficient
  #   times: A numeric scalar with the number of bootstrap repetitions
  #
  # Returns:
  #   A data frame with the partially standardized indirect effect and
  # intervals
  
  require(rsample)
  require(magrittr)
  require(dplyr)
  require(purrr)
  
  resample <- rsample::bootstraps(data, times = times)
  resample$sd <- map_dbl(
    resample$splits,
    function(x) {
      outcome <- as.data.frame(x)[[outcome]]
      sd(outcome, na.rm = TRUE)
    }
  )
  
  bootstrap_ab_ps <- map_dbl(resample$sd, function(x) {indirect_effect / x})
  
  ab_ps <- mean(bootstrap_ab_ps)
  lower_ci <- ab_ps - 1.96 * sd(bootstrap_ab_ps)
  upper_ci <- ab_ps + 1.96 * sd(bootstrap_ab_ps)
  
  data.frame(ab_ps, lower_ci, upper_ci)
}
