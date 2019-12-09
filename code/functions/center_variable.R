center_variable <- function(data, variable) {
  # Center a variable based on the grand mean
  #
  # Args:
  #   data: A data frame containing the variable to be centered and a Group
  #   factor
  #   variable: The name of the variable to be centered as character string
  #
  # Returns:
  #   The original data frame plus a column in the last position with the
  #   centered variable
  
  data  <- as.data.frame(data)
  n_col <- ncol(data)
  idx   <- which(names(data) == variable)
  
  variable_mean <- mean(data[, idx], na.rm = TRUE)
  data$centered <- NA
  for (i in 1:nrow(data)) {
    data$centered[i] <- data[i, variable] - variable_mean
  }
  data <- as_tibble(data)
  
  # Rename centered variable
  names(data)[ncol(data)] <- str_c(variable, "_centered", sep = "")
  
  # Arrange dataframe before returning
  data <- arrange(data, subj, time)
  
  return(data)
}