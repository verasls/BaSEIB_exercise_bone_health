group_center_variable <- function(data, variable) {
  # Center a variable based on the group means
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
  
  # Control group
  CG <- data %>% filter(group == "Control")
  CG_mean <- mean(CG[, idx], na.rm = TRUE)
  CG_centered <- NA
  for (i in 1:nrow(CG)) {
    CG_centered[i] <- CG[i, variable] - CG_mean 
  }
  CG <- cbind(CG, CG_centered)
  
  # Exercise group
  EG <- data %>% filter(group == "Exercise")
  EG_mean <- mean(EG[, idx], na.rm = TRUE)
  EG_centered <- NA
  for (i in 1:nrow(EG)) {
    EG_centered[i] <- EG[i, variable] - EG_mean 
  }
  EG <- cbind(EG, EG_centered)
  
  # NA group
  NG <- data %>% filter(is.na(group))
  NG$NG_centered <- NA
  
  # Join data frames
  data <- plyr::join_all(
    list(CG, EG, NG),
    type = "full",
    by = names(data)
  )
  data <- as_tibble(data)
  
  # Keep only one column for the centered variable
  data$centered <- NA
  for (i in 1:nrow(data)) {
    if (is.na(data$EG_centered[i]) & is.na(data$NG_centered[i])) {
      data$centered[i] <- data$CG_centered[i]
    } else {
      if (is.na(data$CG_centered[i]) & is.na(data$NG_centered[i])) {
        data$centered[i] <- data$EG_centered[i]
      } else {
        if (is.na(data$EG_centered[i]) & is.na(data$CG_centered[i])) {
          data$centered[i] <- data$NG_centered
        }
      }
    }
  }
  data <- data[, c(1:n_col, ncol(data))]
  
  # Rename centered variable
  names(data)[ncol(data)] <- str_c(variable, "_group_centered", sep = "")
  
  # Arrange dataframe before returning
  data <- arrange(data, subj, time)
  
  return(data)
}