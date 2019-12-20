adjust_variable <- function(data, variable) {
  # Create a variable for baseline differences adjustments
  #
  # Args:
  #   data: A data frame containing the variable to be ajusted
  #   variable: The name of the variable to be adjusted as character string
  #
  # Returns:
  #   The original data frame plus a column in the last position with the
  #   adjusted variable
  
  require(tidyverse)
  
  data <- as.data.frame(data)
  idx  <- which(names(data) == variable)
  ID   <- unique(data$subj)
  
  data$adjust <- NA
  for (i in 1:length(ID)) {
    ID_1st_row <- min(which(data$subj == ID[i]))
    if (is.na(data[ID_1st_row, idx])) {
      data$adjust[ID_1st_row:(ID_1st_row + 3)] <- NA
    } else {
      data$adjust[ID_1st_row:(ID_1st_row + 3)] <- data[ID_1st_row, idx]
    }
  }
  
  names(data)[ncol(data)] <- str_c(variable, "_adjust", sep = "")
  
  return(as_tibble(data))
}