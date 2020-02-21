classify_attendance <- function(data, cutoff) {
  # Classify the training sessions attendance rate into groups according to a
  # selected cutoff rate
  #
  # Args:
  #   data: a data frame that MUST contain:
  #     - a variable named group, classifying groups in Control and Exercise
  #     - a variable named attend_2nd_3rd, with the subjects attendance rate
  #     between the 2nd and 3rd evaluations
  #     - a variable named attend_2nd_4th, with the subjects attendance rate
  #     between the 2nd and 4th evaluations
  #   cutoff: an interger with the desired cutoff value for attendance rate
  #
  # Returns:
  #   The model syntax and prints it as formated text
  
  require(stringr)
  require(dplyr)
  
  # If overall attendance rate is NA, get values from 2nd to 3rd attendance rate
  data$attendance <- NA
  for (i in 1:nrow(data)) {
    if (is.na(data$attend_2nd_4th[i])) {
      data$attendance[i] <- data$attend_2nd_3rd[i]
    } else {
      data$attendance[i] <- data$attend_2nd_4th[i]
    }
  }
  
  # Create the attendance rate category
  data$attend_cat <- NA
  for (i in 1:nrow(data)) {
    if (is.na(data$group[i])) {
      data$attend_cat[i] <- NA
    } else {
      if (is.na(data$group[i]) == FALSE) {
        if (data$group[i] == "Control") {
          data$attend_cat[i] <- 0
        } else {
          if (data$group[i] == "Exercise") {
            if (is.na(data$attendance[i])) {
              data$attend_cat[i] <- 1
            } else {
              if (data$attendance[i] < cutoff) {
                data$attend_cat[i] <- 1
              } else {
                if (data$attendance[i] >= cutoff) {
                  data$attend_cat[i] <- 2
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Recode attend_cat into a factor
  data$attend_cat <- as.factor(data$attend_cat)
  # Recode its levels
  under <- str_c(
    "Under ", as.character(cutoff), "% training attendance"
  )
  over <- str_c(
    "Over ", as.character(cutoff), "% training attendance"
  )
  data$attend_cat <- recode(
    data$attend_cat,
    "0" = "Control",
    "1" = under,
    "2" = over
  )
  
  # Drop some variables
  data <- data[, c(1, 2, 9, 6, 7)]
  
  return(data)
}