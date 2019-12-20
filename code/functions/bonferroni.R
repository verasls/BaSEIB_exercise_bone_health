bonferroni <- function(data, comparisons) {
  # Apply the Bonferroni correction on the pairwise tests p value
  #
  # Args:
  #   data: A data frame containing the pairwise comparisons results
  #   comparisons: An interger with the number of comparisons to be considered
  #   in the corrections
  #
  # Returns:
  #   A data frame with the pairwise comparisons with their p values adjusted
  #   with the Bonferroni correction
  
  data <- as.data.frame(data)
  
  data$p.value <- round(data$p.value, digits = 4)
  data$p.bonferroni <- NA
  for (i in 1:nrow(data)) {
    data$p.bonferroni[i] <- data[i, 6] * comparisons
  }
  
  for (i in 1:nrow(data)) {
    if (data$p.bonferroni[i] > 1) {
      data$p.bonferroni[i] <- 1
    }
  }
  return(data[, c(1, 7)])
}