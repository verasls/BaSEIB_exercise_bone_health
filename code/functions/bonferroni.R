bonferroni <- function(data, comparisons) {
  
  data <- as.data.frame(data)
  
  data$p.value <- round(data$p.value, 4)
  data$p.bonferroni <- NA
  for (i in 1:nrow(data)) {
    data$p.bonferroni[i] <- data[i, 6] * comparisons
  }
  
  for (i in 1:nrow(data)) {
    if (data$p.bonferroni[i] > 1) {
      data$p.bonferroni[i] <- 1
    }
  }
  return(data)
}