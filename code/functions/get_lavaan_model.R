get_lavaan_model <- function(outcome, predictor, mediator, covariate) {
  # Create the lavaan package model syntax for a mediation analysis with a
  # covariate
  #
  # Args:
  #   outcome: a character string with the outcome variable name
  #   predictor: a character string with the predictor variable name
  #   mediator: a character string with the mediator variable name
  #   covariate: a character string with the covariate variable name
  #
  # Returns:
  #   The model syntax and prints it as formated text
  
  require(stringr)
  
  # Get direct effect model
  direct <- str_c(outcome, " ~ c*", predictor, " + ", covariate)
  # Get mediator model
  mediator <- str_c(
    mediator, " ~ a*", predictor, " + ", covariate, "\n",
    outcome, " ~ b*", mediator
  )
  # Get indirect effect
  indirect <- "ab := a*b"
  # Get total effect
  total <- "total := c + (a*b)"
  
  # Combine parts into the model
  model <- str_c(
    "# Direct effect\n",
    direct, "\n\n",
    "# Mediator\n",
    mediator, "\n\n",
    "# Indirect effect (a*b)\n",
    indirect, "\n\n",
    "# Total effect\n",
    total
  )
  
  cat(model)
  
  return(model)
}