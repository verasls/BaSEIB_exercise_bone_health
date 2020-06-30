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
  
  # Get mediator
  mediator <- paste0(
    mediator, " ~ a*", predictor, " + ", covariate, "\n",
    outcome, " ~ c*", predictor, " + ", covariate, " + b*", mediator
  )
  # Combine parts into the model syntax
  model <- paste0(
    "# Mediator\n",
    mediator, "\n\n",
    "# Direct effect\n",
    "direct := c", "\n\n",
    "# Indirect effect (a*b)\n",
    "indirect := a*b", "\n\n",
    "# Total effect\n",
    "total := c + (a*b)"
  )
  
  cat(model)
  
  return(model)
}