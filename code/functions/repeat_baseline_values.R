repeat_baseline_values <- function(data, 
                                   var, 
                                   id, 
                                   time, 
                                   baseline_level, 
                                   repeat_NA = TRUE) {
  # Returns a vector with the baseline value repeated for every time value of
  # each id.
  #
  # Args:
  #   data: A data frame.
  #   var: The name of the column with the values to be repeated.
  #   id: The name of the column that identifies each subject.
  #   time: The name of the column with the time values.
  #   baseline_level: The value of time corresponding to baseline
  #   repeat_NA: A logical indicating whether or not NA values in the var will
  #   correspond to NA values in the return vector. Defaults to TRUE.

  require(rlang)
  require(dplyr)
  require(purrr)
  
  var_col_name <- as_string(ensym(var))
  id_col_name <- as_string(ensym(id))
  time_col_name <- as_string(ensym(time))
  data_name <- as_string(ensym(data))
  
  lookup <- filter(data, {{ time }} == baseline_level)
  lookup <- select(lookup, {{ id }}, baseline = {{ var }})
  
  df <- left_join(data, lookup, by = as_string(ensym(id)))
  if (repeat_NA == TRUE) {
    map2_dbl(
      df[[var_col_name]], 
      df[["baseline"]],
      ~ ifelse(is.na(.x), NA, .y)
    )
  } else {
    df[["baseline"]]
  }
}