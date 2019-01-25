cross_validate_mixed_model <- function(df, ID_num) {
  # Cross validates the model, separating sample in:
  #  training dataset: used to build the model
  #  testing dataset: used to predict the model
  #
  # Args:
  #   df: data frame containing data used to build and test the models
  #   ID: subject ID number to be assigned to testing dataset
  #   fix_eff, rand_eff: these mixed model parameters need to be assigned
  # outside of function call
  #
  # Returns:
  #   A data frame containing testing dataset predictions
  
  require(tidyverse)
  require(nlme)
  
  df <- na.omit(df)
  
  training <- filter(df, ID != ID_num)
  testing  <- filter(df, ID == ID_num)
  
  cv_MM <- lme(
    fixed = fix_eff,
    random = rand_eff,
    method = "ML",
    correlation = corAR1(),
    data = training
  )
  
  testing$predicted <- predict(object = cv_MM, newdata = testing, level = 0)
  names(testing)[ncol(testing)] <- paste(fix_eff[[2]], "_predicted", sep = "")
  
  return(testing)
}