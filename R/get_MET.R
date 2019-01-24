get_MET <- function(df, ID_num) {
  # Computes MET values for a single ID based on first step VO2/kg value
  #
  # Args:
  #   df: a data frame containing data for computation   
  #   ID_num: subject ID
  #
  # Returns:
  #   A data frame adding a colunm with computed MET values to the 
  # imput data frame
  
  require(tidyverse)
  
  ID_df <- filter(df, ID == ID_num)
  
  ID_df$MET <- NA
  for (i in 1:nrow(ID_df)) {
    ID_df$MET[i] <- ID_df$VO2.kg[i] / ID_df$VO2.kg[1]
  }
  return(ID_df)
}