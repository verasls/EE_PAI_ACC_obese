get_kcal <- function(df, ID_num) {
  # Computes kilocalory values for a single ID based on VO2 and VCO2
  #
  # Args:
  #   df: a data frame containing data for computation   
  #   ID_num: subject ID
  #
  # Returns:
  #   A data frame adding a colunm with computed kilocalory 
  #   values to the imput data frame
  
  require(tidyverse)
  
  ID_df <- filter(df, ID == ID_num)
  
  ID_df$kcal <- NA
  for (i in 1:nrow(ID_df)) {
    ID_df$kcal[i] <- (3.941 * (ID_df$V.O2[i] / 1000)) + (1.106 * (ID_df$V.CO2[i] / 1000))
  }
  return(ID_df)
}