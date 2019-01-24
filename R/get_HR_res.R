get_HR_res <- function(df, ID_num) {
  # Computes heart rate reserve values for a single ID based on first step HR value
  #
  # Args:
  #   df: a data frame containing data for computation   
  #   ID_num: subject ID
  #
  # Returns:
  #   A data frame adding a colunm with computed percent HR reserve 
  #   values to the imput data frame
  
  require(tidyverse)
  
  ID_df <- filter(df, ID == ID_num)
  
  HR_max <- 208 - 0.7 * unique(ID_df$Age)
  HR_res <- HR_max - ID_df$HR[1]
  ID_df$percent_HR_res <- NA
  for (i in 1:nrow(ID_df)) {
    ID_df$percent_HR_res[i] <- ((ID_df$HR[i] - ID_df$HR[1]) / HR_res) * 100
  }
  return(ID_df)
}