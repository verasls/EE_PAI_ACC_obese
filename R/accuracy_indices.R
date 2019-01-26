accuracy_indices <- function(df, actual_var, predicted_var) {
  # Computes indices of accuracy
  #
  # Args:
  #   df: data frame containing the data used to compute the indices
  #   predicted_var: predicted variable name (as character)
  #   actual_var: dependent variable name (as character)
  #
  # Returns:
  #   A data frame containing the indices values
  
  df <- as.data.frame(df)
  
  # Absolute values
  bias_mean <- round(mean(df[ , actual_var] - df[ , predicted_var]), digits = 2)
  bias_sd   <- round(sd(df[ , actual_var] - df[ , predicted_var]), digits = 2)
  LoA_inf   <- round(bias_mean - (1.96 * bias_sd), digits = 2)
  LoA_sup   <- round(bias_mean + (1.96 * bias_sd), digits = 2)
  MAE       <- round(mean(abs(df[ , actual_var] - df[ , predicted_var])), digits = 2)
  MAE_sd    <- round(sd(abs(df[ , actual_var] - df[ , predicted_var])), digits = 2)
  RMSE      <- round(sqrt(mean((df[ , actual_var] - df[ , predicted_var])^2)), digits = 2)
  
  # Percent values
  bias_mean_p <- round(mean(((df[ , actual_var] - df[ , predicted_var]) / df[ , actual_var])) * 100, digits = 2)
  bias_sd_p   <- round(sd(((df[ , actual_var] - df[ , predicted_var]) / df[ , actual_var])) * 100, digits = 2)
  LoA_inf_p   <- round(bias_mean_p - (1.96 * bias_sd_p), digits = 2)
  LoA_sup_p   <- round(bias_mean_p + (1.96 * bias_sd_p), digits = 2)
  MAPE        <- round(mean(abs((df[ , actual_var] - df[ , predicted_var]) / df[ , actual_var])) * 100, digits = 2)
  MAPE_sd     <- round(sd(abs((df[ , actual_var] - df[ , predicted_var]) / df[ , actual_var])) * 100, digits = 2)
  CV_RMSE     <- round((RMSE / mean(df[ , actual_var])) * 100, digits = 2)
  
  accuracy <- data.frame(
    bias_mean, bias_sd, LoA_inf, LoA_sup, MAE, MAE_sd, RMSE, 
    bias_mean_p, bias_sd_p, LoA_inf_p, LoA_sup_p, MAPE, MAPE_sd, CV_RMSE
  )
  return(accuracy)
}