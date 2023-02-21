# Functions for standardizing data
# Sanjay kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-21

message("Loading functions from standarization-functions.R")


standardize <- function(data) {
  # calculating the mean
  mean_data <- mean(data)
  # calculating standard deviation of data
  sd_data <- sd(data)
  # calculating standardized scores
  standard_data <- (data - mean_data)/sd_data
  return(standard_data) 
}

standardize_df <- function(df) {
  # Identify numeric columns
  col_numeric <- sapply(X = df, FUN = is.numeric)
  # Run standardize on all numeric columns
  df[, col_numeric] <- apply(X = df[, col_numeric],
                             MARGIN = 2,
                             FUN = standardize)
  return(df)
}