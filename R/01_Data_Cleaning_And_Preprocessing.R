# Library----
library(tidyverse)
library(assertive)

# Define a function for loading and initial inspection
load_and_inspect_data <- function(data_path) {
  data <- read_csv(data_path)

  # Print initial summary of data
  print(summary(data))

  return(data)
}

# Define a function for handling missing values
handle_missing_values <- function(data) {
  # Check if there are missing values
  assertive::assert_is_data_frame(data)
  missing_values <- sum(is.na(data))
  print(paste("Number of missing values:", missing_values))

  # If there are missing values, handle them (this will depend on the column - for now, let's replace them with the median)
  if (missing_values > 0) {
    data <- data %>%
      mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  }

  return(data)
}

# Define a function for encoding categorical variables as numeric
encode_categorical_variables <- function(data) {
  # Identify categorical variables
  cat_vars <- select_if(data, is.character)

  # Convert categorical variables to factors and then to numeric
  data <- mutate_at(data, vars(cat_vars), funs(as.numeric(as.factor(.))))

  return(data)
}

# Define a function for normalizing data (e.g., scaling to 0-1 range)
normalize_data <- function(data) {
  data <- data %>%
    mutate(across(where(is.numeric), ~(. - min(.)) / (max(.) - min(.))))

  return(data)
}

# Path to the data
data_path <- "data/Telco_customer_churn.csv"

# Run the defined functions
data <- load_and_inspect_data(data_path)
data <- handle_missing_values(data)
data <- encode_categorical_variables(data)
data <- normalize_data(data)
