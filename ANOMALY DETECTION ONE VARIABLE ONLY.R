dataset <- read.csv("final_weather_data18.csv")

library(tidyverse)
library(anomalize)
library(lubridate)

# Convert the DATE column to a Date object
dataset$DATE <- dmy(dataset$DATE)

# Convert the dataset to a tibble
dataset <- as_tibble(dataset)

# Example of time series analysis on the 'TG1' variable
tg1_data <- dataset %>%
  select(DATE, CC) %>%
  drop_na()  # Remove NA values if necessary

# Decompose, detect anomalies, and plot for TG1
tg1_data %>%
  time_decompose(CC, method = "stl") %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies() +
  ggtitle("Anomalies in Daily Mean Temperature (TG1)")
