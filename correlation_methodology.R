weather_data <-  read.csv("final_weather_data18.csv")

# Load required libraries
library(ggplot2)  # For visualization
library(dplyr)    # For data manipulation

# Load dataset
# Assuming 'weather_data' contains the necessary columns (CC for cloud cover, SS1 for sunshine duration)

# Calculate correlation coefficient
correlation_coefficient <- cor(weather_data$CC, weather_data$SS1)

# Output the correlation coefficient
correlation_coefficient

# Scatter plot for cloud cover and sunshine duration
ggplot(weather_data, aes(x = CC, y = SS1)) +
  geom_point() +
  labs(x = "Cloud Cover", y = "Sunshine Duration") +
  ggtitle("Relationship between Cloud Cover and Sunshine Duration")


# Calculate correlation coefficient
correlation_coefficient <- cor(weather_data$CC, weather_data$QQ)

# Output the correlation coefficient
correlation_coefficient

# Scatter plot for cloud cover and sunshine duration
ggplot(weather_data, aes(x = CC, y = QQ)) +
  geom_point() +
  labs(x = "Cloud Cover", y = "Global Radiation") +
  ggtitle("Relationship between Cloud Cover and Global Radiation")
