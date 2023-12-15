weather_data <-  read.csv("final_weather_data18.csv")

# Summary statistics for temperature variables
summary(weather_data[c("TG1", "TN1", "TX1")])

# Time series plots for daily mean, minimum, and maximum temperatures
library(ggplot2)

ggplot(weather_data, aes(x = DATE)) +
  geom_line(aes(y = TG1, color = "Daily Mean Temp")) +
  geom_line(aes(y = TN1, color = "Daily Min Temp")) +
  geom_line(aes(y = TX1, color = "Daily Max Temp")) +
  labs(title = "Daily Temperatures (1960-2019)",
       y = "Temperature (°C)", color = "Variables") +
  theme_minimal()

# Summary statistics for precipitation
summary(weather_data$RR1)

# Histogram for precipitation distribution
ggplot(weather_data, aes(x = RR1)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Precipitation",
       x = "Precipitation (mm)", y = "Frequency") +
  theme_minimal()


# Summary statistics for sunshine duration
summary(weather_data$SS1)

# Time series plot for sunshine duration
ggplot(weather_data, aes(x = as.Date(DATE), y = SS1)) +
  geom_line(color = "orange") +
  labs(title = "Sunshine Duration Over Time",
       x = "Date", y = "Sunshine Duration (hours)") +
  theme_minimal()


# Extracting month and year from DATE column
weather_data$DATE <- as.Date(weather_data$DATE, format = "%d/%m/%Y")
weather_data$Year <- lubridate::year(weather_data$DATE)
weather_data$Month <- lubridate::month(weather_data$DATE)

# Seasonal analysis using boxplots
ggplot(weather_data, aes(x = factor(Month), y = CC)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Seasonal Analysis of Cloud Cover",
       x = "Month", y = "Cloud Cover (Oktas)") +
  theme_minimal()

