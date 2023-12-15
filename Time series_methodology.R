weather_data <-  read.csv("final_weather_data18.csv")

# Extract temperature columns
temperature_data <- weather_data[, c("DATE", "TG1", "TN1", "TX1")]

# Plot time series for temperatures
library(ggplot2)

ggplot(temperature_data, aes(x = as.Date(DATE), group = 1)) +
  geom_line(aes(y = TG1, color = "Daily Mean Temp")) +
  geom_line(aes(y = TN1, color = "Daily Min Temp")) +
  geom_line(aes(y = TX1, color = "Daily Max Temp")) +
  labs(title = "Daily Mean, Min, and Max Temperatures (1960-2019)",
       y = "Temperature (°C)", color = "Variables") +
  theme_minimal()


# Extract sunshine duration column
sunshine_data <- weather_data[, c("DATE", "SS1")]

# Plot time series for sunshine duration
ggplot(sunshine_data, aes(x = as.Date(DATE), y = SS1)) +
  geom_line() +
  labs(title = "Daily Sunshine Duration (1960-2019)",
       y = "Sunshine Duration (hours)") +
  theme_minimal()


# Extract sea level pressure column
pressure_data <- weather_data[, c("DATE", "PP1")]

# Plot time series for sea level pressure
ggplot(pressure_data, aes(x = as.Date(DATE), y = PP1)) +
  geom_line() +
  labs(title = "Daily Sea Level Pressure (1960-2019)",
       y = "Sea Level Pressure (hPa)") +
  theme_minimal()


###
# Convert 'DATE' column to Date format with correct format specifier
weather_data$DATE <- as.Date(weather_data$DATE, format = "%d/%m/%Y")

# Calculate yearly averages
yearly_avg <- aggregate(PP1 ~ format(DATE, "%Y"), data = weather_data, FUN = mean)

# Calculate yearly standard deviations
yearly_std <- aggregate(PP1 ~ format(DATE, "%Y"), data = weather_data, FUN = sd)

# Merge the averages and standard deviations data frames
combined_data <- merge(yearly_avg, yearly_std, by.x = "format(DATE, \"%Y\")", by.y = "format(DATE, \"%Y\")")

# Plot yearly averages with standard deviation error bars
ggplot(combined_data, aes(x = as.numeric(`format(DATE, "%Y")`), y = PP1.x)) +
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = PP1.x - PP1.y, ymax = PP1.x + PP1.y), width = 0.2) +
  labs(title = "Yearly Average Sea Level Pressure (1960-2019)",
       x = "Year",
       y = "Average Pressure (hPa)") +
  theme_minimal()

###
# Convert 'DATE' column to Date format with correct format specifier
weather_data$DATE <- as.Date(weather_data$DATE, format = "%d/%m/%Y")

# Calculate monthly averages
monthly_avg <- aggregate(PP1 ~ format(DATE, "%Y-%m"), data = weather_data, FUN = mean)

# Calculate monthly standard deviations
monthly_std <- aggregate(PP1 ~ format(DATE, "%Y-%m"), data = weather_data, FUN = sd)

# Merge the averages and standard deviations data frames
combined_data <- merge(monthly_avg, monthly_std, by.x = "format(DATE, \"%Y-%m\")", by.y = "format(DATE, \"%Y-%m\")")

# Plot monthly averages with standard deviation error bars
ggplot(combined_data, aes(x = as.Date(paste0(`format(DATE, "%Y-%m")`, "-01")), y = PP1.x)) +
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = PP1.x - PP1.y, ymax = PP1.x + PP1.y), width = 0.2) +
  labs(title = "Monthly Average Sea Level Pressure (1960-2019)",
       x = "Month",
       y = "Average Pressure (hPa)") +
  theme_minimal()
#####
# Load the ggplot2 library
library(ggplot2)

# Convert the 'DATE' column to Date format with the correct format specifier
weather_data$DATE <- as.Date(weather_data$DATE, format = "%d/%m/%Y")

# Create a time series plot for sea level pressure
ggplot(weather_data, aes(x = DATE, y = PP1)) +
  geom_line() +
  labs(title = "Time Series of Sea Level Pressure (1960-2019)",
       x = "Date",
       y = "Sea Level Pressure (hPa)") +
  theme_minimal()


