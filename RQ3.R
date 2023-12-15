library(ggplot2)
library(tidyr)

# Read the dataset
dataset <- read.csv("final_weather_data18.csv")
# Assuming your dataset has columns 'TG1' for mean, 'TX1' for max, and 'TN1' for min temperatures
# and 'DATE' column has been converted to Date format

# Convert DATE to a Date object if not already
dataset$DATE <- as.Date(dataset$DATE, format = "%d/%m/%Y")

##
#Plot TG1,TX1, TN1  one grap
# Reshape the dataset
dataset_long <- dataset %>%
  gather(variable, temperature, TG1, TX1, TN1)

# Create the plot
ggplot(dataset_long, aes(x = DATE, y = temperature, color = variable)) +
  geom_line() +
  labs(title = "Temperature Trends in Greater London (1960 - 2019)",
       x = "Year", y = "Temperature (°C)") +
  scale_color_manual(values = c("TG1" = "blue", "TX1" = "red", "TN1" = "green")) +
  theme_minimal()

##

# Plotting mean temperature over time
ggplot(dataset, aes(x = DATE, y = TG1)) + 
  geom_line() +
  labs(title = "Mean Temperature in Greater London (1960 - 2019)",
       x = "Year", y = "Temperature (°C)")

# Plotting maximum temperature over time
ggplot(dataset, aes(x = DATE, y = TX1)) + 
  geom_line() +
  labs(title = "Maximum Temperature in Greater London (1960 - 2019)",
       x = "Year", y = "Temperature (°C)")

# Plotting minimum temperature over time
ggplot(dataset, aes(x = DATE, y = TN1)) + 
  geom_line() +
  labs(title = "Minimum Temperature in Greater London (1960 - 2019)",
       x = "Year", y = "Temperature (°C)")

library(forecast)

# Decompose the mean temperature time series (TG1)
tg1_decomp <- stl(ts(dataset$TG1, frequency = 365.25), s.window = "periodic")
plot(tg1_decomp)

# Decompose the maximum temperature time series (TX1)
tx1_decomp <- stl(ts(dataset$TX1, frequency = 365.25), s.window = "periodic")
plot(tx1_decomp)

# Decompose the minimum temperature time series (TN1)
tn1_decomp <- stl(ts(dataset$TN1, frequency = 365.25), s.window = "periodic")
plot(tn1_decomp)


library(tseries)

# ADF test for mean temperature
adf.test(ts(dataset$TG1, frequency = 365.25), alternative = "stationary")

# ADF test for maximum temperature
adf.test(ts(dataset$TX1, frequency = 365.25), alternative = "stationary")

# ADF test for minimum temperature
adf.test(ts(dataset$TN1, frequency = 365.25), alternative = "stationary")



# Linear model for mean temperature trend
mean_trend_lm <- lm(TG1 ~ as.numeric(DATE), data = dataset)
summary(mean_trend_lm)

# Linear model for maximum temperature trend
maximum_trend_lm <- lm(TX1 ~ as.numeric(DATE), data = dataset)
summary(maximum_trend_lm)

# Linear model for minimum temperature trend
minimum_trend_lm <- lm(TN1 ~ as.numeric(DATE), data = dataset)
summary(minimum_trend_lm)


# STL decomposition for mean temperature
mean_decomp <- stl(ts(dataset$TG1, frequency = 365.25), s.window = "periodic")
plot(mean_decomp)

# STL decomposition for maximum temperature
maximum_decomp <- stl(ts(dataset$TX1, frequency = 365.25), s.window = "periodic")
plot(maximum_decomp)

# STL decomposition for minimum temperature
minimum_decomp <- stl(ts(dataset$TN1, frequency = 365.25), s.window = "periodic")
plot(minimum_decomp)


# Checking residuals for the mean temperature linear model
checkresiduals(mean_trend_lm)


# Checking residuals for the maximum temperature linear model
checkresiduals(maximum_trend_lm)


# Checking residuals for the minimum temperature linear model
checkresiduals(minimum_trend_lm)




