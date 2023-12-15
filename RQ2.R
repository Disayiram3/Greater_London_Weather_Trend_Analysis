library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset
dataset <- read.csv("final_weather_data18.csv")

# Ensure the DATE column is in the correct Date format
dataset$DATE <- dmy(dataset$DATE)

# Check the structure of the data to confirm DATE conversion and data in CC
str(dataset)

# Add a month column for grouping the data by month
dataset$MONTH <- factor(month(dataset$DATE, label = TRUE, abbr = TRUE), 
                        levels = month.abb)

# Check for any NA values in CC and the summary of CC
summary(dataset$CC)

# Calculate mean cloud cover by month across all years
cloud_cover_by_month <- dataset %>%
  group_by(MONTH) %>%
  summarize(MeanCloudCover = mean(CC, na.rm = TRUE), .groups = 'drop')

# Check the summary statistics to confirm they are calculated correctly
print(cloud_cover_by_month)

# Plot the mean cloud cover by month to visualize seasonality
cloud_cover_seasonality_plot <- ggplot(cloud_cover_by_month, aes(x = MONTH, y = MeanCloudCover)) +
  geom_line(group = 1) +  # Make sure to group the line plot to connect the points
  labs(
    title = "Seasonal Cloud Cover Patterns in Greater London (1960 - 2019)",
    x = "Month",
    y = "Mean Cloud Cover (oktas)"
  ) +
  theme_minimal()

# Display the plot
print(cloud_cover_seasonality_plot)     


##
#Find whether it is normal distribution or not
##
library(ggplot2)

# Assuming dataset is already loaded with 'CC' as the cloud cover variable

# Histogram
ggplot(dataset, aes(x = CC)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(col = "red") +
  labs(title = "Histogram of Cloud Cover", x = "Cloud Cover", y = "Density")

# QQ Plot
qqnorm(dataset$CC)
qqline(dataset$CC, col = "steelblue", lwd = 2)


# Kolmogorov-Smirnov Test
ks_test <- ks.test(dataset$CC, "pnorm", mean = mean(dataset$CC, na.rm = TRUE), sd = sd(dataset$CC, na.rm = TRUE))
ks_test

# Note: If the sample size is large, even small deviations from normality can result in a significant p-value.

                                       