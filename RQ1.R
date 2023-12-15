library(ggplot2)
library(dplyr)
library(lubridate)

# Load your dataset
dataset <- read.csv("final_weather_data18.csv")

# Convert the DATE column to a Date object
dataset$DATE <- dmy(dataset$DATE)

# Add a year column for grouping the data by year
dataset$YEAR <- year(dataset$DATE)

# Identify precipitation events (any day with more than 0 mm of precipitation)
dataset$PrecipEvent <- ifelse(dataset$RR1 > 0, 1, 0)

# Summary statistics for precipitation events by year
precip_summary_by_year <- dataset %>%
  group_by(YEAR) %>%
  summarize(TotalEvents = sum(PrecipEvent), .groups = 'drop')

# Bar Plot for Frequency of Precipitation Events
ggplot(precip_summary_by_year, aes(x = YEAR, y = TotalEvents)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Annual Frequency of Precipitation Events in Greater London (1960 - 2019)",
       x = "Year",
       y = "Number of Precipitation Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability


##
#Mann-Kendall Test


library(Kendall)

# Assuming dataset is already read and DATE converted to Date format
# Convert the DATE column to a Date object
dataset$DATE <- dmy(dataset$DATE)

# Create a time series of precipitation events (binary: 1 if precipitation > 0, else 0)
precip_events_ts <- dataset %>%
  mutate(PrecipEvent = ifelse(RR1 > 0, 1, 0)) %>%
  na.omit() %>%
  select(DATE, PrecipEvent) %>%
  arrange(DATE) %>%
  .$PrecipEvent  # Extract just the PrecipEvent column as a vector

# Run the Mann-Kendall trend test
mk_result <- MannKendall(precip_events_ts)

# Output the result
print(mk_result)

##
#Find whether it is normal distribution or not
##

# Kolmogorov-Smirnov Test
ks_test <- ks.test(dataset$RR1, "pnorm", mean = mean(dataset$CC, na.rm = TRUE), sd = sd(dataset$CC, na.rm = TRUE))
ks_test

