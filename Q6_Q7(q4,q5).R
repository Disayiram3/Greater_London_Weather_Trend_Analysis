library(ggplot2)
# Read the dataset
dataset <- read.csv("final_weather_data18.csv")
# Assuming the dataset has the variables 'RR1' for daily precipitation amount and 'HU' for daily humidity


# Check for normality first, if not normal, consider Spearman's correlation
set.seed(123)  # For reproducibility
sample_rr1 <- sample(dataset$RR1, size = 5000, replace = FALSE)
shapiro.test(sample_rr1)

set.seed(123)  # For reproducibility
sample_hu <- sample(dataset$HU, size = 5000, replace = FALSE)
shapiro.test(sample_hu)

set.seed(123)  # For reproducibility
sample_cc <- sample(dataset$CC, size = 5000, replace = FALSE)
shapiro.test(sample_cc)

set.seed(123)  # For reproducibility
sample_qq <- sample(dataset$QQ, size = 5000, replace = FALSE)
shapiro.test(sample_qq)



# Pearson Correlation if both are normally distributed
#cor.test(dataset$RR1, dataset$HU, method = "pearson")

# Spearman Correlation if not normally distributed
#cor.test(dataset$RR1, dataset$HU, method = "spearman")

# Spearman's rank correlation for daily precipitation amount and daily humidity
spearman_corr_q4 <- cor.test(dataset$RR1, dataset$HU, method = "spearman")

# Output the results
spearman_corr_q4

ggplot(dataset, aes(x = RR1, y = HU)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +  # Adds a linear regression line
  labs(title = "Scatter Plot of Daily Precipitation Amount vs. Daily Humidity",
       x = "Daily Precipitation Amount (mm)",
       y = "Daily Humidity (%)") +
  theme_minimal()



# Spearman's rank correlation for cloud cover and global radiation
spearman_corr_q5 <- cor.test(dataset$CC, dataset$QQ, method = "spearman")

# Output the results
spearman_corr_q5

ggplot(dataset, aes(x = CC, y = QQ)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +  # Adds a linear regression line
  labs(title = "Scatter Plot of Cloud Cover vs. Global Radiation",
       x = "Cloud Cover (oktas)",
       y = "Global Radiation (W/m²)") +
  theme_minimal()

correlation_results <- data.frame(
  Pair = c("RR1 & HU", "CC & QQ"),
  Correlation = c(spearman_corr_q4$estimate, spearman_corr_q5$estimate),
  P_Value = c(spearman_corr_q4$p.value, spearman_corr_q5$p.value)
)
