# Assuming weather_data contains your dataset
weather_data <-  read.csv("final_weather_data18.csv")

# Convert columns to numeric if they're not already
weather_data$SS1 <- as.numeric(weather_data$SS1)
weather_data$CC <- as.numeric(weather_data$CC)

# Linear regression between cloud cover and sunshine duration
lm_model_cc_ss <- lm(SS1 ~ CC, data = weather_data)

# Summary of the regression model
summary(lm_model_cc_ss)


# Convert columns to numeric if needed
weather_data$SD <- as.numeric(weather_data$SD)
weather_data$TG1 <- as.numeric(weather_data$TG1)

# Linear regression between mean temperature and snow depth
lm_model_tg1_sd <- lm(SD ~ TG1, data = weather_data)

# Summary of the regression model
summary(lm_model_tg1_sd)
