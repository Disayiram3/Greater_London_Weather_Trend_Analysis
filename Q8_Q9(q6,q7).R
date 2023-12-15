dataset <- read.csv("final_weather_data18.csv")
#summary(dataset)

library(ggplot2)

ggplot(dataset, aes(x = CC, y = SS1)) +
  geom_point() +
  labs(title = "Relationship between Cloud Cover and Sunshine Duration",
       x = "Cloud Cover (oktas)",
       y = "Sunshine Duration (hours)")

# Linear regression model
reg_model <- lm(SS1 ~ CC, data = dataset)
summary(reg_model)
##########
ggplot(dataset, aes(x = TG1, y = SD)) +
  geom_point() +
  labs(title = "Relationship between Mean Temperature and Snow Depth",
       x = "Mean Temperature (°C)",
       y = "Snow Depth (cm)")

# Linear regression model
reg_model <- lm(SD ~ TG1, data = dataset)
summary(reg_model)
