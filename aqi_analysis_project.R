setwd("C:/Users/TANISHA MANNA/OneDrive/Desktop/R/R project")
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)
city_day <- read_csv("C:/Users/TANISHA MANNA/OneDrive/Desktop/R/R project/city_day.csv/city_day.csv")
library(readr)
library(dplyr)
glimpse(city_day)
city_day$Date <- as.Date(city_day$Date, format = "%Y-%m-%d")
colSums(is.na(city_day))
city_day_clean <- city_day %>% drop_na(AQI)
unique(city_day_clean$City)
# Daily average AQI across all cities
daily_avg_aqi <- city_day_clean %>%
  group_by(Date) %>%
  summarise(Avg_AQI = mean(AQI, na.rm = TRUE))

# Plot the trend
ggplot(daily_avg_aqi, aes(x = Date, y = Avg_AQI)) +
  geom_line(color = "steelblue") +
  labs(title = "Pan-India Average AQI Over Time",
       x = "Date", y = "Average AQI") +
  theme_minimal()
top_cities <- city_day_clean %>%
  group_by(City) %>%
  summarise(Avg_AQI = mean(AQI, na.rm = TRUE)) %>%
  arrange(desc(Avg_AQI)) %>%
  slice(1:10)

# Plot
ggplot(top_cities, aes(x = reorder(City, -Avg_AQI), y = Avg_AQI, fill = City)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Polluted Cities in India (Avg AQI)",
       x = "City", y = "Average AQI") +
  theme_minimal() +
  theme(legend.position = "none")
bucket_distribution <- city_day_clean %>%
  group_by(City, AQI_Bucket) %>%
  summarise(Days = n()) %>%
  ungroup()

# Top 5 cities for clarity
top5_cities <- top_cities$City[1:5]

ggplot(bucket_distribution %>% filter(City %in% top5_cities),
       aes(x = AQI_Bucket, y = Days, fill = AQI_Bucket)) +
  geom_bar(stat = "identity") +
  facet_wrap(~City) +
  theme_minimal() +
  labs(title = "AQI Bucket Distribution in Top 5 Polluted Cities",
       x = "AQI Category", y = "Number of Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
city_day_clean$Month <- format(city_day_clean$Date, "%m")
city_day_clean$Year <- format(city_day_clean$Date, "%Y")
monthly_aqi <- city_day_clean %>%
  group_by(Month) %>%
  summarise(Average_AQI = mean(AQI, na.rm = TRUE))

ggplot(monthly_aqi, aes(x = Month, y = Average_AQI)) +
  geom_line(group = 1, color = "darkblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Monthly Average AQI Across India",
       x = "Month", y = "Average AQI")
# ---------------------- AQI PREDICTION START ----------------------

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Prepare Data for Regression
pollution_data <- city_day_clean %>%
  select(AQI, PM2.5, PM10, NO, NO2, NOx, NH3, CO, SO2, O3) %>%
  drop_na()

# Step 2: Split into Train & Test Sets
set.seed(123)
sample_index <- sample(1:nrow(pollution_data), 0.8 * nrow(pollution_data))
train_data <- pollution_data[sample_index, ]
test_data <- pollution_data[-sample_index, ]

# Step 3: Train Linear Regression Model
aqi_model <- lm(AQI ~ ., data = train_data)
summary(aqi_model)  # View model performance

# Step 4: Predict on Test Data
predicted_aqi <- predict(aqi_model, newdata = test_data)

# Step 5: Evaluate Model
actual_aqi <- test_data$AQI
rmse <- sqrt(mean((predicted_aqi - actual_aqi)^2))
r_squared <- 1 - sum((actual_aqi - predicted_aqi)^2) / sum((actual_aqi - mean(actual_aqi))^2)

print(paste("RMSE:", round(rmse, 2)))
print(paste("R-squared:", round(r_squared, 3)))

# Step 6: Plot Actual vs Predicted AQI
ggplot(data = NULL, aes(x = actual_aqi, y = predicted_aqi)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual AQI", y = "Predicted AQI", title = "Actual vs Predicted AQI") +
  theme_minimal()

# ---------------------- AQI PREDICTION END ----------------------
