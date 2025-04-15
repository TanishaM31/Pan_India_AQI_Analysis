setwd("C:/Users/TANISHA MANNA/OneDrive/Desktop/R/R project")  # replace with your folder path
install.packages("tidyverse")  # if not already installed
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
