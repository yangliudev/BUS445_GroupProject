library(tidyverse)

data = read_csv("hotel_bookings.csv")

# EDA
summary(data)
str(data)

# Check how many people cancelled
table(data$is_canceled)
prop.table(table(data$is_canceled)) * 100

# NOTE: The following plots were created with the help of ChatGPT
# Cancellation by Hotel Type
data %>%
  group_by(hotel, is_canceled) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = hotel, y = percentage, fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cancellation Rate by Hotel Type", x = "Hotel Type", y = "Percentage", fill = "Canceled")

# Cancellation by Month
data %>%
  group_by(arrival_date_month, is_canceled) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = arrival_date_month, y = percentage, fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cancellation Rate by Month", x = "Month", y = "Percentage", fill = "Canceled") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cancellation by Season
library(lubridate)

data %>%
  mutate(booking_date = ymd(paste(arrival_date_year, arrival_date_month, 1, sep = "-")),
         booking_season = case_when(
           month(booking_date) %in% c(12, 1, 2) ~ "Winter",
           month(booking_date) %in% c(3, 4, 5) ~ "Spring",
           month(booking_date) %in% c(6, 7, 8) ~ "Summer",
           month(booking_date) %in% c(9, 10, 11) ~ "Fall"
         )) %>%
  group_by(booking_season, is_canceled) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = booking_season, y = percentage, fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cancellation Rate by Booking Season", x = "Season", y = "Percentage", fill = "Canceled")

# Cancellation by Country
data %>%
  group_by(country, is_canceled) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(count > 50) %>% # Filter for countries with a sufficient sample size
  ggplot(aes(x = reorder(country, -percentage), y = percentage, fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cancellation Rate by Country", x = "Country", y = "Percentage", fill = "Canceled") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
