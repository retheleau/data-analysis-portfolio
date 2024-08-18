# Load libraries
library(readr)
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

# Set working directory
setwd("C:/Users/rethe/Documents/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16")

# Load data
load_data <- function(file_name) {
  read_csv(file_name) %>%
    rename_with(tolower) %>%
    drop_na()
}

# Load datasets
daily_activity <- load_data("dailyActivity_merged.csv")
daily_sleep <- load_data("sleepDay_merged.csv")
daily_steps <- load_data("dailySteps_merged.csv")
hourly_steps <- load_data("hourlySteps_merged.csv")
hourly_calories <- load_data("hourlyCalories_merged.csv")
hourly_intensities <- load_data("hourlyIntensities_merged.csv")
weight <- load_data("weightLogInfo_merged.csv")

# Check for duplicates and remove them
remove_duplicates <- function(df) {
  df %>%
    distinct() %>%
    drop_na()
}

daily_activity <- remove_duplicates(daily_activity)
daily_sleep <- remove_duplicates(daily_sleep)
daily_steps <- remove_duplicates(daily_steps)
hourly_steps <- remove_duplicates(hourly_steps)
hourly_calories <- remove_duplicates(hourly_calories)
hourly_intensities <- remove_duplicates(hourly_intensities)

# Check for unique users
count_unique_users <- function(df) {
  n_unique(df$id)
}

unique_users_daily_activity <- count_unique_users(daily_activity)
unique_users_daily_sleep <- count_unique_users(daily_sleep)
unique_users_daily_steps <- count_unique_users(daily_steps)
unique_users_hourly_steps <- count_unique_users(hourly_steps)
unique_users_hourly_calories <- count_unique_users(hourly_calories)
unique_users_hourly_intensities <- count_unique_users(hourly_intensities)

# Clean and format date columns
clean_date_columns <- function(df, date_column) {
  df %>%
    rename(date = !!sym(date_column)) %>%
    mutate(date = as_date(date, format = "%m/%d/%Y"))
}

daily_activity <- clean_date_columns(daily_activity, "activitydate")
daily_sleep <- clean_date_columns(daily_sleep, "sleepday")

# Merge datasets
merge_datasets <- function(df1, df2, by_columns) {
  merge(df1, df2, by = by_columns)
}

daily_activity_sleep <- merge_datasets(daily_activity, daily_sleep, c("id", "date"))

# Summarize data
summarize_data <- function(df, columns) {
  df %>%
    select(all_of(columns)) %>%
    summary()
}

daily_activity_summary <- summarize_data(daily_activity, c("totalsteps", "totaldistance", "sedentaryminutes", "calories"))
daily_sleep_summary <- summarize_data(daily_sleep, c("totalsleeprecords", "totalminutesasleep", "totaltimeinbed"))

# Visualize data
visualize_steps_calories <- function(df) {
  ggplot(df, aes(x = totalsteps, y = calories)) +
    geom_point() +
    geom_smooth() +
    labs(title = "Total Steps vs. Calories")
}

visualize_steps_calories(daily_activity)

#Steps per weekday to know who a big stepper
weekday_steps <- daily_activity %>%
  mutate(weekday = weekdays(date))
weekday_steps$weekday <-ordered(weekday_steps$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                "Friday", "Saturday", "Sunday"))
weekday_steps <-weekday_steps %>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(totalsteps))

head(weekday_steps)
#daily steps per weekday
ggplot(weekday_steps, aes(weekday, daily_steps)) +
  geom_col(fill = "#34c9eb") + 
  geom_hline(yintercept = 7500) +
  labs(title = "Daily steps per weekday", x= "", y = "") +
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
#sleep per week summary
weekday_sleep <- daily_sleep %>%
  mutate(weekday = weekdays(date))
weekday_sleep$weekday <-ordered(weekday_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                "Friday", "Saturday", "Sunday"))

weekday_sleep <-weekday_sleep %>%
  group_by(weekday) %>%
  summarize (daily_sleep = mean(totalminutesasleep))

head(weekday_sleep)

# Visualize minutes asleep per weekday
ggplot(weekday_sleep, aes(x = weekday, y = daily_sleep)) +
  geom_col(fill = "#8934eb") +
  geom_hline(yintercept = 480) +
  labs(title = "Minutes asleep per weekday", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

# Split datetime column into date and time columns
hourly_intensities <- hourly_intensities %>%
  separate(activityhour, into = c("date", "time"), sep = " ")

# Summarize hourly intensities
hourly_intensities <- hourly_intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(totalintensity))

# Visualize hourly intensities
ggplot(hourly_intensities, aes(x = time, y = mean_total_int)) +
  geom_col(fill = 'red') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Average Total Intensity vs. Time")

# Split datetime column into date and time columns
hourly_steps <- hourly_steps %>%
  separate(activityhour, into = c("date", "time"), sep = " ") %>%
  mutate(date = ymd(date))

# Summarize hourly steps
hourly_steps <- hourly_steps %>%
  group_by(time) %>%
  summarise(average_steps = mean(steptotal))

# Visualize hourly steps
ggplot(hourly_steps, aes(x = time, y = average_steps)) +
  geom_col(aes(fill = average_steps)) +
  labs(title = "Hourly steps throughout the day", x = "", y = "") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  theme(axis.text.x = element_text(angle = 90))

# Calculate days smart watch was used per user
daily_use <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise(days_used = n()) %>%
  mutate(user_type = case_when(
    days_used >= 1 & days_used <= 10 ~ "low user",
    days_used >= 11 & days_used <= 20 ~ "moderate user",
    days_used >= 21 & days_used <= 31 ~ "high user"
  ))

# Calculate percentage of users per type
daily_use_percent <- daily_use %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  mutate(total_percent = total / totals * 100) # Calculate percentage

# Convert user_type to factor
daily_use_percent$user_type <- factor(daily_use_percent$user_type, levels = c("high user", "moderate user", "low user"))

# Visualize smart device usage per user
ggplot(daily_use_percent, aes(x = "", y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  geom_text(aes(label = paste0(round(total_percent), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values = c("yellow", "#10c3eb", "#a6b2b3"),
    labels = c(
      "High user - 21 to 31 days",
      "Moderate user - 11 to 20 days",
      "Low user - 1 to 10 days"
    )
  ) +
  labs(title = "Daily use of smart device")