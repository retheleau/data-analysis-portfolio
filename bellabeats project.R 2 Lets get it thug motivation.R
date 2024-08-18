# Load the needed libraries
library(readr)
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

# Set the directory to pull CSV files from
setwd("C:/Users/rethe/Documents/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16")

# Read CSV files
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
hourly_intensities <- read_csv("hourlyIntensities_merged.csv")
weight <- read_csv("weightLogInfo_merged.csv")

# Check the first few rows of each dataframe
head(daily_activity)
head(daily_steps)
head(daily_sleep)
head(hourly_steps)
head(hourly_calories)
head(hourly_intensities)
head(weight)

# check the structure of the data so I know whats going inside there
str(daily_activity)
str(daily_sleep)
str(daily_steps)
str(hourly_calories)
str(hourly_intensities)
str(hourly_steps)
str(weight)


# Time to count the unique number of users in each dataframe no set tripping
n_unique(daily_activity$Id)
n_unique(daily_sleep$Id)
n_unique(daily_steps$Id)
n_unique(hourly_calories$Id)
n_unique(hourly_intensities$Id)
n_unique(hourly_steps$Id)
n_unique(weight$Id)

#check for duplicates no need for double trouble aint no double mint twinz
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(daily_steps))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))

# Time to clean duplicates to the window to the Wall!
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()
daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()
daily_steps <- daily_steps %>%
  distinct() %>%
  drop_na()
hourly_calories <- hourly_calories %>%
  distinct() %>%
  drop_na()
hourly_intensities <- hourly_intensities %>%
  distinct() %>%
  drop_na()
hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

#got to make sure the whole window to the wall got cleaned got damn no duplicates
sum(duplicated(daily_sleep))

#Time to get low get low get low put them thangs in lowercase
clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)
clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)
clean_names(daily_steps)
daily_steps <- rename_with(daily_steps, tolower)
clean_names(hourly_calories)
hourly_calories <- rename_with(hourly_calories, tolower)
clean_names(hourly_intensities)
hourly_intensities <- rename_with(hourly_intensities, tolower)
clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)

#got to run it back bring it back like travis porter make sure its right
head(daily_activity)
head(daily_sleep)
head(daily_steps)
head(hourly_calories)
head(hourly_intensities)
head(hourly_steps)
#renaming date and time to always be on time baby I gave you my all
daily_activity <- daily_activity %>%
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))
daily_sleep <- daily_sleep %>%
  rename(date = sleepday) %>%
  mutate(date = as_date(date, format ="%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
hourly_calories <- hourly_calories %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
hourly_intensities <- hourly_intensities %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
hourly_steps<- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

#about to do a fusion dance Fuuuuuuuuusion merge the datasets
daily_activity_sleep <- merge(daily_activity, daily_sleep, by=c ("id", "date"))

#take a sneak peak to see if fusion is successful next time on dragonball z
glimpse(daily_activity_sleep)

#Time to apply the wisdom my child seek to analyze this sheeeiiit
daily_activity %>%  
  select(totalsteps,
         totaldistance,
         sedentaryminutes, calories) %>%
  summary()

#active minutes summary
daily_activity %>%
  select(veryactiveminutes,
         fairlyactiveminutes,
         lightlyactiveminutes) %>%
  summary()
#daily sleep summary
daily_sleep %>%
  select(totalsleeprecords,
         totalminutesasleep,
         totaltimeinbed) %>%
  summary()
#hourly calories summary
hourly_calories %>%
  select(calories) %>%
  summary()

#time for some visuals steps taken vs calories burned 
ggplot(data = daily_activity, aes(x = totalsteps, y = calories)) + 
  geom_point() + geom_smooth() + labs(title ="Total Steps vs. Calories")
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

#now visualize it if I can see it I can believe it I believe I can fly
ggplot(weekday_sleep, aes(weekday, daily_sleep)) +
  geom_col(fill = "#8934eb") +
  geom_hline(yintercept = 480) +
  labs(title = "Minutes asleep per weekday", x= "", y = "") +
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
#split the datetime column to date and time colums divide it
hourly_intensities <- hourly_intensities %>%
  separate(date_time, into = c("date", "time"), sep= " ") 

head(hourly_intensities)

hourly_intensities <- hourly_intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(totalintensity))

#visulize the hourly intensities
ggplot(data = hourly_intensities, aes(x = time, y = mean_total_int)) + geom_histogram(stat = "identity", fill='red') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")
#separate the datetime column to date and a time column
hourly_steps <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 

head(hourly_steps)
#visulize steps throughout the day
hourly_steps %>%
  group_by(time) %>%
  summarize(average_steps = mean(steptotal)) %>%
  ggplot() +
  geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
  labs(title = "Hourly steps throughout the day", x="", y="") + 
  scale_fill_gradient(low = "yellow", high = "blue")+
  theme(axis.text.x = element_text(angle = 90))
#make a new data frame grouping by id and calculate the days smart watch was used
daily_use <- daily_activity_sleep %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(user_type= case_when(
    days_used >= 1 & days_used <= 10 ~ "low user",
    days_used >= 11 & days_used <= 20 ~ "moderate user", 
    days_used >= 21 & days_used <= 31 ~ "high user", 
  ))

head(daily_use)
#Create a percentage data frame to better visualize the results in the graph
daily_use_percent <- daily_use %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

daily_use_percent$user_type <- factor(daily_use_percent$user_type, levels = c("high user", "moderate user", "low user"))

head(daily_use_percent)
#visulization of smart device usage per user dropping it like its hot
daily_use_percent %>%
  ggplot(aes(x = "",y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c( "yellow","#10c3eb","#a6b2b3"),
                    labels = c("High user - 21 to 31 days",
                               "Moderate user - 11 to 20 days",
                               "Low user - 1 to 10 days"))+
  labs(title="Daily use of smart device")
view(daily_activity_sleep)