# PREPARE
# importing required library

library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)


# loading data from local storage of (2021-08 to 2022-07)

df1 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202108-divvy-tripdata.csv")
df2 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202109-divvy-tripdata.csv")
df3 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202110-divvy-tripdata.csv")
df4 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202111-divvy-tripdata.csv")
df5 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202112-divvy-tripdata.csv")

df6 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202201-divvy-tripdata.csv")
df7 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202202-divvy-tripdata.csv")
df8 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202203-divvy-tripdata.csv")
df9 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202204-divvy-tripdata.csv")
df10 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202205-divvy-tripdata.csv")
df11 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202206-divvy-tripdata.csv")
df12 <- read_csv("../coursera_courses/google_data_analytics/case_study/csv_files/202207-divvy-tripdata.csv")

colnames(df1)
typeof(df1)

str(df1)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)
str(df7)
str(df8)
str(df9)
str(df10)
str(df11)
str(df12)

all_data <- bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

str(all_data)
colnames(all_data)

# rename dataset name and one column 

all_trips <- all_data %>% 
  rename(membership_status = member_casual)




# PROCESS

# data cleaning started

# checking ride_id's length is 16 for all non-null values
no_of_char_in_ride_id <- c(nchar(all_trips$ride_id))
summary(no_of_char_in_ride_id)
# all id's length is 16

# checking number of rows and columns
dim(all_trips)
# checking first few values
head(all_trips)

# what types rideable_type contains and also checking if having any typos 
table(all_trips$rideable_type)
# conclusion : classic > electric > docked

# checking membership_status contain any typos or not.
table(all_trips$membership_status)
# conclusion : member > casual

# changing date string to date and making a consist format
all_trips$started_at <- ymd_hms(all_trips$started_at)
all_trips$ended_at <- ymd_hms(all_trips$ended_at)
# changed to YYYY-MM-DD HH:MM:SS

# confirming all above
str(all_trips)

# dropping rows with all nulls
dim(all_trips)
all_trips <- drop_na(all_trips)
dim(all_trips)

# Adding the column that have day, month, year and year of each ride
all_trips$date <- as.Date(all_trips$started_at)     # YYYY-MM-DD

all_trips$year <- format(as.Date(all_trips$date), "%Y")   # year
all_trips$month <- format(as.Date(all_trips$date), "%m")  # month
all_trips$day <- format(as.Date(all_trips$date), "%d")    # day

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") # day of week


# checking column name added or not
colnames(all_trips)

# finding length of ride to check if there are any negative number or not.
all_trips$length_of_ride <- difftime(all_trips$ended_at, all_trips$started_at)
glimpse(all_trips)
all_trips$length_of_ride <- as.numeric(as.character(all_trips$length_of_ride))
# changed length of ride to numeric format

# verifying change
glimpse(all_trips)
is.numeric(all_trips$length_of_ride)


# 1 day = 3600 * 24 seconds = 86400 seconds
# now checking if length_of_ride is negative or more than one day

all_trips %>% 
  filter(length_of_ride<0 | length_of_ride>86400)
# we have 606 rows for above condition

all_trips %>% 
  filter(length_of_ride>86400)
# we have 495 rows for above condition

# we filter out these rows from all_trips dataframe
all_trips_2 <- all_trips %>% 
  filter(!(length_of_ride <0 | length_of_ride >86400))

# changed data 
glimpse(all_trips_2)


# checking if we have any duplicate ride_id
all_trips_2[duplicated(all_trips_2$ride_id),]
# we don't have any duplicate in data


# you need to install package which is useful for finding distance between end location and start location
library(geosphere)

# adding column ride_distance for finding distance between start location and end location by latitude and longitude
all_trips_2$ride_distance <- distGeo(matrix(c(all_trips_2$start_lng, all_trips_2$start_lat), ncol=2),
                                     matrix(c(all_trips_2$end_lng, all_trips_2$end_lat), ncol=2))
glimpse(all_trips_2)
summary(all_trips_2)

# for checking ride distance are reasonable
all_trips_2 %>% 
  arrange(-ride_distance)




# ANALYZE

# showing all trips by each day of week 
table(all_trips_2$day_of_week)

# ordered each day from Monday to Sunday for detailed analysis
all_trips_2$day_of_week <- 
  ordered(all_trips_2$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

# for analyzing number of ride through each day of week and also through membership status
all_trips_2 %>% 
  group_by(membership_status, day_of_week) %>% 
  summarise(number_of_ride = n(), .groups = 'drop') %>% 
  arrange(day_of_week)


# ordered for each month from September to August because we have data from that particular month
all_trips_2$month <- 
  ordered(all_trips_2$month, levels = c('08', '09', '10', '11', '12', '01', '02', '03', '04', '05', '06', '07'))

# for analyzing number of ride through for each month and also through membership status
all_trips_2 %>% 
  group_by(membership_status, month) %>% 
  summarize(number_of_ride = n(), .groups = 'drop') %>% 
  arrange(month) %>% 
  print(n=25)


# BY TIME
# aggregating for length_of_ride for each week for both casual and member
aggregate(all_trips_2$length_of_ride ~ all_trips_2$membership_status + all_trips_2$day_of_week, FUN = mean)

# aggregating for length_of_ride for for each month for both casual and member
aggregate(all_trips_2$length_of_ride ~ all_trips_2$membership_status + all_trips_2$month, FUN = mean)


# BY DISTANCE
# aggregating for ride_distance for each week for both casual and member
aggregate(all_trips_2$ride_distance ~ all_trips_2$membership_status + all_trips_2$day_of_week, FUN = mean)

# aggregating for ride_distance for for each month for both casual and member
aggregate(all_trips_2$ride_distance ~ all_trips_2$membership_status + all_trips_2$month, FUN = mean)


# seeing rides taken by membership_status
all_trips_2 %>% 
  group_by(membership_status) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')

# how long each type of bike used for
all_trips_2 %>% 
  group_by(rideable_type) %>% 
  summarize(distance_of_ride = mean(ride_distance), .groups = 'drop') %>% 
  arrange(rideable_type) 



# SHARE

# plotting 


# For showing number of ride taken by ride type
all_trips_2 %>%
  group_by(rideable_type) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = '', y = number_of_rides, fill = rideable_type)) + 
  geom_bar(width=1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = round((as.double(number_of_rides)/as.double(dim(all_trips_2)[1]))*100, digits=2)),
            position = position_stack(vjust = 0.5)) +
  labs(tag = "1.", title = "Percentage of bike type used by users", subtitle = "Data for Sept, 2021 - Aug, 2022" )

# For showing number of ride taken by membership status
all_trips_2 %>% 
  group_by(membership_status) %>% 
  summarise(number_of_ride = n(), .groups = 'drop') %>% 
  ggplot(aes(x="", y = number_of_ride, fill= membership_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = round((as.double(number_of_ride)/as.double(dim(all_trips_2)[1]))*100, digits = 2)),
            position = position_stack(vjust = 0.5)) +
  labs(tag = "2.", title = "Percentage of bike used by membership status", subtitle = "Data for Sept, 2021 - Aug, 2022" )


# Comparing number of ride on day for casual and member users
all_trips_2 %>% 
  group_by(membership_status, day_of_week) %>% 
  summarise(number_of_ride = n(), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = number_of_ride, fill = membership_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(tag = "3.", title = "Comparing number of ride on day for casual and member users", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Days of week", y = "Number of rides")

# Comparing number of ride for each month for casual and member users
all_trips_2 %>% 
  group_by(membership_status, month) %>% 
  summarise(number_of_ride = n(), .groups = 'drop') %>% 
  ggplot(aes(x = month, y = number_of_ride, fill = membership_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(tag = "4.", title = "Comparing number of ride for each month for casual and member users", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Month", y = "Number of rides")


# Comparing average time of ride on day for casual and member users
all_trips_2 %>% 
  group_by(membership_status, day_of_week) %>% 
  summarise(average_ride_time = mean(length_of_ride), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = average_ride_time/60, fill = membership_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(tag = "5.", title = "Comparing average ride time on days for casual and member users", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Day of week", y = "Average ride time(In minutes)")

# Comparing average time of ride for each month for casual and member users
all_trips_2 %>% 
  group_by(membership_status, month) %>% 
  summarise(average_ride_time = mean(length_of_ride), .groups = 'drop') %>% 
  ggplot(aes(x = month, y = average_ride_time/60, fill = membership_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(tag = "6.", title = "Comparing average ride time for each month for casual and member users", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Month", y = "Average ride time(In minutes)")


# Comparing average riding distance on day for casual and member users
all_trips_2 %>% 
  group_by(membership_status, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_distance), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = membership_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(tag = "7.", title = "Comparing average riding distance on day for casual and member users", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Day of week", y = "Ride distance(In meters)")

# Comparing average riding distance for each month for casual and member users
all_trips_2 %>% 
  group_by(membership_status, month) %>% 
  summarise(average_ride_length = mean(ride_distance), .groups = 'drop') %>% 
  ggplot(aes(x = month, y = average_ride_length, fill = membership_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(tag = "8.", title = "Comparing average riding distance for each month for casual and member users", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Month", y = "Ride distance(In meters")


# Number of rides vs. Ride distance(meters) for casual and member status
all_trips_2 %>% 
  group_by(membership_status) %>% 
  filter(ride_distance < 10000) %>% 
  ggplot(aes(x = ride_distance, fill = membership_status)) +
  geom_histogram() +
  labs(tags = "9.", title = "Number of rides vs. Ride distance(meters) for casual and member status", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Ride Distance(In meters)", y = "Number of rides")


# Bike used as per bike type
all_trips_2 %>% 
  group_by(membership_status) %>% 
  ggplot(aes(x = rideable_type, fill = membership_status)) +
  geom_histogram(stat = "count", position = "dodge") +
  labs(tags = "10.", title = "Bike used as per bike type", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Bike type", y = "Number of rides")


# Bike usage as per time duration 
all_trips_2 %>% 
  group_by(rideable_type) %>% 
  summarise(average_duration = mean(length_of_ride), .groups = 'drop') %>% 
  ggplot(aes(x = rideable_type, y = average_duration)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(tag = "11.", title = "Bike usage as per time duration", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Bike type", y = "Average ride time")

#Bike usage as per covered distance
all_trips_2 %>% 
  group_by(rideable_type) %>% 
  summarise(average_distance = mean(ride_distance), .groups = 'drop') %>% 
  ggplot(aes(x = rideable_type, y = average_distance)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(tag = "12.", title = "Bike usage as per covered distance", subtitle = "Data for Sept, 2021 - Aug, 2022",
       x = "Bike type", y = "Average ride distance")
