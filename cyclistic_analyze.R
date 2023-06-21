# install packages and load it
install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)

# import the tripsCombined_cleaned.csv file
tripsCombined <- read.csv('tripsCombined_cleaned.csv')

# I want to find the percentage of the riders type
bikers <- tripsCombined %>% count(member_casual) %>% group_by(member_casual)
print(bikers)

ggplot(bikers, aes(x = "", y = n/sum(n)*100, fill = member_casual)) +
  geom_col(color = 'white') +
  geom_text(aes(label = paste0(round(n/sum(n)*100,3), "%")),
            position = position_stack(vjust = 0.5))  +
  coord_polar(theta = "y", start=1) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Percentage of Bikers Type',
       fill = 'Member Type')

ggsave("Chart/BikersPercentage.png", dpi = 1000)

# I want to find the relation between rideable_type and member_casual
bike_type_casual <- print(count(filter(tripsCombined,member_casual=='casual'),rideable_type))
bike_type_member <- print(count(filter(tripsCombined,member_casual=='member'),rideable_type))

ggplot(data=tripsCombined) + 
  geom_bar(mapping=aes(x=rideable_type, fill=member_casual), position='dodge') +
  labs(title = 'Bike Type vs Member Type',
       x = 'Bike Type',
       y = 'Total trips',
       fill = 'Member Type')

ggsave("Chart/BikeType_MemberType.png", dpi = 1000)

# I want to find the relation between the day of trips and member_casual
casual_daily_trips <- print(count(filter(tripsCombined,member_casual=='casual'),day))
member_daily_trips <- print(count(filter(tripsCombined,member_casual=='member'),day))


tripsCombined$day <- factor(tripsCombined$day, levels= c("Sun", "Mon", 
                                           "Tue", "Wed", "Thu", "Fri", "Sat"))

ggplot(data=tripsCombined) + 
  geom_bar(mapping=aes(x=day, fill=member_casual), position='dodge') +
  labs(title = 'Rides per Day vs Member Type',
       x = 'Day of Week',
       y = 'Total trips',
       fill = 'Member Type')

ggsave("Chart/RidesPerDay_MemberType.png", dpi = 1000)

# I want to find the relation between bike type and member_casual per day
ggplot(data=tripsCombined) + 
  geom_bar(mapping=aes(x=day, fill=rideable_type), position='stack') +
  labs(title = 'Bike Type per Day by Member Type',
       x = 'Start Hour',
       y = 'Total trips',
       fill = 'Member Type') +
  facet_wrap(~member_casual)

ggsave("Chart/BikeTypeperDay_MemberType.png", dpi = 1000)

# I want to find the relation between the total trips per month and member_casual
casual_monthly_trips <- print(count(filter(tripsCombined,member_casual=='casual'),month))
member_monthly_trips <- print(count(filter(tripsCombined,member_casual=='member'),month))


tripsCombined$month <- factor(tripsCombined$month, levels= c("Jan", "Feb", 
                                               "Mar", "Apr", "May", "Jun", "Jul",
                                               "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(data=tripsCombined) + 
  geom_bar(mapping=aes(x=month, fill=member_casual), position='dodge') +
  labs(title = 'Rides per Month by Member Type',
       x = 'Month',
       y = 'Total trips',
       fill = 'Member Type')

ggsave("Chart/RidesPerMonth_MemberType.png", dpi = 1000)

# I want to find the relation between start hour and member_casual
casual_start_hour <- print(count(filter(tripsCombined,member_casual=='casual'),start_hour))
member_start_hour <- print(count(filter(tripsCombined,member_casual=='member'),start_hour))

ggplot(data=tripsCombined) + 
  geom_bar(mapping=aes(x=start_hour, fill=member_casual), position='dodge') +
  labs(title = 'Start Hour by Member Type',
       x = 'Start Hour',
       y = 'Total trips',
       fill = 'Member Type')

ggsave("Chart/StartHour_MemberType.png", dpi = 1000)

# I want to find the relation between start hour per day and member_casual
ggplot(data=tripsCombined) + 
  geom_bar(mapping=aes(x=start_hour, fill=member_casual), position='stack') +
  labs(title = 'Start Hour per Day by Member Type',
       x = 'Start Hour',
       y = 'Total trips',
       fill = 'Member Type') +
  facet_wrap(~ day)

ggsave("Chart/StartHourperDay_MemberType.png", dpi = 1000)

# I want to find the relation between start hour on weekdays and member_casual
ggplot(data = filter(tripsCombined, !(day %in% c('Sun','Sat')))) + 
  geom_line(mapping = aes(x = start_hour,
                          color=member_casual,
                          group=member_casual),
            stat = 'count') +
  labs(title = 'Start Hour on Weekdays by Member Type',
       x = 'Start Hour',
       y = 'Total Trips',
       color = 'Member Type')

ggsave("Chart/StartHourOnWeekdays_MemberType.png", dpi = 1000)

# I want to find the relation between start hour on weekends and member_casual
ggplot(data = filter(tripsCombined, (day %in% c('Sun','Sat')))) + 
  geom_line(mapping = aes(x = start_hour,
                          color=member_casual,
                          group=member_casual),
            stat = 'count') +
  labs(title = 'Start Hour on Weekends by Member Type',
       x = 'Start Hour',
       y = 'Total Trips',
       color = 'Member Type')

ggsave("Chart/StartHourOnWeekends_MemberType.png", dpi = 1000)

# I want to find the relation between avg trip duration and member_casual
avg_durations <- aggregate(trip_durations~member_casual + day, tripsCombined, FUN = mean)
print(avg_durations)
ggplot(data = avg_durations) + 
  geom_line(mapping = aes(x = day , y = as.numeric(trip_durations),
                          color=member_casual,
                          group=member_casual)) +
  labs(title = 'Average Trip Durations per Day by Member Type',
       x = 'Day of Week',
       y = 'Avg Trip Durations',
       color = 'Member Type')

ggsave("Chart/TripsDurationsPerDay_MemberType.png", dpi = 1000)


casual <- tripsCombined %>% filter(member_casual=='casual')
member <- tripsCombined %>% filter(member_casual=='member')

# top 10 most popular start stations for casual riders 
casual %>% count(start_station_name, sort = T) %>% head(10)
# top 10 most popular end stations for casual riders 
casual %>% count(end_station_name, sort = T) %>% head(10)

# top 10 most popular start stations for member riders 
member %>% count(start_station_name, sort = T) %>% head(10)
# top 10 most popular start stations for member riders 
member %>% count(end_station_name, sort = T) %>% head(10)
