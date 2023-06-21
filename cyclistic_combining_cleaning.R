# install packages and load it
install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)

# import data
tripMay22 <- read.csv("2022_05.csv")
tripJun22 <- read.csv("2022_06.csv")
tripJul22 <- read.csv("2022_07.csv")
tripAug22 <- read.csv("2022_08.csv")
tripSep22 <- read.csv("2022_09.csv")
tripOct22 <- read.csv("2022_10.csv")
tripNov22 <- read.csv("2022_11.csv")
tripDec22 <- read.csv("2022_12.csv")
tripJan23 <- read.csv("2023_01.csv")
tripFeb23 <- read.csv("2023_02.csv")
tripMar23 <- read.csv("2023_03.csv")
tripApr23 <- read.csv("2023_04.csv")


# checking the data type integrity
str(tripMay22)
str(tripJun22)
str(tripJul22)
str(tripAug22)
str(tripSep22)
str(tripOct22)
str(tripNov22)
str(tripDec22)
str(tripJan23)
str(tripFeb23)
str(tripMar23)
str(tripApr23)


# combining all data (total data = 5.859.061)
tripsCombined <- bind_rows(tripMay22,tripJun22,tripJul22, tripAug22, tripSep22,
                           tripOct22, tripNov22, tripDec22, tripJan23, tripFeb23,
                           tripMar23, tripApr23)

#
# DATA CLEANING PROCESS
#
# ensure there is only characters, numbers, and underscores in the names
tripsCombined <- clean_names(tripsCombined)

# assign all empty strings into NA value
tripsCombined[tripsCombined == ""] <- NA

# drop all rows with NA value (total data = 4.533.999)
tripsCombined <- tripsCombined[complete.cases(tripsCombined),]

# convert the started_at and ended_at variable into date format
tripsCombined$started_at <- ymd_hms(tripsCombined$started_at)
tripsCombined$ended_at <- ymd_hms(tripsCombined$ended_at)
str(tripsCombined)

# adding new column for trip_durations
tripsCombined$trip_durations <- difftime(tripsCombined$ended_at,
                                         tripsCombined$started_at, units = 'secs')

# drop all rows with negative value and over 86400 secs of trip_durations
tripsCombined <- tripsCombined[!(tripsCombined$trip_durations < 0
                                 & tripsCombined$trip_durations > 86400 ),]

# adding new column to extract the day when customers started to use Cyclistic
tripsCombined$day <- wday(tripsCombined$started_at, label = T, abbr = T)
# adding new column to extract the month when customers started to use Cyclistic
tripsCombined$month <- month.abb[month(tripsCombined$started_at)]
# adding new column to extract the start hour when customers started to use Cyclistic
tripsCombined$start_hour <- hour(tripsCombined$started_at)

# After I added 3 new columns, I did a double check to find if there is any 
# inconsistent value for the day, month, and start_hour using View(). 
# Eventually, the sample is cleaned so I can continue to analyze the data

# save tripsCombined in csv format
write.csv(tripsCombined, file = "tripsCombined_cleaned.csv", row.names = FALSE)
View(tripsCombined)
