install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
lubridate(ggplot2)
library(dplyr)
library(tidyr)

getwd()
setwd("C:/Users/jolim/OneDrive/Documents")

feb_2021 <-read.csv(file.choose())
mar_2021 <-read.csv(file.choose())
apr_2021 <-read.csv(file.choose())
may_2021 <-read.csv(file.choose())
jun_2021 <-read.csv(file.choose())
jul_2021 <-read.csv(file.choose())
aug_2021 <-read.csv(file.choose())
sep_2021 <-read.csv(file.choose())
oct_2021 <-read.csv(file.choose())
nov_2021 <-read.csv(file.choose())
dec_2021 <-read.csv(file.choose())
jan_2022 <-read.csv(file.choose())

q1_bikes <- bind_rows(feb_2021, mar_2021, apr_2021, may_2021)
q2_bikes <- bind_rows(jun_2021, jul_2021, aug_2021, sep_2021)
q3_bikes <- bind_rows(oct_2021, nov_2021, dec_2021, jan_2022)

q_bikes <- bind_rows(q1_bikes, q2_bikes, q3_bikes) 

q_bikes <- q_bikes %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
         
         colnames(q_bikes)
         nrow(q_bikes)
         dim(q_bikes)
         head(q_bikes)
         str(q_bikes)
         summary(q_bikes)
         
         colmn <- paste("S", 1:2)
         colmn2 <- paste("E", 1:2)
         
         q_bikes <- separate(data = q_bikes,
             col = started_at,
             sep = " ",
             into = colmn,
             remove = FALSE)
         
         q_bikes <- separate(data = q_bikes,
                             col = ended_at,
                             sep = " ",
                             into = colmn2,
                             remove = FALSE)
         
         q_bikes$date <- as.Date(q_bikes$started_at, "%m/%d/%Y %H:%M") 
         q_bikes$month <- format(as.Date(q_bikes$date), "%m")
         q_bikes$day <- format(as.Date(q_bikes$date), "%d")
         q_bikes$year <- format(as.Date(q_bikes$date), "%Y")
         q_bikes$day_of_week <- format(as.Date(q_bikes$date), "%A")
         q_bikes$`S 2` <- strptime(q_bikes$`S 2`, format = "%H:%M", tz = "EST")
         q_bikes$`E 2` <- strptime(q_bikes$`E 2`, format = "%H:%M", tz = "EST")
         q_bikes$ride_length <- difftime(q_bikes$`S 2`, q_bikes$`E 2`) #This is where the problem starts
         q_bikes$ride_length <- abs(q_bikes$ride_length)
         colnames(q_bikes)
           
           str(q_bikes)
         
         is.factor(q_bikes$ride_length)
         q_bikes$ride_length <- as.numeric(as.character(q_bikes$ride_length))
         is.numeric(q_bikes$ride_length)
         
         q_bikes_v2 <- q_bikes[!(q_bikes$start_station_name == "HQ QR" | q_bikes$ride_length<0),]
         
         mean(q_bikes_v2$ride_length)
         median(q_bikes_v2$ride_length)
         max(q_bikes_v2$ride_length)
         min(q_bikes_v2$ride_length)
         
         summary(q_bikes_v2$ride_length)
         
         aggregate(q_bikes_v2$ride_length ~ q_bikes_v2$member_casual, FUN = mean)
         aggregate(q_bikes_v2$ride_length ~ q_bikes_v2$member_casual, FUN = median)
         aggregate(q_bikes_v2$ride_length ~ q_bikes_v2$member_casual, FUN = max)
         aggregate(q_bikes_v2$ride_length ~ q_bikes_v2$member_casual, FUN = min)
         
         aggregate(q_bikes_v2$ride_length ~ q_bikes_v2$member_casual + q_bikes_v2$day_of_week, FUN = mean)
         
         q_bikes_v2$day_of_week <- ordered(q_bikes_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
         
         aggregate(q_bikes_v2$ride_length ~ q_bikes_v2$member_casual + q_bikes_v2$day_of_week, FUN = mean)
         
         q_bikes_v2 %>% 
           mutate(day_of_week = wday(`S 2`, label = TRUE)) %>%  
         group_by(member_casual, day_of_week) %>%  
           summarise(number_of_rides = n()							
                     ,average_duration = mean(ride_length)) %>% 	
           arrange(member_casual, day_of_week)	
         
         q_bikes_v2 %>% 
           mutate(day_of_week = wday(`S`, label = TRUE)) %>% 
           group_by(member_casual, day_of_week) %>% 
           summarise(number_of_rides = n()
                     ,average_duration = mean(ride_length)) %>% 
           arrange(member_casual, day_of_week)  %>% 
           ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
           geom_col(position = "dodge")
         
         counts <- aggregate(q_bikes_v2$ride_length ~ q_bikes_v2$member_casual + q_bikes_v2$day_of_week, FUN = mean)
         write.csv(counts, file = 'C:/Users/jolim/Dropbox/My PC (DESKTOP-T3NGS8B)/Downloads/Google Certification Case Study/output.csv')
         