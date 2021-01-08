# HOBO Exercise-1
# DataCSM 2020/2021
# Nora Obladen
# nano.broleda@gmail.com

# HOBO Nr: 10350017

#set working directory
setwd("~/Documents/Studium/Master/4_Data Collection/Data_Collection_Nora Obladen/01_data")

#start with a clean workspace
rm(list = ls()) 

#load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(magrittr) #set_colnames

#import data
data <- read_csv("10350017 Original.csv", skip = 1)

#dublicate and select data
HOBO <- data %>%
  select(1:4) %>%
  set_colnames(c("id","date_time","temp","lux"))

#change date_time to an appropriate format
HOBO <- HOBO %>%
  mutate(date_time = parse_date_time(date_time, orders = "dmY HMS")) 

#extract sequence and add new id
HOBOmin <- HOBO %>%
  select(date_time,temp,lux) %>% 
  filter(between(ymd_hms(date_time), 
                 ymd_hms('2020-11-30 00:00:00'), 
                 ymd_hms('2020-12-20 23:50:00')))%>%
  add_column(id = 1:3024, .before = T)

#write.csv(HOBOmin, file = "10350017.csv", row.names = F)

#separate data for later use 
HOBOfull <- HOBOmin %>% 
  mutate(date = date(date_time),
         hour = hour(date_time),
         minute = minute(date_time)) 

#select only hourly data 
HOBOhour <- HOBOfull %>%
  filter(minute == 0) %>%
  select(date_time,temp,lux,date,hour,minute) %>%
  add_column(id = 1:504, .before = T)

#quality check
#check the temperature range (qc1) -> should be between -20 and 70 °C
HOBOcheck1 <- HOBOfull %>% 
  mutate(qc1 = case_when((temp < -20) ~ 1,
                          (temp < 70 & temp > -20) ~ 0,
                          (temp > 70) ~ 1))

#check the difference to the previous data point (qc2) -> should be below 1 °C
HOBOcheck2 <- HOBOcheck1 %>%
  mutate(qc2 = temp - lag(temp,1))%>% 
  mutate(qc2 = case_when((qc2 >= 1)~1,
                          (qc2 < 1)~0))

#check the persistence of the data (qc3) -> should be different each hour
HOBOcheck3 <- HOBOcheck2 %>%
  mutate(qc3 = case_when((lag(temp,1) == temp &
                          lag(temp,2) == temp &
                          lag(temp,3) == temp &
                          lag(temp,4) == temp &
                          lag(temp,5) == temp)~1,
                         (lag(temp,1) != temp &
                          lag(temp,2) != temp &
                          lag(temp,3) != temp &
                          lag(temp,4) != temp &
                          lag(temp,5) != temp)~0))

?if_else
#check the 
HOBOcheck <- HOBOcheck %>%
  mutate(qc4 = lux - lag(temp,+1,-1))%>% 
  mutate(qc4 = case_when((lux > 20000)~1,
                         (qc2 < 1)~0))





