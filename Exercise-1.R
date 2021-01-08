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

#check temperature values
HOBOcheck <- HOBOfull %>% 
  mutate(Flag = case_when((temp < -20) ~ FALSE,
                          (temp < 70 & temp > -20) ~ TRUE,
                          (temp > 70) ~ FALSE))
