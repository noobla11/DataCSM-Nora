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
library(zoo)

#import data
HOBOmin <- read_csv("10350017.csv")

#separate data for later use 
HOBOfull <- HOBOmin %>% 
  mutate(date = date(date_time),
         hour = hour(date_time),
         minute = minute(date_time)) 

#quality check
#check the temperature range (qc1) -> should be between -20 and 70 °C
HOBOcheck1 <- HOBOfull %>% 
  mutate(qc1 = case_when((temp < -20 & temp > 70) ~ 1,
                          (temp < 70 & temp > -20) ~ 0))

#check the difference to the previous data point (qc2) -> should be below 1 °C
HOBOcheck2 <- HOBOcheck1 %>%
  mutate(qc2 = temp - lag(temp,1)) %>% 
  mutate(qc2 = case_when((qc2 >= 1)~1,
                          (qc2 < 1)~0)) 

#also possible with if_else instead:
#HOBOcheck2 <- HOBOcheck1 %>%
  #mutate(qc2 = temp - lag(temp,1)) %>% 
  #mutate(qc2 = if_else(qc2 >= 1,1,0))

#check the persistence of the data (qc3) -> should be different each hour
HOBOcheck3 <- HOBOcheck2 %>%
  mutate(diff = rollapply(temp, 
                         width = 6, 
                         FUN = sd, 
                         fill = NA,
                         align = "right"))

HOBOcheck3 <- HOBOcheck3 %>%
  mutate(qc3 = case_when((diff == 0) ~ 1,
                         (diff != 0) ~ 0))

#Replace NA with 1 to flag 
HOBOcheck3$qc2 <- HOBOcheck3$qc2 %>%
  replace_na(1)

HOBOcheck3$qc3 <- HOBOcheck3$qc3 %>%
  replace_na(1)
  
#check the maximum lux value
max(HOBOcheck3$lux) 
#11022.3 is the maximum value therefore below the threshold of 20000

#summarise the quality check values 
HOBOtotal <- HOBOcheck3 %>% 
  mutate(qc_total= qc1+qc2+qc3) 






