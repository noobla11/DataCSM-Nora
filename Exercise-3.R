# HOBO Exercise-1
# DataCSM 2020/2021
# Nora Obladen
# nano.broleda@gmail.com

# HOBO Nr: 10350017

#set working directory
setwd("~/Documents/Studium/Master/4_Data Collection/Data_Collection_Nora Obladen/01_data")

#start with a clean workspace
rm(list = ls()) 

options(pillar.sigfig=5) #how many digits do I want to see in a tibble?

#load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(magrittr) #set_colnames
library(zoo)

#import data
HOBOhour <- read_csv("10350017_Th.csv")
head(HOBOhour)

#1.1 Mean temperatures TAVG, TD, TN---------------------------------------------
mean(HOBOhour$th)

HOBOhour <- HOBOhour %>% 
  mutate(date = date(date_time),
         hour = hour(date_time)) %>% 
  mutate(day_night = ifelse(hour >= 06 & hour <= 17, "day","night"))

HOBOhour %>% 
  group_by(day_night) %>% 
  summarise(mean(th)) %>% 
  ungroup()

5.3790-4.5351

#t_avg  (mean temparture)                   = 4.957
#t_d    (mean day temparature)              = 5.379
#t_n    (mean night temparature)            = 4.535
#t_nd   (Difference -> t_d-t_n)             = 0.844


#1.2 Mean daily amplitude t_amp-------------------------------------------------
day_temp <- HOBOhour %>% 
  group_by(date) %>% 
  summarise(t_max = max(th),
            t_min = min(th))%>%
  mutate(t_dif = t_max-t_min)
  ungroup()
  
mean(day_temp$t_dif)

#t_amp  (Mean daily temperature amplitude)  = 4.162

#1.3 Most rapid temperature change TΔ6h-----------------------------------------
dif <- function(x) abs(diff(range(x)))

test <-HOBOhour %>%  
  mutate(t06 = rollapply(th,
                         width = 6,
                         FUN = dif, 
                         fill = 0,
                         align = "left"))

max(test$t06)

#t_6h (Most rapid temperature change in 6 hours) = 6.514

#1.7 Fraction of NA-Values fNA -------------------------------------------------
#import data
HOBO_NA <- read_csv("10350017_NA.csv")
head(HOBO_NA)

HOBO_NA <- HOBO_NA %>% 
  mutate(n_na = if_else(is.na(temp),1,0))

mean(HOBO_NA$n_na)

#f_na  (Fraction of missing values)  = 0.03968254 = 0.04

#2.0 Light intensity indices (with 10-min series)-------------------------------
#import data
HOBO_lux <- read_csv("10350017.csv")
head(HOBO_lux)

HOBO_lux <- HOBO_lux %>% 
  mutate(int = if_else(lux == 0, "dark", "light"))

HOBO_lux %>% 
  group_by(int) %>% 
  summarise(mean(lux)) %>% 
  ungroup()

HOBO_lux <- HOBO_lux %>% 
  mutate(date = date(date_time),
         minute = minute(date_time)) %>% 
  group_by(date, minute) %>% 
  summarise(mean(lux)) %>% 
  ungroup()
  


#l_avg 	(Mean light intensity	lux)                      = 2511.5
#l_max	(Hour:Minute of maximum light intensity	hh:mm)  =