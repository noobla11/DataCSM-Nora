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

HOBOmax <- HOBOhour %>%  
  mutate(t06 = rollapply(th,
                         width = 6,
                         FUN = dif, 
                         fill = 0,
                         align = "left"))

max(HOBOmax$t06)

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
  mutate(hour_minute = format(date_time, format = "%H:%M:%S"),
         hour = hour(date_time),
         minute = minute(date_time),
         date = date(date_time))

HOBO_lux %>% 
  group_by(hour_minute) %>% 
  summarise(mean_lux = mean(lux)) %>% 
  arrange(desc(mean_lux))

#OR
HOBO_lux %>% 
  group_by(hour, minute) %>% 
  summarise(mean_lux = mean(lux)) %>% 
  arrange(desc(mean_lux))

#l_avg 	(Mean light intensity	lux)                      = 2511.5
#l_max	(Hour:Minute of maximum light intensity	hh:mm)  = 12:30


#4. Comparison with a long-term average-----------------------------------------
#import the data 

DWD_long <- read_delim("air_temp_19510101_20191231_01443.txt", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

DWD_lon <- DWD_long %>%
  select(MESS_DATUM, TT_TU) %>% 
  set_colnames(c("date_time","th")) %>% 
  mutate(date_time = parse_date_time(date_time, orders = "Ymdh")) %>% 
  mutate(day = day(date_time),
         month = month(date_time),
         year = year(date_time)) %>% 
  filter(year(date_time)>=1999 & year(date_time)<=2019) #to have only 20 years

DWD_lon_trunc <- DWD_lon %>% 
  group_by(year) %>% 
  filter(month(date_time)==11 & day(date_time)== 30 | 
        month(date_time)== 12 & day(date_time) >=1 & day(date_time) < 21) %>% #Leons solution!  
  ungroup()

DWD_mean <- DWD_lon_trunc %>% 
  select("date_time","th","year") %>% 
  group_by(year) %>% 
  summarise(mean = mean(th))






