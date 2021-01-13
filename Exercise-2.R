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

# -1- QUALITY CONTROL ----------------------------------------------------------
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

# -2- FLAGGING -----------------------------------------------------------------
#summarise the quality check values 
HOBOtotal <- HOBOcheck3 %>% 
  mutate(qc_total= qc1+qc2+qc3) %>% 
  group_by(date, hour) %>% 
  mutate(sum_h = sum(qc_total)) %>% 
  mutate(flag = if_else(sum_h >= 2,1,0)) %>% 
  ungroup() %>% 
  mutate(temp_new = if_else(flag == 0, temp, 1))

HOBOhour <- HOBOtotal %>% 
  group_by(date, hour) %>%
  mutate(avg_t = mean(temp_new)) %>% 
  ungroup() %>% 
  select("date_time","minute","avg_t") %>% 
  filter(minute == 0) %>% 
  select("date_time","avg_t") %>% 
  rename(temp = avg_t) %>% 
  add_column(origin = "H") %>% 
  add_column(id = "HOBO")

HOBOhour[HOBOhour == 1] <- NA
#write.csv(HOBOhour, file = "10350017_NA.csv", row.names = F, quote = F)


#HOBOhour <- HOBOhour %>%
  #mutate(temp =round(temp, digits = 1))

# -3- GAP FILLING --------------------------------------------------------------
#import reference data ---------------------------------------------------------
DWD <- read_delim("air_temp_01443_akt.txt", 
                  ";", escape_double = FALSE, trim_ws = TRUE)

DWD_Urban <- read_delim("air_temp_13667_akt.txt", 
                  ";", escape_double = FALSE, trim_ws = TRUE)

Garden <- read_csv("air_temp_Garten_akt.csv")
WBI <- read.csv("air_temp_WBI_akt.csv", sep = ";", dec = ",")

#manipulate date
DWD <- DWD %>%
  select(MESS_DATUM, TT_TU) %>% 
  set_colnames(c("date_time","temp")) %>% 
  mutate(date_time = parse_date_time(date_time, orders = "YmdH")) 

DWD_Urban <- DWD_Urban %>%
  select(MESS_DATUM, LUFTTEMPERATUR) %>% 
  set_colnames(c("date_time","temp")) %>% 
  mutate(date_time = parse_date_time(date_time, orders = "YmdH")) 

Garden <- Garden %>%
  select(Lokalzeit, `Lufttemperatur (°C)`) %>% 
  set_colnames(c("date_time","temp")) %>% 
  mutate(date_time = parse_date_time(date_time, orders = "Ymd HMS")) 

WBI <- WBI %>%
  unite(Tag, Stunde,col = "Zeit", sep = "") %>% 
  select(Zeit, AVG_TA200) %>% 
  set_colnames(c("date_time","temp")) %>% 
  mutate(date_time = parse_date_time(date_time, orders = "dmY HM"))

#extract sequence and add new id
DWD_trunc <- DWD %>%
  select(date_time,temp) %>% 
  filter(between(ymd_hms(date_time), 
                 ymd_hms('2020-11-30 00:00:00'), 
                 ymd_hms('2020-12-20 23:50:00')))%>%
  add_column(id = "DWD", .before = T) %>% 
  add_column(origin = "R")

DWD_Urban_trunc <- DWD_Urban %>%
  select(date_time,temp) %>% 
  filter(between(ymd_hms(date_time), 
                 ymd_hms('2020-11-30 00:00:00'), 
                 ymd_hms('2020-12-20 23:50:00')))%>%
  add_column(id = "Urban", .before = T) %>% 
  add_column(origin = "R")

Garden_trunc <- Garden %>%
  select(date_time,temp) %>% 
  filter(between(ymd_hms(date_time), 
                 ymd_hms('2020-11-30 00:00:00'), 
                 ymd_hms('2020-12-20 23:50:00')))%>%
  add_column(id = "Garden", .before = T) %>% 
  add_column(origin = "R")

WBI_trunc <- WBI %>%
  select(date_time,temp) %>% 
  filter(between(ymd_hms(date_time), 
                 ymd_hms('2020-11-30 00:00:00'), 
                 ymd_hms('2020-12-20 23:50:00')))%>%
  add_column(id = "WBI", .before = T) %>% 
  add_column(origin = "R")


#join reference date -----------------------------------------------------------
ref_data <- DWD_trunc %>% 
  full_join(Garden_trunc) %>% 
  full_join(DWD_Urban_trunc) %>% 
  full_join(WBI_trunc)

all <- ref_data %>% 
  full_join(HOBOhour)

#show reference date 
ggplot(data = all, 
        aes(x = date_time, y = temp)) +
  geom_line(aes(group = id, colour = id))

head(all)
tail(all)

lm_DWD <- lm(HOBOhour$temp ~ DWD_trunc$temp)
summary(lm_DWD)
#Multiple R-squared:  0.8794

lm_Urban <- lm(HOBOhour$temp ~ DWD_Urban_trunc$temp)
summary(lm_Urban)
#Multiple R-squared:  0.978

lm_Garden <- lm(HOBOhour$temp ~ Garden_trunc$temp)
summary(lm_Garden)
#Multiple R-squared:  0.9569

lm_WBI <- lm(HOBOhour$temp ~ WBI_trunc$temp)
summary(lm_WBI)
#Multiple R-squared:  0.9791 -> as this is the highest value this is used for the prediction


#predict values with the WBI data and create new data_frame
WBI_df <- data.frame(date_time = WBI_trunc$date_time, temp = WBI_trunc$temp)

HOBO_Th <- HOBOhour %>% 
  mutate(origin = ifelse(is.na(temp),"R","H")) %>% 
  mutate(th = ifelse(is.na(temp),predict(lm_WBI,newdata = WBI_df),temp)) %>% 
  mutate(th = round(th, 3)) %>% 
  select(date_time, th, origin) %>% 
  rename(date = date_time)

#write.csv(HOBO_Th, file = "10350017_Th.csv", row.names = F, quote = F)
