library(tidyverse)
library(ggplot2)
library(here)
library(tsibble)
library(lubridate)

records <- readRDS(here("data/tidy/records.rds"))

data <- records %>% 
  mutate(datetime = make_datetime(year, month, day, hour, minute, second)) %>% 
  select(datetime, type, unit, value) %>% 
  as_tsibble(key = type,index = datetime,  regular = FALSE)        


#miles per month
dist <- data %>% 
  filter(type == "DistanceWalkingRunning") %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = mday(datetime))  %>% 
  group_by_key() %>% 
  index_by(month = ~ month(.)) %>% 
  group_by(year, month) %>% 
  summarise(distance = sum(value))

x <- data %>% 
  filter(type == "StepCount") %>% 
  filter(year(datetime) == 2020) %>% 
  mutate(month = month(datetime)) %>% 
  mutate(day = mday(datetime)) %>% 
  group_by_key() %>% 
  index_by(day = ~ day(.)) %>% 
  group_by(month, day) %>% 
  summarise(total = sum(value))


x %>% ggplot(aes(x = day, y = month)) +
      geom_tile(aes(fill = total)) +
  scale_fill_gradient(low="white", high="mediumpurple")








x <- data %>% 
     mutate(day = wday(datetime))


x <- data %>% 
      filter_index("2018-04") %>%
      mutate(day = wday(datetime)) %>% 
      filter(type == "StepCount") %>% 
      group_by_key() %>% 
      index_by(hour = ~ hour(.)) %>%
      group_by(day) %>% 
      summarise(count = sum(value)) %>% 
      fill_gaps(.full = TRUE, count = 0)
 
x %>% ggplot(aes(x = hour, y = day, fill = count)) +
      geom_tile() +
      scale_fill_gradient(low="white", high="red")

x %>% ggplot(aes(x = hour, y  = count, fill = type)) +
      geom_bar(stat = "identity", position = "dodge")


ym <- data %>%
  group_by_key() %>% 
  index_by(ym = ~ as_date(.)) %>% 
  summarise(count = sum(value))

ym %>% 
  filter(type == "DistanceWalkingRunning") %>% 
  ggplot(aes(x = ym, y = count)) +
  geom_line()


ym %>% 
  filter(type == "DistanceWalkingRunning") %>% 
  ggplot(aes(x = ym, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge")


steps <- records %>% 
  filter(type == "DistanceWalkingRunning") %>% 
  mutate(datetime = make_datetime(year, month, day, hour, minute, second)) %>% 
  select(datetime, value) %>% 
  as_tsibble(regular = FALSE)


ym <- steps %>%
  group_by_key() %>% 
  index_by(ym = ~ yearmonth(.)) %>% 
  summarise(count = sum(value))

ym %>% 
  # filter(count <= 50000) %>% 
  ggplot(aes(x = ym, y = count))+ 
  geom_bar(stat = "identity") 


hr <- steps %>%
  group_by_key() %>% 
  index_by(ym = ~hour(.)) %>% 
  summarise(count = mean(value))


hr %>% 
  # filter(count <= 50000) %>% 
  ggplot(aes(x = ym, y = count))+ 
  geom_bar(stat = "identity") 


x <- records %>% 
  group_by(type, endDate) %>% 
  filter(n() >1)
