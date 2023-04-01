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

returndata <- data %>% 
  filter_index("2021-04-01") %>% 
  filter(type == "StepCount") %>% 
  group_by_key() %>% 
  index_by(hour = ~ hour(.)) %>% 
  summarise(count = sum(value)) %>% 
  fill_gaps(.full = TRUE, count = 0)



x <- tibble(type = "StepCount", hour = 1:24)
z <- x %>% 
      left_join(returndata) %>% 
      mutate(count = coalesce(count, 0))

ggplot(z, aes(x = hour, y  = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = 1:23) +

  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 15)) +
  labs(title = "Title") 



returndata <- data %>% 
  filter_index("2017-04-01" ~ "2021-04-03") %>%
  mutate(day = wday(datetime,label = TRUE)) %>% 
  filter(type == "StepCount") %>% 
  group_by_key() %>% 
  index_by(hour = ~ hour(.)) %>%
  group_by(day) %>% 
  summarise(average = mean(value)) %>% 
  fill_gaps(.full = TRUE, average = 0)

x <- tibble(day = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
y <- tibble(hour = 0:23)
template <- x %>% 
     full_join(y, by = character())

z1 <- template %>%
  left_join(returndata) 



z1$day <- ordered(z1$day, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
