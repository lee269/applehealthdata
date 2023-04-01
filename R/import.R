# from https://taraskaduk.com/posts/2019-03-23-apple-health/

# https://sharla.party/post/2021-03-12-big-bud-press/


library(XML)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(here)

# First backup the existing data
records <- readRDS(here("data/tidy/records.rds"))
filename <- paste0("records-", Sys.time(), ".rds")
saveRDS(records, here("data", "tidy", filename))

# Unzip and overwrite existing raw data 
path <- here("/data/raw")
zip <- paste(path, 'export.zip', sep = '/')
unzip(zip, exdir = path)


# Parse records and convert to data frame
xml <- xmlParse(paste0(path, '/apple_health_export/export.xml'))
summary(xml)

df_record <-   XML:::xmlAttrsToDataFrame(xml["//Record"])

# df_activity <- XML:::xmlAttrsToDataFrame(xml["//ActivitySummary"])
# df_workout <-  XML:::xmlAttrsToDataFrame(xml["//Workout"])
# df_clinical <- XML:::xmlAttrsToDataFrame(xml["//ClinicalRecord"])
# df_location <- XML:::xmlAttrsToDataFrame(xml["//Location"]) %>% 
#   mutate(latitude = as.numeric(as.character(latitude)),
#          longitude = as.numeric(as.character(longitude)))

# Srtip off the +0100 at the end of dates because it is messing up the
# conversion from text to date
df <- df_record %>%
  mutate(device = gsub(".*(name:)|,.*", "",device),
         value = as.numeric(as.character(value)),
         startDate = ymd_hms(str_remove(startDate, "\\+0100")),
         endDate = ymd_hms(str_remove(endDate, "\\+0100")),
         date = date(endDate),
         year = year(endDate),
         month = month(endDate),
         day = day(endDate),
         yday = yday(endDate),
         wday = wday(endDate),
         hour = hour(endDate),
         minute = minute(endDate),
         second = second(endDate),
         type = str_remove(type, "HKQuantityTypeIdentifier")
  )

# Cleanup - getting rid of strange records

#There is a duplicate enddate record at this exact time for steps and distance
# odd1 <- df %>% 
#   filter(type == "StepCount" | type == "DistanceWalkingRunning",
#          endDate == "2017-04-17 15:47:26")
# 
# But here is a more generic way of doing it

odd1 <- df %>% 
  group_by(type, endDate) %>% 
  filter(n() >1)

# There are some odd stepcounts that seem to have been collected over days. We
# look for records which have an interval of an hour or less (most seem to be
# collected over minutes or seconds)
odd2 <- df %>% 
  mutate(interval = endDate-startDate) %>% 
  filter(type == "StepCount", interval >3600) %>% 
  select(-interval)

# gather up the records to delete
odds <- bind_rows(odd1, odd2)

# delete and save the master data
df <- df %>% anti_join(odds)
saveRDS(df, here("data", "tidy", "records.rds"))


