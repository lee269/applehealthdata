library("here")
  
update_available <- function(rootfolder = here::here()){
  update = FALSE
  rawfile <- paste0(rootfolder, "/data/raw/export.zip")  
  archivefile <- paste0(rootfolder, "/data/tidy/records.rds")
  
  if(!file.exists(rawfile)) {stop(paste(rawfile, "is missing"))}
  if(!file.exists(archivefile)) {stop(paste(archivefile, "is missing"))}
  
  raw <- file.info(rawfile)$mtime
  archive <- file.info(archivefile)$mtime
  if(raw > archive) {update = TRUE}

  return(update)  
}


backup_archive <- function(rootfolder = here::here()){
  
  archivefile <- paste0(rootfolder, "/data/tidy/records.rds")
  if(!file.exists(archivefile)) {stop(paste(archivefile, "is missing"))}
  
  # First backup the existing data
  records <- readRDS(archivefile)
  filename <- paste0("records-", Sys.time(), ".rds")
  saveRDS(records, paste0(rootfolder, "/data/tidy/", filename))
  message(paste(filename, "backup created"))
  
}


update_archive <- function(rootfolder = here::here()) {
  # from https://taraskaduk.com/posts/2019-03-23-apple-health/
  require(XML)
  require(lubridate)
  require(dplyr)
  require(stringr)
  
  rawfile <- paste0(rootfolder, "/data/raw/export.zip")  
  if(!file.exists(rawfile)) {stop(paste(rawfile, "is missing"))}
  
  unzip(rawfile, exdir = paste0(rootfolder, "/data/raw"))
  
  # export files have some dodgy records which are corrupt. The following
  # command gets rid of the dodgy records and keeps the good ones.
  
  system("xmllint --recover ./data/raw/apple_health_export/export.xml > ./data/raw/apple_health_export/exporttest.xml")
  
  # Parse records and convert to data frame
  xml <- xmlParse(paste0(rootfolder, '/data/raw/apple_health_export/exporttest.xml'))
  df_record <-   XML:::xmlAttrsToDataFrame(xml["//Record"])  
  
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
  saveRDS(df, paste0(rootfolder, "/data/tidy/records.rds"))
  message("Data updated!")
  
}

check_archive <- function(rootfolder = here::here()){
  if(update_available(rootfolder)){
    backup_archive(rootfolder)
    update_archive(rootfolder)
    stop()
  }
  
  message("No update available")
  
}