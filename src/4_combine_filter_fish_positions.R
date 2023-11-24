##############################################
# Combining and filtering of fish positions
##############################################

# Project: Acoustic telemetry analysis of plaice within an offshore wind farm (Belwind, BE)
# Institute: Institute for Agriculture, Fisheries and Food Research (ILVO)
# Author: Jolien Buyse - jolien.buyse@ilvo.vlaanderen.be

# Load necessary packages

library(readr)
library(dplyr)
library(lubridate)

# Read necessary data

animals <- read_csv("./data/animals.csv")

# Filter fish that were at least 20 days present during first monitoring period (n = 21)

transmitters <- animals %>%
  mutate(tag = substr(tag_id, 6,9))%>%
  filter(!(tag %in% c('9276', '9285','9283','9279','9268','9258', '9275', '9249', '9257', '9262')))%>%
  pull()

# Make vector with the tagging dates of each fish

tagdates <- animals %>%
  mutate(start_day_det = floor_date(as.POSIXct(release_date_time, tz = "UTC") + days(2), "day"),
         tag = substr(tag_id, 6,9))%>%
  select(start_day_det, tag)

# Combine the yaps output (5x) for each fish by selecting the time bin with the lowest object score and combine all positions in one data frame

all_positions <- lapply(transmitters, function(transmitter){
  
  print(paste0('Start: ', transmitter))
  
  Det1 <- read.csv(paste0("./T",transmitter,"/T",transmitter,"_alldetections.csv"))
  Det2 <- read.csv(paste0("./T",transmitter,"/T",transmitter,"_alldetections_2.csv"))
  Det3 <- read.csv(paste0("./T",transmitter,"/T",transmitter,"_alldetections_3.csv"))
  Det4 <- read.csv(paste0("./T",transmitter,"/T",transmitter,"_alldetections_4.csv"))
  Det5 <- read.csv(paste0("./T",transmitter,"/T",transmitter,"_alldetections_5.csv"))
  
  Detlist <- list(Det1,Det2,Det3,Det4,Det5)
  names(Detlist) <- c('Det1','Det2','Det3','Det4','Det5')
  
  combined_det <- lapply(Detlist, function(det){
    
    det <- det %>% 
      filter(!(is.na(x_sd) | is.na(y_sd) | nobs<3 | x_sd > 10 | y_sd > 10))%>%
      group_by(top)%>%
      slice_min(order_by = obj)%>%
      ungroup()%>%
      mutate(tag = transmitter,
             datetime = parse_date_time(top, orders = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 
             roundmin =  round(datetime, units = "mins"))
    
  })
  
  df_temp <- plyr::ldply(combined_det, .id = NA)
  
  df_temp_2 <- df_temp %>%
    group_by(roundmin)%>%
    slice_min(order_by = obj)%>%
    ungroup()%>%
    left_join(tagdates, by = 'tag')%>%
    filter(start_day_det < datetime)
  
  df_temp_3 <- df_temp_2%>%
    mutate(diff = difftime(datetime,lag(datetime)),
           cut = if_else(diff > 150, "CUT", "-"),
           row.id = c(1:nrow(df_temp_2)))%>%
    select(-c(z,z_sd,.id))
  
  rows <- ifelse(df_temp_3$cut == "CUT", df_temp_3$row.id, NA)
  rows <- as.numeric(na.omit(rows))  
  
  df_temp_4 <- split(df_temp_3, cumsum(1:nrow(df_temp_3) %in% rows))
  df_temp_5 <- plyr::ldply(df_temp_4, .id = NA)
  
  df_temp_5 <- df_temp_5 %>%
    rename(path_id = .id)%>%
    select(-c(cut, row.id))
  
  print(paste0('Stop: ', transmitter))
  
  return(df_temp_5)
  
})

allpositions <- bind_rows(all_positions)

write.csv(allpositions, "allpositions.csv", row.names = FALSE)

# Import in QGIS: Join to nearest feature => distance to nearest turbine for each estimated position

