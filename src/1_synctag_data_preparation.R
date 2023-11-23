# Preparation of synctag metadata

# Institute: Institute for Agriculture, Fisheries and Food Research (ILVO)
# Author: Jolien Buyse - jolien.buyse@ilvo.vlaanderen.be
# Source: Adapted from https://github.com/baktoft/yaps

## Load in necessary packages

library(leaflet)
library(data.table)
library(sp)
library(ggplot2)
library(FedData)
library(dplyr)
library(lubridate)
library(readr)

## Prepare built-in sync tag metadata for synchronization

synctags <- read_csv("./data/deployments.csv")
rec <- read_csv("./data/receivers.csv")

# Merge receiver metadata with built-in-sync-tag ID

synctags <- synctags %>%
  filter(deploy_date_time < "2020-07-01 00:00:00" & 
           !(receiver_id == "TBR700-000900"))%>% # Remove Thelma Biotel receiver (test)
  right_join(rec, by = "receiver_id")%>%
  rename(Receiver = receiver_id)%>%
  select(Receiver, deploy_date_time, deploy_latitude, deploy_longitude, station_name, built_in_tag_id)

# Visualize receiver setup

synctags$idx <- c(1:nrow(synctags))
  
coordinates(synctags) <- ~deploy_longitude+deploy_latitude
m <- leaflet(data=synctags, options = leafletOptions(minZoom = 0, maxZoom = 18), width="100%", height=700)
m <- addTiles(m, group="OSM")
m <- addCircles(m, radius=5, label=as.character(synctags$idx), labelOptions = labelOptions(noHide = T, textOnly = TRUE))
m <- addMeasure(m, primaryLengthUnit="meters")
m <- addProviderTiles(m, providers$Esri.WorldImagery, group="Esri.WorldImagery")
m <- addLayersControl(m, baseGroups = c("OSM (default)", "Esri.WorldImagery"),    options = layersControlOptions(collapsed = FALSE)  )
m

# Change WGS84 to UTM coordinates for working with YAPS

proj4string(synctags) <- CRS("+init=epsg:4326")
synctags_utm <- as.data.table(spTransform(synctags, CRS("+init=epsg:32631")))

write.csv(synctags_utm, "./synctags_utm.csv", row.names = FALSE)

# Prepare metadata from synctags and adapt to the right format

synctags_utm$tag_id_sync <- substr_right(synctags_utm$built_in_tag_id,5)

# Get mean depth of receivers from logged environmental data

depth <- read_csv("./data/depth.csv")

synctags_utm <- synctags_utm %>%
 inner_join(depth, by = "Receiver")

hydros <- data.table(serial = as.integer(substr_right(synctags_utm$Receiver, 6)),
                     x = synctags_utm$deploy_longitude,
                     y = synctags_utm$deploy_latitude, 
                     z = synctags_utm$wdepth,
                     sync_tag = as.numeric(synctags_utm$tag_id_sync),
                     idx = c(1:nrow(synctags_utm)))

write.csv(hydros, "./hydros.csv", row.names = FALSE)

## Prepare speed of sound data

ss_data <- read_csv("./data/speedofsound.csv")
ss_data <- ss_data %>%
  select(ts, ss)%>%
  mutate(ts = as.POSIXct(ts, format = "%d/%m/%Y %H:%M"))
ss_data <- as.data.table(ss_data)

## Prepare synctag detections for yaps synchronization

all_detections <- read_csv("./data/detections.csv")

sync_data <- all_detections %>%
  filter(Transmitter %in% rec$built_in_tag_id) 

sync_det <- yaps::prepDetections(sync_data, type = "vemco_vue")

sync_det2$epo <- as.numeric(sync_det2$ts)
sync_det2$epofrac <- sync_det2$epo + sync_det2$frac

sync_det[,epo-floor(epo)]
sync_det[,epo:=floor(epo)]

sync_det_time <- sync_det[ts %between% c("2020-06-17", "2020-10-12")]

## Load in hydros and make list with synctag detections

hydros <- read_csv("./data/hydros.csv")
hydros <- as.data.table(hydros)

## Create list with sync tag detections and hydrophone metadata

bel <- list(detections=sync_det_time, hydros=hydros)

