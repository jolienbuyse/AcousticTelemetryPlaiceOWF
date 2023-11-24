##############################################
# Estimating positions of a fish using yaps 
##############################################

# Project: Acoustic telemetry analysis of plaice within an offshore wind farm (Belwind, BE)
# Institute: Institute for Agriculture, Fisheries and Food Research (ILVO)
# Author: Jolien Buyse - jolien.buyse@ilvo.vlaanderen.be

# Load packages

library(lubridate)
library(yaps)
library(stringr)

# Load in necessary data

detections_synced <- read_csv("./detections_synced.csv")

# Specify focal tag and tag specific min and max burst intervals (example with tag 9281 is given)

focal_tag <- 9281

rbi_min <- 60
rbi_max <- 130

# Extract relevant data from the synced data

synced_dat_JJBelwind <- detections_synced[tag == focal_tag]

# Create overlapping time bins

vector <- c(seq(min(synced_dat_JJBelwind$ts), 
                max(synced_dat_JJBelwind$ts), by = "hour"))

hrs <- function(u) {
  x <- u * 3600
  return(x)
}

T9281 <- list()

for(i in vector){
  
  e <- i + hrs(4)
  m <- synced_dat_JJBelwind[ts > i & ts < e]
  name <- as.character(paste0("df_", i))
  T9281[[name]] <- m
  
}

names(T9281) <- vector

# Apply yaps 5 times to each time bin separately 

yaps <- function(x){
  toa_JJBelwind <- getToaYaps(x, hydros3, rbi_min, rbi_max, pingType = "rbi")
  bbox <- getBbox(hydros_yaps, buffer=, pen=1e6)
  inp_JJBelwind <- getInp(hydros_yaps, toa_JJBelwind, E_dist="t", n_ss=3, pingType="rbi", 
                          sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what="est",ss_data=0, bbox = bbox)
  outputyaps <- runYaps(inp_JJBelwind, getPlsd = TRUE, getRep = TRUE, silent=FALSE,
                        tmb_smartsearch = TRUE, maxIter=10000)
}


lapply_with_error <- function(X,FUN,...){    
  lapply(X, function(x, ...) tryCatch(FUN(x, ...), error=function(e) NULL))
}

T9281_outputyaps <- lapply_with_error(T9281, yaps)
T9281_outputyaps_2 <- lapply_with_error(T9281, yaps)
T9281_outputyaps_3 <- lapply_with_error(T9281, yaps)
T9281_outputyaps_4 <- lapply_with_error(T9281, yaps)
T9281_outputyaps_5 <- lapply_with_error(T9281, yaps)

# Extract the relevant positioning information and paste together all bins

T9281_alldetections <- data.table::rbindlist(lapply(T9281_outputyaps, "[[", "track"))
T9281_obj <- data.table::rbindlist(lapply(T9281_outputyaps, "[", "obj"), idcol = "data")
T9281_alldetections <- data.table::rbindlist(lapply(T9281_outputyaps, "[[", "track"), idcol = "data")
T9281_alldetections <-  merge(T9281_alldetections, T9281_obj, by = "data")

T9281_alldetections_2 <- data.table::rbindlist(lapply(T9281_outputyaps_2, "[[", "track"))
T9281_obj_2 <- data.table::rbindlist(lapply(T9281_outputyaps_2, "[", "obj"), idcol = "data")
T9281_alldetections_2 <- data.table::rbindlist(lapply(T9281_outputyaps_2, "[[", "track"), idcol = "data")
T9281_alldetections_2 <-  merge(T9281_alldetections_2, T9281_obj_2, by = "data")

T9281_alldetections_3 <- data.table::rbindlist(lapply(T9281_outputyaps_3, "[[", "track"))
T9281_obj_3 <- data.table::rbindlist(lapply(T9281_outputyaps_3, "[", "obj"), idcol = "data")
T9281_alldetections_3 <- data.table::rbindlist(lapply(T9281_outputyaps_3, "[[", "track"), idcol = "data")
T9281_alldetections_3 <-  merge(T9281_alldetections_3, T9281_obj_3, by = "data")

T9281_alldetections_4 <- data.table::rbindlist(lapply(T9281_outputyaps_4, "[[", "track"))
T9281_obj_4 <- data.table::rbindlist(lapply(T9281_outputyaps_4, "[", "obj"), idcol = "data")
T9281_alldetections_4 <- data.table::rbindlist(lapply(T9281_outputyaps_4, "[[", "track"), idcol = "data")
T9281_alldetections_4 <-  merge(T9281_alldetections_4, T9281_obj_4, by = "data")

T9281_alldetections_5 <- data.table::rbindlist(lapply(T9281_outputyaps_5, "[[", "track"))
T9281_obj_5 <- data.table::rbindlist(lapply(T9281_outputyaps_5, "[", "obj"), idcol = "data")
T9281_alldetections_5 <- data.table::rbindlist(lapply(T9281_outputyaps_5, "[[", "track"), idcol = "data")
T9281_alldetections_5 <-  merge(T9281_alldetections_5, T9281_obj_5, by = "data")

# Save all position estimates (5 times)

write.csv(T9281_alldetections, "T9281_alldetections.csv", row.names = FALSE)
write.csv(T9281_alldetections_2, "T9281_alldetections_2.csv", row.names = FALSE)
write.csv(T9281_alldetections_3, "T9281_alldetections_3.csv", row.names = FALSE)
write.csv(T9281_alldetections_4, "T9281_alldetections_4.csv", row.names = FALSE)
write.csv(T9281_alldetections_5, "T9281_alldetections_5.csv", row.names = FALSE)