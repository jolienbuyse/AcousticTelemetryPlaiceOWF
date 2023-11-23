##########################################
# Development of synchronisation model 
##########################################

# Institute: Institute for Agriculture, Fisheries and Food Research (ILVO)
# Author: Jolien Buyse - jolien.buyse@ilvo.vlaanderen.be

# Load packages

library(FedData)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(yaps)
library(data.table)

### Developing synchronisation model

## Load and prepare data for synchronization model

hydros <- read_csv("./hydros.csv")
hydros <- as.data.table(hydros)

sync_det_time <- read_csv("./sync_det_time.csv")

# Create list with sync tag detections and hydrophone metadata

bel <- list(detections=sync_det_time, hydros=hydros)

# Step 1: Model with 4 fixed hydrophones, let YAPS estimate the positions of the other receivers

max_epo_diff <- 280 # max is 600 (so a bit less than half of the RBI); random interval, median 10 min
min_hydros <- 2
time_keeper_idx <- 4
fixed_hydros_idx <- c(1:4) # Receivers put on tripod frames at 20 m depth: no exact coordinates 
n_offset_day <- 1
n_ss_day <- 1
keep_rate <- 50 # increase keep rate to 50 for final model

inp_sync <- getInpSync(sync_dat = bel, max_epo_diff, min_hydros, time_keeper_idx,
                       fixed_hydros_idx, n_offset_day, ss_data_what = "data", ss_data = ss_data, n_ss_day = n_ss_day, keep_rate = keep_rate)

df <- as.data.frame(getSyncCoverage(inp_sync, plot=TRUE))
getSyncCoverage(inp_sync, plot=TRUE)

sync_model <- getSyncModel(inp_sync, silent=F, max_iter = 300)
sync_model_KR_25_entireperiod <- sync_model
sync_model_KR_30_entireperiod <- sync_model
sync_model_KR_40_entireperiod <- sync_model
sync_model_KR_50_entireperiod <- sync_model

save(sync_model_KR_25_entireperiod, file="Sync_model_KR_25_entireperiod.Rdata")
save(sync_model_KR_30_entireperiod, file="Sync_model_KR_30_entireperiod.Rdata")
save(sync_model_KR_40_entireperiod, file="Sync_model_KR_40_entireperiod.Rdata")
save(sync_model_KR_50_entireperiod, file="Sync_model_KR_50_entireperiod.Rdata")

plotSyncModelResids(sync_model, by='overall')
plotSyncModelResids(sync_model, by = "quantiles")
plotSyncModelResids(sync_model, by = "sync_tag")
plotSyncModelResids(sync_model, by = "hydro")
plotSyncModelResids(sync_model, by = "temporal_hydro")
plotSyncModelResids(sync_model, by = "temporal_sync_tag")

model_finetuned <- fineTuneSyncModel(sync_model, eps_threshold = 1E4)
model_finetuned2 <- fineTuneSyncModel(model_finetuned, eps_threshold = 1E3)
model_finetuned3 <- fineTuneSyncModel(model_finetuned2, eps_threshold = 1E2)
model_finetuned4 <- fineTuneSyncModel(model_finetuned3, eps_threshold = 1E1)

plotSyncModelHydros(model_finetuned4)

FT_sync_model_KR_25_entireperiod <- model_finetuned3
save(FT_sync_model_KR_25_entireperiod, file="FT_sync_model_KR_25_entireperiod.Rdata")

plotSyncModelResids(model_finetuned, by='overall')
plotSyncModelResids(model_finetuned4, by='overall')

FT_sync_model_KR_50_entireperiod <- model_finetuned4
save(FT_sync_model_KR_50_entireperiod, file="FT_sync_model_KR_50_entireperiod.Rdata")

# Get estimated positions of receivers 5:28 and replace in hydros 

df_estim_5_28 <- as.data.frame(model_finetuned4$pl$TRUE_H)
colnames(df_estim_5_28) <- c("x","y","z")

# Run model again and estimate position of first 4 hydrophones

hydros2 <- data.frame(serial = hydros$serial, x = df_estim_5_28$x, y = df_estim_5_28$y, z = df_estim_5_28$z, sync_tag = hydros$sync_tag, idx = hydros$idx)
hydros2 <- as.data.table(hydros2)

bel2 <- list(detections=sync_det_time, hydros=hydros2)

max_epo_diff <- 280 # max is 600 (so a bit less than half of the RBI); random interval, median 10 min
min_hydros <- 2
time_keeper_idx <- 5
fixed_hydros_idx <- c(5:28) # Receivers put on tripod frames at 20 m depth: no exact coordinates 
n_offset_day <- 1
n_ss_day <- 1
keep_rate <- 50 # increase keep rate to 50 for final model

inp_sync <- getInpSync(sync_dat = bel2, max_epo_diff, min_hydros, time_keeper_idx,
                       fixed_hydros_idx, n_offset_day, ss_data_what = "data", ss_data = ss_data, n_ss_day = n_ss_day, keep_rate = keep_rate)

getSyncCoverage(inp_sync, plot=TRUE)

sync_model_B <- getSyncModel(inp_sync, silent=F, max_iter = 300)
Sync_model_KR_50_entireperiod_B <- sync_model_B
save(Sync_model_KR_50_entireperiod_B, file="Sync_model_KR_50_entireperiod_B.Rdata")

model_finetuned_B <- fineTuneSyncModel(sync_model_B, eps_threshold = 1E4)
plotSyncModelResids(model_finetuned_B, by='overall')
model_finetund_B2 <- fineTuneSyncModel(model_finetuned_B, eps_threshold = 1E3)
plotSyncModelResids(model_finetund_B2, by='overall')
model_finetuned_B3 <- fineTuneSyncModel(model_finetund_B2, eps_threshold = 1E2)
plotSyncModelResids(model_finetuned_B3, by='overall')
model_finetuned_B4 <- fineTuneSyncModel(model_finetuned_B3, eps_threshold = 1E1)
plotSyncModelResids(model_finetuned_B4, by='overall')

FT_sync_model_KR_50_entireperiod_withiteration <- model_finetuned_B4
save(FT_sync_model_KR_50_entireperiod_withiteration, file="FT_sync_model_KR_50_entireperiod_withiteration.Rdata")
load(file="FT_sync_model_KR_50_entireperiod_withiteration.Rdata")

# Check final model

plotSyncModelResids(model_finetuned_B4, by='overall')
plotSyncModelResids(model_finetuned_B4, by = "quantiles")
plotSyncModelResids(model_finetuned_B4, by = "sync_tag")
plotSyncModelResids(model_finetuned_B4, by = "hydro")
plotSyncModelResids(model_finetuned_B4, by = "temporal_hydro")
plotSyncModelResids(model_finetuned_B4, by = "temporal_sync_tag")

plotSyncModelCheck(model_finetuned_B4, by = "hydro")
plotSyncModelCheck(model_finetuned_B4, by = "sync_tag")
plotSyncModelCheck(model_finetuned_B4, by = "sync_bin_sync")
plotSyncModelCheck(model_finetuned_B4, by = "sync_bin_hydro")
plotSyncModelCheck(model_finetuned_B4, by = "sync_bin_sync_smooth")
plotSyncModelCheck(model_finetuned_B4, by = "sync_bin_hydro_smooth")

## Apply sync model to all detections (sync + animal detections)

model_finetuned_B4 <- FT_sync_model_KR_50_entireperiod_withiteration
all_detections <- read_csv("./data/detections.csv")

detectanimals <- yaps::prepDetections(all_detections, type="vemco_vue")
detectanimals$tz <- as.POSIXct(detectanimals$ts, tz = "UTC")
detectanimals <- detectanimals[ts %between% c("2020-06-17", "2020-10-12")]

detectanimals[, epofrac:= as.numeric(ts)]
detectanimals[, epo:=floor(epofrac)]
detectanimals[, frac:=epofrac - epo]
detectanimals[,epo-floor(epo)]

df_estim_1_4 <- as.data.frame(model_finetuned_B4$pl$TRUE_H)
colnames(df_estim_1_4) <- c("x","y","z")

hydros3 <- data.frame(serial = hydros$serial, x = df_estim_1_4$x, y = df_estim_1_4$y, z = df_estim_1_4$z, 
                      sync_tag = hydros$sync_tag, idx = hydros$idx)
hydros3 <- as.data.table(hydros3)

detections_synced <- applySync(toa=detectanimals, hydros=hydros3, model_finetuned_B4)

# Some detections are not synced: remove

detections_synced <- detections_synced[complete.cases(detections_synced),]
sum(is.na(detections_synced))

## Save CSV file with synced detections

write.csv(detections_synced, "detections_synced.csv", row.names = FALSE)