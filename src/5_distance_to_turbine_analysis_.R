#####################################
# Analysis of distance to turbines 
#####################################

# Project: Acoustic telemetry analysis of plaice within an offshore wind farm (Belwind, BE)
# Institute: Institute for Agriculture, Fisheries and Food Research (ILVO)
# Author: Jolien Buyse - jolien.buyse@ilvo.vlaanderen.be

# Load necessary packages

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(mgcv)
library(suncalc)
library(GGally)
library(gamm4)
library(tidyr)

# Load data from QGIS where the distance of each fish position to the nearest turbine was calculated

detdist <- read_csv("./data/alldetections_distancetonearestturbine.csv")

detdist <- detdist %>%
  mutate(datetime = parse_date_time(datetime, orders = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         hour = hour(datetime),
         tag = factor(tag),
         day = date(datetime),
         dist_turbine = distance - 2.5)%>%  # turbine foundations are 5m in diameter
  rename(turbine = Habitat)%>%
  filter(dist_turbine < 150,
         turbine %in% c('B09', 'C08', 'D09'))%>%
  select(datetime, hour, day, tag, x, y, x_sd, y_sd, dist_turbine, turbine)

detdist$dist_turbine <- ifelse(detdist$dist_turbine < 0, 0, detdist$dist_turbine)

ggplot() + geom_boxplot(data = detdist, aes(x = hour, y = dist_turbine, group = hour)) + facet_wrap(~turbine)

# Get all explanatory variables

light <- getSunlightTimes(
  date = c(seq(as.Date("2020-06-17"), as.Date("2020-10-12"), by="days")),
  lat = 51.653074,
  lon = 2.79089,
  keep = c("sunset", "nauticalDusk", "nauticalDawn", "sunrise"),
  tz = "UTC"
)

# current speed

current <- read_csv("./data/currentspeed.csv")

current <- current %>%
  slice(-1)%>%
  mutate(velo_bot_east = as.numeric(bottom_baroclinic_eastward_sea_water_velocity),
         velo_bot_north = as.numeric(bottom_baroclinic_northward_sea_water_velocity),
         velo_surf_east = as.numeric(surface_baroclinic_eastward_sea_water_velocity),
         velo_surf_north = as.numeric(surface_baroclinic_northward_sea_water_velocity),
         surf_height = as.numeric(sea_surface_height_above_sea_level),
         datetime = parse_date_time(time, orders = 'ymd HMS', tz = 'UTC'),
         hour = hour(datetime),
         date = date(datetime),
         cur_speed_bot = sqrt((velo_bot_east^2)+(velo_bot_north^2)),
         cur_speed_surf = sqrt((velo_surf_east^2) + (velo_surf_north^2)),
         cur_dir_bot = (180/pi) * atan2(velo_bot_east, velo_bot_north),
         cur_dir_surf = (180/pi) * atan2(velo_surf_east, velo_surf_north)) %>%
  select(c(hour, date, cur_speed_bot, cur_speed_surf, cur_dir_bot, cur_dir_surf, surf_height))

# noise

noise <- read_csv("./data/noise.csv")

# Format detection data with distances and join with explanatory variables

detdist2 <- detdist %>%
  rename(date = day) %>%
  select(-c(x,y)) %>%
  left_join(light, by = 'date')%>%
  select(-c(lon,lat)) %>%
  left_join(noise, by = c('date', 'hour'))%>%
  left_join(current, by = c('date', 'hour'))%>%
  mutate(timesinceSR = as.numeric(difftime(datetime, sunrise, units = "mins")),
         timetoSS = as.numeric(difftime(datetime, sunset, units = "mins")))

# Test relationships between response variable and explanatory variables

detdist2 %>%
  ggplot(aes(x = timesinceSR, y = dist_turbine)) + geom_point() + geom_smooth()

detdist2 %>%
  ggplot(aes(x = noise_level, y = dist_turbine)) + geom_point() + geom_smooth()

detdist2 %>%
  ggplot(aes(x = cur_speed_bot, y = dist_turbine)) + geom_point() + geom_smooth()

detdist2 %>%
  ggplot(aes(x = timetoSS, y = dist_turbine)) + geom_point() + geom_smooth()


# Divide data into training and test data set 

detdist2$id <- c(1:nrow(detdist2))
set.seed(0)
train_detdist = sample_n(detdist2, 10000)
test_detdist = anti_join(detdist2, train_detdist, by = "id")

ggpairs(train_detdist, columns = c('noise_level', 'timesinceSR', 'timetoSS','cur_speed_bot'))

# Noise_level correlates highly (0.84) with bottom current speed, so increased current speeds lead to increase in noise and lowers the detection efficiency
# Time to SS and time since SR correlate also highly, choose one => time since SR
# Never put them together in a single model! 

# dist_turbine ~ s(timesinceSR, bs = 'cs') + s(cur_speed_bot, bs = 'cs')
# dist_turbine ~ s(timesinceSR, bs = 'cs') 
# dist_turbine ~ s(cur_speed_bot)

# Apply different models

M1 <- gamm4(dist_turbine ~  s(timesinceSR, bs = 'cs') + s(cur_speed_bot, bs = 'cs'),
            random = ~ (1 | tag) + (1 | turbine),
            data = train_detdist)
M2 <- gamm4(dist_turbine ~  s(cur_speed_bot, bs = 'cs'),
            random = ~ (1 | tag) + (1 | turbine),
            data = train_detdist)
M3 <- gamm4(dist_turbine ~ s(timesinceSR, bs = 'cs'),
            random = ~ (1 | tag) + (1 | turbine),
            data = train_detdist)

# Choose best model with lowest RMSE and AIC

models <- c(M1,M2,M3)

calc_rmse <- function(model){
  
  p.model <- predict(model$gam,newdata=test_detdist)
  rmse.model <- sqrt(mean((test_detdist$dist_turbine - p.model)^2))

  return(rmse.model)
}

calc_rmse(M1)
calc_rmse(M2)
calc_rmse(M3)

c(AIC(M1$mer), AIC(M2$mer), AIC(M3$mer))

# Check model assumptions for M3 

summary(M3$gam)
plot(M3$gam, pages = 1)
gam.check(M3$gam)

# Predict values and plot

train_detdist <- train_detdist %>% 
mutate(timesinceSR_h = timesinceSR/60)%>%
  mutate(timesinceSR_h_rounded = round(as.numeric(timesinceSR_h), digits = 0))

new_data <- tidyr::expand(detdist2, timesinceSR = seq(-360,1200,by = 5), nesting(tag))

M3_pred <- predict.gam(M3$gam, newdata = new_data, se.fit = TRUE)
M3_df <-  cbind(fit = M3_pred$fit, se = M3_pred$se.fit, new_data)
M3_df <- M3_df %>%
  mutate(timesinceSR_h = timesinceSR/60,
         low = fit - 1.96 * se,
         high = fit + 1.96 * se)

ggplot() + 
  geom_jitter(data = train_detdist, aes(x = timesinceSR_h_rounded, y = dist_turbine), fill = "#7493a1", colour = "#7493a1", alpha = 0.05)+
  geom_line(data = M3_df, aes(x= timesinceSR_h, y = fit), size = 1.2, colour = "#ff7171")+
  geom_boxplot(data = train_detdist, aes(x = timesinceSR_h_rounded, y = dist_turbine, 
                                         group = as.factor(timesinceSR_h_rounded)), alpha = 0.1, size = 0.8, colour = "#7493a1", fill = "#7493a1") + 
  geom_ribbon(data = M3_df, aes(x= timesinceSR_h, ymin = low, ymax = high), colour = "#ff7171", fill = "#ff7171", alpha = 0.1, linetype = 2, size = 0.8)+
  theme_bw()+theme(axis.text = element_text(size = 12),
                   axis.title = element_text(size = 13),
                   panel.border = element_blank(),
                   axis.line = element_line(colour = "black"))+
  xlab("Time relative to sunrise (h)") + ylab("Distance to turbine (m)")