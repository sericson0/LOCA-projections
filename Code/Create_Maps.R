rm(list = ls())
##
#installs package if not currently installed
loadPackage = function(package) {
  if(!require(package, character.only = TRUE)){
    install.packages(package)
    require(package, character.only =TRUE)
  }
}
loadPackage("rstudioapi") 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
loadPackage("tidyverse")
loadPackage("ncdf4")
loadPackage("raster")
loadPackage("rasterVis")
loadPackage("sf")
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
plot_values = function(dta, save_file, label, fill_low = 80, fill_high = 130) {
  S <- raster(t(dta), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) %>%
    flip(direction = "y") %>%
    as(., "SpatialPixelsDataFrame") %>%
    as.data.frame() %>%
    rename(val=1, "Long"=2, "Lat"=3) 
  
  p = ggplot(S, aes(x = Long, y = Lat, fill = val)) + 
    geom_tile() +
    geom_sf(data = States, inherit.aes = FALSE, fill = NA) + 
    scale_fill_distiller(type = "div", palette = "RdYlGn", limits = c(fill_low, fill_high), oob = scales::squish) +
    ylim(c(24,50)) + 
    xlim(min(long), max(long)) +
    theme_classic()  + 
    labs(fill = label)
  
  ggsave(save_file, p, height = 6, width = 8, units = "in", dpi = "retina")
}
#__________________________________________________________________________________________________________
#Get long lat and NA values
max_temp_nc = nc_open(file.path("../Downloaded Files", "Temperature", "ACCESS1-0", "rcp85", paste0("ACCESS1-0", "_", "rcp85", "_", 2010, "_", "tasmax", ".nc")))
lat =      ncvar_get(max_temp_nc, "lat_bnds")
long =     ncvar_get(max_temp_nc,  "lon_bnds") - 360
fill_val = ncatt_get(max_temp_nc, "tasmax", "_FillValue")
max_temp = ncvar_get(max_temp_nc, "tasmax") 
max_temp[max_temp == fill_val$value] = NA
max_temp = max_temp[, , 1]
nc_close(max_temp_nc)
#__________________________________________________________________________________________________________
Temp_1990 = readRDS("../Compiled Data/Max Temperature 1980 to 1990.rds")
Temp_2020 = readRDS("../Compiled Data/Max Temperature 2010 to 2020.rds")
Temp_2050 = readRDS("../Compiled Data/Max Temperature 2040 to 2050.rds")
# Temp_2080 = readRDS("../Compiled Data/Max Temperature 2070 to 2080.rds")
#
Num_Above_1990 = readRDS("../Compiled Data/Avg Num Above 95F 1980 to 1990.rds")
Num_Above_1990[which(is.na(max_temp))] = NA
Num_Above_2020 = readRDS("../Compiled Data/Avg Num Above 95F 2010 to 2020.rds")
Num_Above_2020[which(is.na(max_temp))] = NA
#
Num_Above_2050 = readRDS("../Compiled Data/Avg Num Above 95F 2040 to 2050.rds")
Num_Above_2050[which(is.na(max_temp))] = NA
# Num_Above_2080 = readRDS("../Compiled Data/Avg Num Above 95F 2070 to 2080.rds")
# Num_Above_2080[which(is.na(max_temp))] = NA
#
Temp_Change_2020 = Temp_2020 - Temp_1990
Temp_Change_2050 = Temp_2050 - Temp_1990
#
Num_Above_Change_2020 = Num_Above_2020 - Num_Above_1990 
Num_Above_Change_2050 = Num_Above_2050 - Num_Above_1990 
# Num_Above_Change_2080 = Num_Above_2080 - Num_Above_1990 
##_________________________________________________________________________________________________________________________________________

States = st_read("../cb_2018_us_state_500k", layer = "cb_2018_us_state_500k") %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "American Samoa", "Puerto Rico", "United States Virgin Islands"))) %>%
  st_geometry()


Cities = read_csv("../City-level_Data.csv") %>%
  mutate(long_num = sapply(Long, function(x) which(abs(long[1, ] - x) == min(abs(long[1,] - x))[1])),
         lat_num =  sapply(Lat, function(x) which(abs(lat[1, ] - x) == min(abs(lat[1,] - x))[1])),
         Temp1990 = sapply(1:nrow(.), function(i) Temp_1990[long_num[i], lat_num[i]]), 
         Temp2020 = sapply(1:nrow(.), function(i) Temp_2020[long_num[i], lat_num[i]]),
         Temp2050 = sapply(1:nrow(.), function(i) Temp_2050[long_num[i], lat_num[i]]),
         Temp_Change_2020 = sapply(1:nrow(.), function(i) Temp_Change_2020[long_num[i], lat_num[i]]),
         Temp_Change_2050 = sapply(1:nrow(.), function(i) Temp_Change_2050[long_num[i], lat_num[i]]),
         Num_Above_1990 = sapply(1:nrow(.), function(i) Num_Above_1990[long_num[i], lat_num[i]]),
         Num_Above_2020 = sapply(1:nrow(.), function(i) Num_Above_2020[long_num[i], lat_num[i]]),
         Num_Above_2050 = sapply(1:nrow(.), function(i) Num_Above_2050[long_num[i], lat_num[i]]),
         # Num_Above_2080 = sapply(1:nrow(.), function(i) Num_Above_2080[long_num[i], lat_num[i]],
         Num_Above_Change_2020 = sapply(1:nrow(.), function(i) Num_Above_Change_2020[long_num[i], lat_num[i]]),
         Num_Above_Change_2050 = sapply(1:nrow(.), function(i) Num_Above_Change_2050[long_num[i], lat_num[i]]) #,
         # Num_Above_Change_2080 = sapply(1:nrow(.), function(i) Num_Above_Change_2080[long_num[i], lat_num[i]]))
  )
##
write_csv(Cities, "../Compiled Data/City Level Data.csv")

#Subset of cities which show up well on map
Select_Cities = read_csv("../Select Cities.csv") %>%
  mutate(long_num = sapply(Long, function(x) which(abs(long[1, ] - x) == min(abs(long[1,] - x))[1])),
         lat_num =  sapply(Lat, function(x) which(abs(lat[1, ] - x) == min(abs(lat[1,] - x))[1])),
         Temp1990 = sapply(1:nrow(.), function(i) Temp_1990[long_num[i], lat_num[i]]), 
         Temp2020 = sapply(1:nrow(.), function(i) Temp_2020[long_num[i], lat_num[i]]),
         Temp2050 = sapply(1:nrow(.), function(i) Temp_2050[long_num[i], lat_num[i]]),
         Temp_Change_2020 = sapply(1:nrow(.), function(i) Temp_Change_2020[long_num[i], lat_num[i]]),
         Temp_Change_2050 = sapply(1:nrow(.), function(i) Temp_Change_2050[long_num[i], lat_num[i]]),
         Num_Above_1990 = sapply(1:nrow(.), function(i) Num_Above_1990[long_num[i], lat_num[i]]),
         Num_Above_2020 = sapply(1:nrow(.), function(i) Num_Above_2020[long_num[i], lat_num[i]]),
         Num_Above_2050 = sapply(1:nrow(.), function(i) Num_Above_2050[long_num[i], lat_num[i]]),
         # Num_Above_2080 = sapply(1:nrow(.), function(i) Num_Above_2080[long_num[i], lat_num[i]]),
         Num_Above_Change_2020 = sapply(1:nrow(.), function(i) Num_Above_Change_2020[long_num[i], lat_num[i]]),
         Num_Above_Change_2050 = sapply(1:nrow(.), function(i) Num_Above_Change_2050[long_num[i], lat_num[i]]) #,
         # Num_Above_Change_2080 = sapply(1:nrow(.), function(i) Num_Above_Change_2080[long_num[i], lat_num[i]])
         ) %>%
  st_as_sf(coords = c("Long", "Lat"), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"), remove = FALSE)

write_csv(Select_Cities, "../Compiled Data/Select Cities.csv")
##_________________________________________________________________________________________________________________________________________
##_________________________________________________________________________________________________________________________________________


plot_values(Temp_1990, "../Figures/One in 10 Year Max 1980 to 1990.png", "Hottest Temp (F)", fill_low = 80, fill_high = 130)
plot_values(Temp_2020, "../Figures/One in 10 Year Max 2010 to 2020.png", "Hottest Temp (F)", fill_low = 80, fill_high = 130)
plot_values(Temp_2050, "../Figures/One in 10 Year Max 2040 to 2050.png", "Hottest Temp (F)", fill_low = 80, fill_high = 130)

plot_values(Temp_Change_2020, "../Figures/Temperature Change 1990 to 2020.png", "Hottest Temp (F)", fill_low = 1, fill_high = 10)
plot_values(Temp_Change_2050, "../Figures/Temperature Change 1990 to 2050.png", "Hottest Temp (F)", fill_low = 1, fill_high = 10)

plot_values(Num_Above_1990, "../Figures/Number Above 95F 1980 to 1990.png", "Number Above 95F", fill_low = 0, fill_high = 150)
plot_values(Num_Above_2020, "../Figures/Number Above 95F 2010 to 2020.png", "Number Above 95F", fill_low = 0, fill_high = 150)
plot_values(Num_Above_2050, "../Figures/Number Above 95F 2040 to 2050.png", "Number Above 95F", fill_low = 0, fill_high = 150)
# plot_values(Num_Above_2080, "../Figures/Number Above 95F 2070 to 2080.png", "Number Above 95F", fill_low = 0, fill_high = 150)

plot_values(Num_Above_Change_2020, "../Figures/Num Above Change 1990 to 2020.png", "Increase in days\nAbove 95F", fill_low = 0, fill_high = 50)
plot_values(Num_Above_Change_2050, "../Figures/Num Above Change 1990 to 2050.png", "Increase in days\nAbove 95F", fill_low = 0, fill_high = 50)
# plot_values(Num_Above_Change_2080, "../Figures/Num Above Change 1990 to 2080.png", "Increase in\ndays Above 95F", fill_low = 0, fill_high = 50)


##_________________________________________________________________________________________________________________________________________
##_________________________________________________________________________________________________________________________________________
##_________________________________________________________________________________________________________________________________________
#Plot for selected cities

ggplot(Select_Cities, aes(color = Temp_Change_2050, label = PlaceName)) +
  geom_sf(data = States, inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_sf(size = 4.25) +
  geom_text(aes(Long, Lat, label = PlaceName), inherit.aes = FALSE, nudge_y = 0.9, size = 3) + 
  scale_color_distiller(type = "seq", palette = "YlOrRd", direction = 2) +
  ylim(c(24,50)) + 
  xlim(min(long), max(long)) +
  xlab(element_blank()) + ylab(element_blank()) + 
  labs(color = "Max Temp \nIncrease (F)") +
  theme_classic()
ggsave("../Figures/Select Cities Increase in temperature 1990 to 2050.png", height = 6, width = 8, units = "in", dpi = "retina")

ggplot(Select_Cities, aes(color = Temp2050, label = PlaceName)) +
  geom_sf(data = States, inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_sf(size = 4.25) +
  geom_text(aes(Long, Lat, label = PlaceName), inherit.aes = FALSE, nudge_y = 0.9, size = 3) + 
  scale_color_distiller(type = "seq", palette = "YlOrRd", direction = 2) +
  ylim(c(24,50)) + 
  xlim(min(long), max(long)) +
  labs(color = "Max Temp (F)") +
  theme_classic()
ggsave("../Figures/Maximum Tempearture Select Cities 2040 to 2050.png", height = 6, width = 8, units = "in", dpi = "retina")


ggplot(Select_Cities, aes(color = Num_Above_2050, label = PlaceName)) +
  geom_sf(data = States, inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_sf(size = 4.25) +
  geom_text(aes(Long, Lat, label = PlaceName), inherit.aes = FALSE, nudge_y = 0.9, size = 3) + 
  scale_color_distiller(type = "seq", palette = "YlOrRd", direction = 2) +
  ylim(c(24,50)) + 
  xlim(min(long), max(long)) +
  labs(color = "Num Above 95F") +
  theme_classic()
ggsave("../Figures/Num Above 95F Select Cities 2040 to 2050.png", height = 6, width = 8, units = "in", dpi = "retina")


ggplot(Select_Cities, aes(color = Num_Above_Change_2050, label = PlaceName)) +
  geom_sf(data = States, inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_sf(size = 4.25) +
  geom_text(aes(Long, Lat, label = PlaceName), inherit.aes = FALSE, nudge_y = 0.9, size = 3) + 
  scale_color_distiller(type = "seq", palette = "YlOrRd", direction = 2) +
  ylim(c(24,50)) + 
  xlim(min(long), max(long)) +
  labs(color = "Increase in \nNum Above 95F") +
  theme_classic()
ggsave("../Figures/Increase in Num Above 95F Select Cities 2040 to 2050.png", height = 6, width = 8, units = "in", dpi = "retina")
