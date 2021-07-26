# rm(list = ls())
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
loadPackage("sf")
#__________________________________________________________________________________________________________
####Not Currently used####
# get_heat_index = function(tmp, hmd) {
#   #Valid only for temperatures above 80 and humidity above 40
#   hi_vals = -42.379 + 2.04901523*tmp + 10.14333127*hmd - 0.22475541*tmp*hmd - 6.83783e-3 * tmp^2 - 5.481717e-2 * hmd^2 + 
#     1.22874e-3 * tmp^2 * hmd + 8.5282e-4 * tmp * hmd^2 - 1.99e-6 * tmp^2 * hmd^2
#   hi_vals[which(tmp < 80)] = tmp[which(tmp < 80)]
#   hi_vals[which(hmd < 40)] = tmp[which(hmd < 40)]
#   return(hi_vals)
# }
##

memory.limit(size=99999)

#Loads downloaded LOCA data into memory
load_data = function(model, scenario, year) {
  max_temp_nc = nc_open(file.path("../Downloaded Files", "Temperature", model, scenario, paste0(model, "_", scenario, "_", year, "_", "tasmax", ".nc")))
  
  lat =      ncvar_get(max_temp_nc, "lat_bnds")
  long =     ncvar_get(max_temp_nc,  "lon_bnds") - 360
  time =     ncvar_get(max_temp_nc, "time")
  fill_val = ncatt_get(max_temp_nc, "tasmax", "_FillValue")
  
  Max_Temp = ncvar_get(max_temp_nc, "tasmax") 
  Max_Temp[Max_Temp == fill_val$value] = NA
  Max_Temp = (Max_Temp - 273.15) * (9/5) + 32 #Converts from Kelvin to Ferenheight
  nc_close(max_temp_nc)
  
  min_temp_nc = nc_open(file.path("../Downloaded Files", "Temperature", model, scenario, paste0(model, "_", scenario, "_", year, "_", "tasmin", ".nc"))) 
  fill_val = ncatt_get(min_temp_nc, "tasmin", "_FillValue")
  Min_Temp = ncvar_get(min_temp_nc, "tasmin") 
  Min_Temp[Min_Temp == fill_val$value] = NA
  Min_Temp = (Min_Temp - 273.15) * (9/5) + 32 
  nc_close(min_temp_nc)
  
  humid_nc = nc_open(file.path("../Downloaded Files", "Relative Humidity", model, scenario, paste0(model, "_", scenario, "_", year, "_", "relHumid", ".nc")))
  fill_val = ncatt_get(humid_nc, "relHumid", "_FillValue")
  Relative_Humidity = ncvar_get(humid_nc, "relHumid")
  Relative_Humidity[Relative_Humidity == fill_val$value] = NA
  
  # Heat_Index = get_heat_index(Max_Temp, Relative_Humidity) 
  return(list(lat = lat, long = long, time = time, max_temp = Max_Temp, min_temp = Min_Temp, relative_humidity = Relative_Humidity))
  
}


subset_temperature_data = function(dta) {
  #Return maximum and minimum temperatures, days they occur, and relative humidity during those times. 
  max_temp = apply(dta$max_temp, c(1,2), max)
  min_temp = apply(dta$min_temp, c(1,2), min)
  max_temp_day = apply(dta$max_temp, c(1,2), function(x) which(x == max(x))[1])
  min_temp_day = apply(dta$min_temp, c(1,2), function(x) which(x == min(x))[1])
  humidity = matrix(nrow = nrow(max_temp_day), ncol = ncol(max_temp_day))
    for(row in 1:nrow(humidity)) {
    for(col in 1:ncol(humidity)) {
      humidity[row, col] = dta$relative_humidity[row, col, max_temp_day[row, col]]
    }
  }
  return(list(max_temp = max_temp, max_temp_day = max_temp_day, min_temp = min_temp, min_temp_day = min_temp_day, humidity = humidity))
}

get_multiyear_max = function(years, model, scenario) {
  #Calculate the maximum across entire period
  all_years = sapply(years, function(y) subset_temperature_data(load_data(model, scenario, y))$max_temp, simplify = "array")
  return(apply(all_years, c(1,2), max))
}

get_number_above_cutoff = function(dta, cutoff = 95) {
  #Helper function for number of days above cutoff
  X = apply(dta$max_temp, c(1,2), function(x) length(which(x > cutoff)))
  # X[which(is.na(dta$max_temp))] = NA
}


get_multiyear_above = function(years, model, scenario, cutoff = 95) {
  #Get average above cutoff for multiple years
  num_above = get_number_above_cutoff(load_data(model, scenario, years[1]))
  for(y in years[-1]) {
    num_above = num_above + get_number_above_cutoff(load_data(model, scenario, y))
  }
  return(num_above / length(years))
}

get_average_across_models = function(fn, years, models, scenario) {
  #Calculate average across multiple models
  print(models[1])
  df = fn(years, models[1], scenario) 
  for(model in models[-1]) {
    print(model)
    df = df + fn(years, model, scenario) 
  }
  return(df / length(models))
}
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

models = c("ACCESS1-0", "CCSM4", "CESM1-BGC",	"CMCC-CMS",	"CNRM-CM5",	"CanESM2", "GFDL-CM3",	"HadGEM2-CC",	"HadGEM2-ES", "MIROC5")
scenario = "rcp85"
years_0 = 1980:1990 
years_1 = 2010:2020 
years_2 = 2040:2050
# years_3 = 2070:2080

#Runs for each period
##
Max_Temp_1990 = get_average_across_models(get_multiyear_max, years_0, models, "historical")
saveRDS(Max_Temp_1990, "../Compiled Data/Max Temperature 1980 to 1990.rds")
#
Max_Temp_2020 = get_average_across_models(get_multiyear_max, years_1, models, scenario)
saveRDS(Max_Temp_2020, "../Compiled Data/Max Temperature 2010 to 2020.rds")
#
Max_Temp_2050 = get_average_across_models(get_multiyear_max, years_2, models, scenario)
saveRDS(Max_Temp_2050, "../Compiled Data/Max Temperature 2040 to 2050.rds")
##
# Max_Temp_2080 = get_average_across_models(get_multiyear_max, years_3, models, scenario)
# saveRDS(Max_Temp_2080, "../Compiled Data/Max Temperature 2070 to 2080.rds")
##
##
Num_Above_1990 = get_average_across_models(get_multiyear_above, years_0, models, "historical")
saveRDS(Num_Above_1990, "../Compiled Data/Avg Num Above 95F 1980 to 1990.rds")
#
Num_Above_2020 = get_average_across_models(get_multiyear_above, years_1, models, scenario)
saveRDS(Num_Above_2020, "../Compiled Data/Avg Num Above 95F 2010 to 2020.rds")
#
Num_Above_2050 = get_average_across_models(get_multiyear_above, years_2, models, scenario)
saveRDS(Num_Above_2050, "../Compiled Data/Avg Num Above 95F 2040 to 2050.rds")
#
# Num_Above_2080 = get_average_across_models(get_multiyear_above, years_3, models, scenario)
# saveRDS(Num_Above_2080, "../Compiled Data/Avg Num Above 95F 2070 to 2080.rds")
# 
