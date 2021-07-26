rm(list = ls())
loadPackage = function(package) {
  if(!require(package, character.only = TRUE)){
    install.packages(package)
    require(package, character.only =TRUE)
  }
}
loadPackage("rstudioapi") 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
loadPackage("tidyverse")
#_____________________________________________________________________________________________________________________________
#All models available
# models = c("ACCESS1-0",	"ACCESS1-3",	"CCSM4",	"CESM1-BGC",	"CESM1-CAM5",	"CMCC-CM",	"CMCC-CMS",	"CNRM-CM5",	"CSIRO-Mk3-6-0",	"CanESM2",	
#            "EC-EARTH",	"FGOALS-g2",	"GFDL-CM3",	"GFDL-ESM2G",	"GFDL-ESM2M",	"GISS-E2-H",	"GISS-E2-R",	"HadGEM2-AO",	"HadGEM2-CC",	"HadGEM2-ES",
#            "IPSL-CM5A-LR",	"IPSL-CM5A-MR",	"Livneh_L14",	"Livneh_L14_CONUS",	"MIROC-ESM",	"MIROC-ESM-CHEM",	"MIROC5",	"MPI-ESM-LR",	"MPI-ESM-MR", 
#            "MRI-CGCM3",	"NorESM1-M",	"bcc-csm1-1",	"bcc-csm1-1-m",	"inmcm4")
#_____________________________________________________________________________________________________________________________

check_download = function(model, scenario, variable, year, type = "Temperature") {
  #Looks if file has already been downloaded
  folder_path = folder_path = file.path("../Downloaded Files/", type, model, scenario)
  if(file.exists(paste0(folder_path, "/", model, "_", scenario, "_", year, "_", variable, ".nc"))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

download_temp = function(model, scenario, variable, year, quiet_download = TRUE) {
  #Downloads LOCA temperature data from ftp site
  #Note that historical data in the set go up to 2005 and future scenarios go up to 2100
  if(year > 2005 & scenario == "historical") {
    print("historical scenario data set only goes up to 2005. Set scenario to rcp45 or rcp85 for years after 2005")
  } 
  if(year < 2006 & scenario != "historical") {
    print("historical scenario data set goes up to 2005. Set scenario to historical for years before 2006")
  } 
  if(check_download(model, scenario, variable, year) == FALSE) {
    
  x = "r1i1p1"
  change_x = c("ACCESS1-3"="r1i1p1", "CCSM4" = "r6i1p1", "GISS-E2-H" = "r6i1p3", "GISS-E2-R" = "r6i1p1", "EC-EARTH" = "r8i1p1")
  if(model %in% names(change_x)) {
    x = change_x[[model]]
  }
    file_path = paste0("ftp://gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/loca/LOCA_2016-04-02/", model, "/16th/", scenario, "/", x, "/", variable, "/",
                       variable, "_day_", model, "_", scenario, "_", x, "_", year,"0101-", year, "1231.LOCA_2016-04-02.16th.nc")
    
    folder_path = file.path("../Downloaded Files/Temperature", model, scenario)
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  download.file(file_path, paste0(folder_path, "/", model, "_", scenario, "_", year, "_", variable, ".nc"), method = "libcurl", quiet = quiet_download, mode = "wb")
  }
}

download_humidity = function(model, scenario, variable, year, quiet_download = TRUE) {
  #Downloads relative hydrological data. File path currently called are available but are currently not downloaded)
  if(year > 2005 & scenario == "historical") {
    print("historical scenario data set only goes up to 2005. Set scenario to rcp45 or rcp85 for years after 2005")
  } 
  if(year < 2006 & scenario != "historical") {
    print("historical scenario data set goes up to 2005. Set scenario to historical for years before 2006")
  } 
  if(check_download(model, scenario, variable, year, type = "Relative Humidity") == FALSE) {
    
  file_path = paste0("ftp://gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/loca_hydro/LOCA_VIC_dpierce_2017-02-28/", model, "/vic_output.", scenario, ".netcdf/", variable, ".", year,".v0.nc")
  
  folder_path = file.path("../Downloaded Files/Relative Humidity", model, scenario)
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  download.file(file_path, paste0(folder_path, "/", model, "_", scenario, "_", year, "_", variable, ".nc"), method = "libcurl", quiet = quiet_download, mode = "wb")
  }
}
#_____________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________
#Downloads 10 models for historical period and future period for scenarios
historical_period = 1980:1990
future_period = c(2010:2020, 2040:2050)
#10 subset given from 4th California Climate Assessment which can be found on the What is LOCA? page at https://loca.ucsd.edu/
models = c("ACCESS1-0", "CCSM4", "CESM1-BGC",	"CMCC-CMS",	"CNRM-CM5",	"CanESM2", "GFDL-CM3",	"HadGEM2-CC",	"HadGEM2-ES", "MIROC5")
scenario = "rcp85"


#Downloads historical model
for(model in models) {
  print(model)
  for(year in historical_period) {
    print(year)
    download_temp(model, "historical", "tasmax", year)
    download_temp(model, "historical", "tasmin", year)
    download_humidity(model, "historical", "relHumid", year)
  }
}


#Downloads future periods
for(model in models) {
  print(model)
  # for(year in c(2010:2020, 2040:2050, 2070:2080)) {
  for(year in c(future_period)) {
    print(year)
    download_temp(model, scenario, "tasmax", year)
    download_temp(model, scenario, "tasmin", year)
    download_humidity(model, scenario, "relHumid", year)
  }
}



