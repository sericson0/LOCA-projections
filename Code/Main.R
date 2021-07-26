rm(list = ls())
loadPackage = function(package) {
  if(!require(package, character.only = TRUE)){
    install.packages(package)
    require(package, character.only =TRUE)
  }
}
loadPackage("rstudioapi") 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#_____________________________________________________________________________
#_____________________________________________________________________________
#_____________________________________________________________________________
#Download data
#This file takes a long time to run and requires a substantial amount of harddrive space. Compiled files already uploaded so only run if you want to change or 
#Rerun full pipeline

source("Download_LOCA_Data.R")

#Process Data
#Takes some time to run. Processed data already loaded to files. Only works if raw data are downloaded
source("Process_LOCA_Data.R")

#Create Plots
#Can run without first two steps. 
source("Create_Maps.R")