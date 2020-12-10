# Author: Xiang Chen
# Last updated: 2020.11.28

# 1. Preparation 
## Check for packages needed to run analyses/install worldmet, AirSensor, and PWFSLSmoke package.
# rm(list=ls())
pckgs <- c("here","tidyverse","magrittr","broom","skimr","knitr","rmarkdown",
           "devtools","MazamaCoreUtils","MazamaSpatialUtils","AirSensor","PWFSLSmoke")
pckgs <- c(pckgs,"ggtext","jsonlite")
sapply(pckgs,function(x)
        if(!require(x,character.only = T,quietly = T)){
                install.packages(x)
                require(x,character.only = T)
        }
)
rm(pckgs)

## Below may take a few minutes because of the size of the data packages.
## Install the worldmet package and dependencies.
## It's a forked version includes fix.
## Package for accessing NOAA Integrated Surface Database (ISD) meteorological observations.
if(!require("worldmet")){
        devtools::install_github("MazamaScience/worldmet")
        require("worldmet")
}

## Install the AirMonitorPlots package and dependencies.
## Package for accessing EPA air data.
if(!require("AirMonitorPlots")){
        install_github('mazamascience/AirMonitorPlots')
        require("AirMonitorPlots")
}

## Download spatial data from the MazamaSpatialUtils package
if(!dir.exists(here("data","raw","spatial"))){
        dir.create(here("data","raw","spatial"), recursive=TRUE)
        MazamaSpatialUtils::setSpatialDataDir(here("data","raw","spatial"))
        MazamaSpatialUtils::installSpatialData()
}



