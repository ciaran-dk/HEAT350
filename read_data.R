
# KA - Kattegat
# DS - Danish Straits
# AR - Arkona Basin
# BN - Bornholm Basin
# GS - Baltic Proper
# GR - Gulf of Riga
# GF - Gulf of Finland
# BS - Bothnian Sea
# BB - Bothnian Bay

rm(list=ls())

library(dplyr)
library(tidyr)
library(sqldf)
library(sas7bdat)

AreaWeights<-TRUE
#AreaWeights<-FALSE


# --------------- Get HEAT 100m years data - we are using the same target values --------------
folder<-"C:/Data/GitHub/HEAT_100yrs/SAS apr2015/sas_data"

files<-list.files(path=folder)
filepaths<-paste0(folder,"/",files)


SASdata <- lapply(filepaths, read.sas7bdat)
for(i in 1:length(files)){
  print(files[i])
  assign(files[i], data.frame(SASdata[i]))
}
