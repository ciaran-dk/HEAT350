
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
library(plyr)
library(tidyr)
library(sqldf)

StnID = c("KA","DS","AR","BN","GS","GR","GF","BS","BB")
StnName = c("Kattegat","Danish Straits","Arkona Basin","Bornholm Basin","Baltic Proper",
            "Gulf of Riga","Gulf of Finland","Bothnian Sea","Bothnian Bay")
OD=c(0,0,1,1,1,1,1,1,1) # indicates if there is an oxygen debt dataset for this WB

dfStns = data.frame(StnID,StnName,OD, stringsAsFactors = FALSE)
dfStns[,"StnID"] <- factor(dfStns[,"StnID"], levels = StnID)


Scenario<-c("nhiresaffBAU30years_2","nhiresaffPLC55_2",
            "nhiresaffBSAP30years_2","nhiresaffBSAP_2")
ScenarioID<-c("BAU30","PLC55","BSAP30","BSAP")
Scenarios<-data.frame(ScenarioID,Scenario,stringsAsFactors = FALSE)
Scenarios[,"ScenarioID"] <- factor(Scenarios[,"ScenarioID"], levels = ScenarioID)

Criteria<-c("Causative factors","Direct effects","Indirect effects")
CriteriaID<-c("C1","C2","C3")
#Criteria<-data.frame(Criteria)

n<-0
for(i in 1:nrow(Scenarios)) {
  for(j in 1:nrow(dfStns)) {
    n<-n+1
    
    filename<-print(paste0("./data/bärbel/",Scenarios[i,"Scenario"],"/",dfStns[j,"StnID"],".txt"))
    print(filename)
    dftemp<-read.table(filename, header=TRUE,sep="\t", stringsAsFactors=FALSE)
    dftemp$Scenario<-Scenarios[i,"ScenarioID"]
    dftemp$StnID<-dfStns[j,"StnID"]
    if (exists("df") && is.data.frame(get("df"))){
      df<-bind_rows(df,dftemp)
    }else{
      df<-dftemp
    }
    rm(list=c("dftemp","filename"))
    
    if(dfStns[j,"OD"]==1){
      filename<-print(paste0("./data/bärbel/",Scenarios[i,"Scenario"],"/",dfStns[j,"StnID"],"_O2debt.txt"))
      print(filename)
      dftemp<-read.table(filename, header=TRUE,sep="\t", stringsAsFactors=FALSE)
      dftemp$Scenario<-Scenarios[i,"ScenarioID"]
      dftemp$StnID<-dfStns[j,"StnID"]
      if (exists("dfOD") && is.data.frame(get("dfOD"))){
        dfOD<-bind_rows(dfOD,dftemp)
      }else{
        dfOD<-dftemp
      }
      rm(list=c("dftemp","filename"))
    }
    
  }
}

Parameters<-c("O2debt")
for(i in 1:length(Parameters)){
  ColList<-c("Scenario","StnID","Year",Parameters[i])
  dftemp<-dfOD[,ColList]
  dftemp$Parameter<-Parameters[i]
  names(dftemp)[names(dftemp) == Parameters[i]] <- "Status"
  if (exists("dflong") && is.data.frame(get("dflong"))){
    dflong<-bind_rows(dflong,dftemp)
  }else{
    dflong<-dftemp
  }
  rm(list=c("dftemp"))
}

Parameters<-c("Chla","Secchi","DIN","PO4")
for(i in 1:length(Parameters)){
  ColList<-c("Scenario","StnID","Year",Parameters[i])
  dftemp<-df[,ColList]
  dftemp$Parameter<-Parameters[i]
  names(dftemp)[names(dftemp) == Parameters[i]] <- "Status"
  if (exists("dflong") && is.data.frame(get("dflong"))){
    dflong<-bind_rows(dflong,dftemp)
  }else{
    dflong<-dftemp
  }
  rm(list=c("dftemp"))
}

targets<-read.table('./data/targets.txt', header=TRUE,sep="\t", stringsAsFactors=FALSE,quote="", encoding="UTF-8")
targets<-left_join(targets,dfStns[,c("StnID","StnName")],by=c("Basin"="StnName"))
targets[,"Criteria"]<-factor(targets[,"Criteria"],levels=Criteria)
targets[,"CriteriaID"]<-factor(targets[,"CriteriaID"],levels=CriteriaID)

df<-dflong
rm(list=c("dflong","dfOD"))

df<-left_join(df,targets)

df$Response<-ifelse(df$Parameter %in% c("Secchi"),"-","+")

saveRDS(df, file="data/HEAT350.rds")

