
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
library(zoo)
library(ggplot2)
library(RColorBrewer)

bPrint<-F

# --------------- Get HEAT 100m years data - we are using the same target values --------------
folder<-"C:/Data/GitHub/HEAT350/data/100yrs/SAS data apr2015"
files<-list.files(path=folder)
filepaths<-paste0(folder,"/",files)


SASdata <- lapply(filepaths, read.sas7bdat)
for(i in 1:length(files)){
  print(files[i])
  assign(files[i], data.frame(SASdata[i]))
}

obs<-heat3targrev_obs.sas7bdat %>% 
  filter(!is.nan(Value)) %>%
  mutate(basin=gsub("_"," ",basin)) %>%
  select(Year=year,Basin=basin,Variable=variable,Value,StdErr_lo,StdErr_up)


# --------------- import data file for 1850-2200 --------------

df <- readRDS("data/HEAT350.rds")
df$EUT_Ratio<-ifelse(df$Response=="+",df$Status/df$Target,df$Target/df$Status)

# Create a data frame to match parameter names in 350 yr data to variable names in the target information
Parameter<-c("O2debt","Chla","Secchi","DIN","PO4")
Variable<-c("O2debt","chl_summer","secchi_summer","din_winter","dip_winter")
parameters<-data.frame(Parameter,Variable)

# Add the variable names to df
df<-left_join(df,parameters,c("Parameter"="Parameter"))

# Find the variable (indicator) weights within criteria from 100 yr target information 
weights<-distinct(select(targets.sas7bdat,Basin,Variable,Weight))
weights$Basin <- gsub("_", " ", weights$Basin)

# Add weights to df
df<-left_join(df,weights,by=c("Basin"="Basin","Variable"="Variable"))

# Calculate weighted EUT_ratio
df$EUT_Ratio_wt<-df$EUT_Ratio*df$Weight

df<- df %>% left_join(obs,by=c("Basin","Year","Variable")) %>%
  select(Scenario,StnID,Basin,Year,Variable,Unit,Target,Model=Status,Obs=Value)


basins<-c("Kattegat","Danish Straits","Arkona Basin","Bornholm Basin","Baltic Proper","Gulf of Riga","Gulf of Finland","Bothnian Sea","Bothnian Bay")
basins3<-c("Bornholm~Basin","Baltic~Proper","Gulf~of~Finland")
basins2<-gsub(" ","~",basins)
df$Basin <- factor(df$Basin,levels=basins,labels=basins2)

distinct(df,Variable)
vars<-c("din_winter","dip_winter","chl_summer","secchi_summer","O2debt")
#vars2<-c("Winter~DIN","Winter~DIP","Summer~Chl~a","Summer Secchi","O2~debt")
vars2<-c("Winter DIN","Winter DIP","Summer Chl a","Summer Secchi","O2 debt")
df$Variable<-factor(df$Variable,levels=vars,labels=vars2)

# ----- do the linear regression of model on obs ------------------------

models<-df %>%filter(!is.na(Model),!is.na(Obs)) %>%
  group_by(Scenario,Basin,Variable) %>% 
  do(mod=lm(Model~Obs,data = .))

for(i in 1:nrow(models)){
  p<-anova(models$mod[[i]])[1,5]
  models$p[i]<-p
  r2<-summary(models$mod[[i]])[["adj.r.squared"]]
  models$r2[i]<-r2
}
models <- models %>% arrange(Scenario,Variable,Basin) %>% 
  mutate(p=ifelse(p<0.001,"p<0.001",paste0("p=",round(p,3))),r2=(paste0("R^2*'='~'",round(r2,2),"'")),text=paste0(r2,"\n",p))

df <- df %>% left_join(select(models,Scenario,Basin,Variable,r2,p,text),by=c("Scenario","Basin","Variable")) %>%
  mutate(r2=ifelse(is.na(r2),"",r2),p=ifelse(is.na(p),"",p),text=ifelse(is.na(text),"",text)) 


if(bPrint){saveRDS(df,file="results/model_vs_obs_variables_regressions.rds")}


# ----- gather obs/model for time series plots  ------------------------

df<-df %>% gather(key="Param",value="Value",Model,Obs)


ntot<-nrow(df)
ny<-nrow(distinct(df,Year))
n<-ntot/ny

for(j in 1:n)
  for(i in 1:ny){
    nfrom<-(j-1)*ny+max(1,i-2)
    nto<-(j-1)*ny+min(ny,i+2)
    if(sum(is.na(df$Value[nfrom:nto]))<3){
      df$Value_5yr[(j-1)*ny+i] <- mean(df$Value[nfrom:nto],na.rm=T)
    }else{df$Value_5yr[(j-1)*ny+i] <- NA}
  }


if(bPrint){saveRDS(df,file="results/model_vs_obs_variables.rds")}


