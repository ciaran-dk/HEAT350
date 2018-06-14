
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


# --------------- import data file for 1850-2200 --------------

source('HEAT.R')
df <- readRDS("HEAT350.rds")
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


# QEdata - Calculate the Eutrophication Sum (EUT_SUM) by Quality Element (Criteria)

QE<-df %>%
  group_by(Scenario,StnID,Basin,Year,CriteriaID) %>%
  summarise(EUT_Sum=sum(EUT_Ratio_wt,rm.na=TRUE),Wt_Sum=sum(Weight,rm.na=TRUE))

QE$EUT_Ratio<-QE$EUT_Sum/QE$Wt_Sum
QE$EUT_Sum<-NULL
QE$Wt_Sum<-NULL


# Overall HEAT "score" is given by the worst Quality Element (greatest EUT_Ratio) 
HEAT<-QE %>%
  filter(!is.na(EUT_Ratio)) %>%
  group_by(Scenario,StnID,Basin,Year) %>%
  summarise(EUT_Ratio=max(EUT_Ratio))

# Add the ID of the Criteria with the worst score to the HEAT overall results
HEAT<-left_join(HEAT,QE,by=c("Scenario"="Scenario","StnID"="StnID","Basin"="Basin","Year"="Year","EUT_Ratio"="EUT_Ratio"))

# Transpose the QE resuts to give a column for each Quality Element (Criteria)
QEspr<-spread_(data=QE, key_col="CriteriaID" , value_col = "EUT_Ratio")

# Combine transposed QE results and overall HEAT results to give a table which has columns for each Criteria (QE) as well as the overall HEAT score,
# also showing the name of the worst QE
HEAT<-left_join(QEspr,HEAT,by=c("Scenario"="Scenario","StnID"="StnID","Basin"="Basin","Year"="Year"))

# ----------- Get Basin areas -----------
BasinInfo<-read.table(file="Basin_area.txt", header=TRUE,sep=",",stringsAsFactors=FALSE)
BasinInfo$BASINID<-as.integer(BasinInfo$BASINID)
BasinInfo$BASIN<-ifelse(BasinInfo$BASINID==2,"Danish Straits",BasinInfo$BASIN)
BasinInfo$BASIN<-ifelse(BasinInfo$BASINID==9,"Bothnian Bay",BasinInfo$BASIN)
BasinInfo$BASIN<-ifelse(BasinInfo$BASINID==8,"Bothnian Sea",BasinInfo$BASIN)
#BasinInfo$BASIN<-ifelse(BasinInfo$BASIN=="Gulf of Bothnia" && BasinInfo$BASINID==5,"Bothnian Sea",BasinInfo$BASIN)
BasinInfo$BASIN<-ifelse(BasinInfo$BASIN=="Gulf of Bothnia","Bothnian Sea",BasinInfo$BASIN)
BasinInfo$BASIN<-ifelse(is.na(BasinInfo$BASINID),NA,BasinInfo$BASIN)

Area<-BasinInfo %>%
  filter(!is.na(BASINID)) %>%
  group_by(BASIN) %>%
  summarise(Area_km2=sum(AREA_KM2))

# ----------- Calculate averages by Basis of variable absolute values and Eutrophication ratios (weighted by area and unweighted) -----------

df2<-df %>%
  filter(!is.na(EUT_Ratio)) %>%
  select(Scenario,StnID,Basin,Year,CriteriaID,Variable,Unit,Response,Status,EUT_Ratio)
HEAT2<-select(HEAT,Scenario,StnID,Basin,Year,EUT_Ratio)
HEAT2$Variable<-"HEAT"
HEAT2$Response<-NA
HEAT2$CriteriaID<-NA
HEAT2$Status<-HEAT2$EUT_Ratio
HEAT2$Unit<-NA
HEAT2<-ungroup(HEAT2)

df2<-rbind(df2,HEAT2)

df2<-left_join(df2,Area,by=c("Basin"="BASIN"))

df2$Status_Wt<-df2$Status*df2$Area_km2
df2$EUT_Ratio_Wt<-df2$EUT_Ratio*df2$Area_km2


df3<-df2 %>%
  group_by(Scenario,Year,CriteriaID,Variable,Unit,Response) %>%
  summarise(Status=mean(Status,rm.na=TRUE),
            EUT_Ratio=mean(EUT_Ratio,rm.na=TRUE),
            sum_Status_Wt=sum(Status_Wt,rm.na=TRUE),
            sum_EUT_Ratio_Wt=sum(EUT_Ratio_Wt,rm.na=TRUE),
            sum_Area_km2=sum(Area_km2,rm.na=TRUE)
  )

df3$Status_Wt<-df3$sum_Status_Wt/df3$sum_Area_km2
df3$EUT_Ratio_Wt<-df3$sum_EUT_Ratio_Wt/df3$sum_Area_km2
df3$sum_Status_Wt<-NULL
df3$sum_EUT_Ratio_Wt<-NULL
df3$sum_Area_km2<-NULL




nyears<-5
transp<-0.1

library(zoo)

#df3<-arrange(df3,Scenario,CriteriaID,Variable,Unit,Response,Year)
df4<-df3 %>% 
  group_by(Scenario,CriteriaID,Variable,Unit,Response) %>%
  mutate('Status_MA' = rollmean(Status, nyears, align="right", na.pad=TRUE ),
         'EUT_Ratio_MA' = rollmean(EUT_Ratio, nyears, align="right", na.pad=TRUE ),
         'Status_Wt_MA' = rollmean(Status_Wt, nyears, align="right", na.pad=TRUE ),
         'EUT_Ratio_Wt_MA' = rollmean(EUT_Ratio_Wt, nyears, align="right", na.pad=TRUE )
  )

df5<-select(df4,Scenario,Year,CriteriaID,Variable,Unit,Response,Status,EUT_Ratio,Status_Wt,EUT_Ratio_Wt)
df6<-select(df4,Scenario,Year,CriteriaID,Variable,Unit,Response,Status_MA,EUT_Ratio_MA,Status_Wt_MA,EUT_Ratio_Wt_MA)
nshift<-floor((nyears-1)/2)
df6$Year<-df6$Year-nshift

df7<-left_join(df5,df6,by=c("Scenario"="Scenario","Year"="Year","CriteriaID"="CriteriaID",
                            "Variable"="Variable","Unit"="Unit","Response"="Response"))

# ------------------------------ Plotting  ---------------------------------------------

require(ggplot2)
Scenarios<-c("BAU30","PLC55","BSAP","BSAP30")
Variables<-c("O2debt","chl_summer","secchi_summer","din_winter","dip_winter","HEAT")
Units<-c("[mg/l]","[µg/l]","[m]","[µM]","[µM]","")

plotdata<-df7 %>%
  filter(Scenario=="BSAP")
plotdata<-ungroup(plotdata)

plotdata$Variable<-ifelse(is.na(plotdata$Unit),plotdata$Variable, paste0(plotdata$Variable, " [", plotdata$Unit,"]"))
plotdata$Variable<-factor(plotdata$Variable,levels=c("din_winter [µM]","dip_winter [µM]","chl_summer [µg/l]","secchi_summer [m]","O2debt [mg/l]","HEAT"))

#plotdata$Variable<-factor(plotdata$Variable,levels=c("din_winter","dip_winter","chl_summer","secchi_summer","O2debt","HEAT"))
p<-ggplot(data=plotdata) + geom_point(aes(Year,Status),colour="#999999") + geom_line(aes(Year,Status_MA),size=1)  + facet_grid(Variable ~ ., scales="free_y")
p<-p+ xlab("Year") + ylab("") + ggtitle("HEAT Baltic Sea")
p

plotfile<-"./figures/BALTIC_HEAT.png"
ggsave(plotfile,p,dpi=600,width=15,height=20,units ="cm")





# ------------------------------ Plotting using function ---------------------------------------------

plotout<-function(df,SelectScen,SelectVar,Weighted=FALSE,Ratio=FALSE,Units=""){
  
  plotdata<-df %>%
    filter(Scenario==SelectScen,Variable==SelectVar)

  xlabel<-expression(paste("Year",sep=""))
  title<-expression(paste("Eutrophication Ratio (Baltic)",sep=""))
  ylabel<-SelectVar
                 
  p<-ggplot(data = plotdata)
  if(Weighted==TRUE){
    if(Ratio==TRUE){
      p<-p + geom_point(aes(Year,EUT_Ratio_Wt),colour="#999999") + geom_line(aes(Year,EUT_Ratio_Wt_MA),size=1) 
      plotfile<-paste0("./figures/",SelectScen,"_",SelectVar,"_ratio_wt.png")
      ylabel<-paste0(SelectVar, " (EUT Ratio)")
   }else{
      p<-p + geom_point(aes(Year,Status_Wt),colour="#999999") + geom_line(aes(Year,Status_Wt_MA),size=1) 
      plotfile<-paste0("./figures/",SelectScen,"_",SelectVar,"_value_wt.png")
      ylabel<-paste0(SelectVar, " ", Units)
   }
  }else{
    if(Ratio==TRUE){
      p<-p + geom_point(aes(Year,EUT_Ratio),colour="#999999") + geom_line(aes(Year,EUT_Ratio_MA),size=1) 
      plotfile<-paste0("./figures/",SelectScen,"_",SelectVar,"_ratio.png")
      ylabel<-paste0(SelectVar, " (EUT Ratio)")
    }else{
      p<-p + geom_point(aes(Year,Status),colour="#999999") + geom_line(aes(Year,Status_MA),size=1) 
      plotfile<-paste0("./figures/",SelectScen,"_",SelectVar,"_value.png")
      ylabel<-paste0(SelectVar, " ", Units)
    }
  }
  p<-p + xlab(xlabel) + ylab(ylabel) #+ ggtitle(title)
  #p 
  ggsave(plotfile,p,dpi=600,width=15,height=6,units ="cm")
  
}

#

# --- plotting heat by basin ----------------------------------------------------------------------
library(ggplot2)
library(zoo)

StnID <- c("KA","DS","AR","BN","GR","GF","BS","BB","GS")
StnName <- c("Kattegat","Danish Straits","Arkona Basin","Bornholm Basin",
            "Gulf of Riga","Gulf of Finland","Bothnian Sea","Bothnian Bay","Baltic Proper")

nyears<-5
df11<-HEAT %>% select(Scenario,StnID,Basin,Year,EUT_Ratio) %>%
  group_by(Scenario,StnID,Basin) %>%
  mutate('EUT_Ratio_MA' = rollmean(EUT_Ratio, nyears, align="right", na.pad=TRUE ))

df12<-select(df11,Scenario,StnID,Basin,Year,EUT_Ratio)
df13<-select(df11,Scenario,StnID,Basin,Year,EUT_Ratio_MA)
nshift<-floor((nyears-1)/2)
df13$Year<-df13$Year-nshift

df14<-left_join(df12,df13,by=c("Scenario"="Scenario","Year"="Year","StnID"="StnID","Basin"="Basin"))



for(i in 1:length(StnID)){
  plotdata<-df14 %>%
    filter(Scenario=="BSAP",Basin==StnName[i]) %>%
    ungroup()
  p<-ggplot(data = plotdata)
  p<-p + geom_point(aes(Year,EUT_Ratio),colour="#999999") + geom_line(aes(Year,EUT_Ratio_MA),size=1) 
  p<-p + xlab("Year") + ylab("EUT Ratio") + ggtitle(StnName[i])
  p<-p + geom_hline(yintercept = 1,linetype=2, colour="red")
  p<-p + coord_cartesian(ylim=c(0.5,1.9)) + theme_minimal(base_size=11)
  plotfile<-paste0("./figures/basins/EUT_",StnName[i],".png")
  ggsave(plotfile,p,dpi=100,width=9,height=5,units ="cm")  
  
}
 p 
  p
  

