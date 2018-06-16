

for(i in 1:3){
  n1<-1+4*(i-1)
  n2<-4*i
  
  n2<-ifelse(n2>9,9,n2)
  bselect<-basins2[n1:n2]
  dfplot<-dfPlotBasin %>% filter(Basin %in% bselect)
  
  p4<-ggplot(dfplot) + 
    theme_minimal() + facet_wrap(~Basin, nrow=3, ncol=2, scales="free",labeller = label_parsed) +
    #geom_point(aes(x=Year,y=ER,colour=Scenario),shape=1, alpha=0.1)  + 
    geom_line(aes(x=Year,y=ER,colour=Scenario, alpha=0.1),show_guide = FALSE) +
    geom_line(aes(x=Year,y=ER_5yr,colour=Scenario)) +
    geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
    coord_cartesian(ylim=c(0,2.5)) +
    scale_color_brewer(palette="Set1") + 
    labs(y="Eutrophication Ratio", 
         title="HEAT Baltic Basins")
  print(p4)
  
  if(length(bselect)<2){
    figh<-6.6
    figw<-13.6
  }else{
    figh<-12
    figw<-24
  }
  
  fig<-paste0("./figures/Scenarios_basins_",i,".png")
  ggsave(p4,filename=fig, width = figw, height = figh, units = "cm", dpi=300)
  
}



#----------- Check target values ---------------------------------------

Variable<-c("chl_summer","din_winter","dip_winter","O2debt","secchi_summer")
Parameter<-c("Chla","DIN","PO4","O2debt","Secchi")
dfvar<-data.frame(Variable,Parameter,stringsAsFactors = F)

# distinct target values (model)
targets_model <- df %>% filter(Year==2200) %>%
  distinct(Parameter,Basin,StnID,Target,Unit,Response) %>%
  arrange(Parameter,Basin,StnID)

# check that there are not duplicate thresholds
testcount <-targets_model %>% group_by(Parameter,Basin) %>%
  summarise(n=n()) %>% 
  filter(n>1)

# distinct target values (model)
targets_obs <- targets.sas7bdat %>% filter(Year==2011) %>%
  distinct(Variable,Basin,Value,Weight) %>%
  arrange(Variable,Basin)

targets_obs <- targets_obs %>%
  left_join(dfvar,by="Variable") %>%
  mutate(Basin=gsub("_"," ",Basin)) %>%
  select(Basin,Parameter,Value,Weight)

targets_model <- targets_model %>%
  left_join(targets_obs,by=c("Basin","Parameter"))
# ----------------------------------------------------


my_label_parsed <- function (variable, value) {
  if (variable == "r2") {
    llply(as.character(value), function(x) parse(text = x))    
  } else {
    return(as.character(value))
  }
}




# ----------- Get Basin areas -----------
BasinInfo<-read.table(file="data/Basin_area.txt", header=TRUE,sep=",",stringsAsFactors=FALSE)
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

df7<-left_join(df5,df6,by=c("Scenario","Year","CriteriaID",
                            "Variable","Unit","Response"))


#saveRDS(df7,file="")

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

df14<-left_join(df12,df13,by=c("Scenario","Year","StnID","Basin"))



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


