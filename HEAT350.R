
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
#library(wesanderson)

#Source the HEAT assessment calculation routine
#source('HEAT.R')
#AreaWeights<-TRUE
AreaWeights<-FALSE
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
HEAT<-left_join(HEAT,QE,by=c("Scenario","StnID","Basin","Year","EUT_Ratio"))

# Transpose the QE resuts to give a column for each Quality Element (Criteria)
QEspr<-spread_(data=QE, key_col="CriteriaID" , value_col = "EUT_Ratio")

# Combine transposed QE results and overall HEAT results to give a table which has columns for each Criteria (QE) as well as the overall HEAT score,
# also showing the name of the worst QE
HEAT<-left_join(QEspr,HEAT,by=c("Scenario","StnID","Basin","Year"))


# --------------- import HEAT file for observations --------------
dfBasin <- readRDS("data/BioReviews.rds") %>% 
  mutate(Basin=gsub("_"," ",Basin)) %>% 
  select(-nYrs5) %>% 
  rename(ER_obs=nYrs1)

# filtering where ER_obs for HEAT <1.5
dfBasinSelect<-dfBasin %>% filter(Parameter=="HEAT",ER_obs<=1.5) %>% select(Basin,Year)
dfBasin<-dfBasinSelect %>% left_join(dfBasin, by=c("Basin","Year"))


dfBasin <- HEAT %>% rename(HEAT=EUT_Ratio) %>% 
  select(-CriteriaID) %>% 
  gather(key="Parameter",value="ER",C1,C2,C3,HEAT) %>%
  left_join(dfBasin,by=c("Basin","Parameter","Year")) %>%
  ungroup()

  
basins<-c("Kattegat","Danish Straits","Arkona Basin","Bornholm Basin","Baltic Proper","Gulf of Riga","Gulf of Finland","Bothnian Sea","Bothnian Bay")
basins3<-c("Bornholm~Basin","Baltic~Proper","Gulf~of~Finland")
basins2<-gsub(" ","~",basins)
dfBasin$Basin <- factor(dfBasin$Basin,levels=basins,labels=basins2) #,labels=gsub("_"," ",basins))
 
maxER<-4
dfBasin <- dfBasin %>% mutate(ER_obs=ifelse(ER_obs>4,maxER,ER_obs)) #%>% filter(Year>=1980)


Parameter<-c("C1","C2","C3","HEAT")
ParamLong<-c("C1 - Nutrients","C2 - Direct Effects","C3 - Indirect Effects","HEAT - Overall Status")
dfparam<-data.frame(Parameter,ParamLong)


models<-dfBasin %>%filter(!is.na(ER_obs),!is.na(ER)) %>%
  group_by(Scenario,Basin,Parameter) %>% 
  do(mod=lm(ER~ER_obs,data = .))

for(i in 1:nrow(models)){
  p<-anova(models$mod[[i]])[1,5]
  models$p[i]<-p
  r2<-summary(models$mod[[i]])[["adj.r.squared"]]
  models$r2[i]<-r2
}

models <- models %>% arrange(Basin,Parameter,Scenario) %>% 
  mutate(p=ifelse(p<0.001,"p<0.001",paste0("p=",round(p,3))),r2=(paste0("R^2*'='~'",round(r2,2),"'")),text=paste0(r2,"\n",p))

dfBasin <- dfBasin %>% left_join(select(models,Scenario,Basin,Parameter,r2),by=c("Scenario","Basin","Parameter")) %>%
  mutate(r2=ifelse(is.na(r2),"",r2)) %>%
  mutate(ER_obs=ifelse(Parameter=="C3" & Basin %in% c("Kattegat","Danish Straits","Arkona Basin","Gulf of Riga","Bothnian Sea","Bothnian Bay"),
                       NA,ER_obs))
         


#for(scen in c("BAU30","PLC55","BSAP30","BSAP")){
for(scen in c("BSAP")){
  for(param in c("HEAT","C1","C2","C3")){
  #for(param in c("HEAT")){
      desc<-dfparam %>% filter(Parameter==param)
    desc<-desc[1,2]
    
    dfplot<-dfBasin %>% filter(Parameter==param,Scenario==scen) %>%
      mutate(label=paste0(Basin,"~(",r2,")"))
    order <- dfplot %>% group_by(Basin,label) %>% summarise() %>% arrange(Basin)
    dfplot$label<-factor(dfplot$label, levels=order$label)
    
    if(param=="C3"){
      dfplot<-dfplot %>% filter(Basin %in% basins3)
      figh<-7
      figw<-15
      }else{
      figh<-12
      figw<-24
      }
    
    p<-ggplot(dfplot,aes(x=ER_obs,y=ER)) + facet_wrap(~label, nrow=2, ncol=5, scales="free",labeller = label_parsed) +
      geom_point(shape=1,colour="#000000", alpha=0.6)  + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.1,fill="#0000ff") +
      geom_line(stat='smooth',method="lm",alpha=0.6,formula=y~x,colour="#0000ff") +
      labs(title=desc,
           subtitle=paste0("Observations vs. BALTSEM"," [",scen,"]"),
           y="Eutrophication Ratio (Model)",
           x="Eutrophication Ratio (Observed)") +
      theme_minimal() + theme(strip.text.x = element_text(size=8)) +
      scale_color_brewer(palette="Set1") +
      coord_cartesian(xlim=c(0.5,1.5),ylim=c(0.5,1.5))

    print(p)
    
    fig<-paste0("./figures/obs_vs_model_",scen,"_",param,"_lt15.png")
    if(bPrint){ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
    
  }
}

# --------------- import HEAT file for observations - NO filtering --------------
dfBasin <- readRDS("data/BioReviews.rds") %>% 
  mutate(Basin=gsub("_"," ",Basin)) %>% 
  select(-nYrs5) %>% 
  rename(ER_obs=nYrs1)

dfBasin <- HEAT %>% rename(HEAT=EUT_Ratio) %>% 
  select(-CriteriaID) %>% 
  gather(key="Parameter",value="ER",C1,C2,C3,HEAT) %>%
  left_join(dfBasin,by=c("Basin","Parameter","Year")) %>%
  ungroup()


basins<-c("Kattegat","Danish Straits","Arkona Basin","Bornholm Basin","Baltic Proper","Gulf of Riga","Gulf of Finland","Bothnian Sea","Bothnian Bay")
basins3<-c("Bornholm~Basin","Baltic~Proper","Gulf~of~Finland")
basins2<-gsub(" ","~",basins)
dfBasin$Basin <- factor(dfBasin$Basin,levels=basins,labels=basins2) #,labels=gsub("_"," ",basins))

maxER<-4
dfBasin <- dfBasin %>% mutate(ER_obs=ifelse(ER_obs>4,maxER,ER_obs)) #%>% filter(Year>=1980)

Parameter<-c("C1","C2","C3","HEAT")
ParamLong<-c("C1 - Nutrients","C2 - Direct Effects","C3 - Indirect Effects","HEAT - Overall Status")
dfparam<-data.frame(Parameter,ParamLong)


models<-dfBasin %>%filter(!is.na(ER_obs),!is.na(ER)) %>%
  group_by(Scenario,Basin,Parameter) %>% 
  do(mod=lm(ER~ER_obs,data = .))

for(i in 1:nrow(models)){
  p<-anova(models$mod[[i]])[1,5]
  models$p[i]<-p
  r2<-summary(models$mod[[i]])[["adj.r.squared"]]
  models$r2[i]<-r2
}

models <- models %>% arrange(Basin,Parameter,Scenario) %>% 
  mutate(p=ifelse(p<0.001,"p<0.001",paste0("p=",round(p,3))),r2=(paste0("R^2*'='~'",round(r2,2),"'")),text=paste0(r2,"\n",p))

dfBasin <- dfBasin %>% left_join(select(models,Scenario,Basin,Parameter,r2),by=c("Scenario","Basin","Parameter")) %>%
  mutate(r2=ifelse(is.na(r2),"",r2)) %>%
  mutate(ER_obs=ifelse(Parameter=="C3" & Basin %in% c("Kattegat","Danish Straits","Arkona Basin","Gulf of Riga","Bothnian Sea","Bothnian Bay"),
                       NA,ER_obs))

#for(scen in c("BAU30","PLC55","BSAP30","BSAP")){
for(scen in c("BSAP")){
  for(param in c("HEAT","C1","C2","C3")){
    #for(param in c("HEAT")){
    desc<-dfparam %>% filter(Parameter==param)
    desc<-desc[1,2]
    
    dfplot<-dfBasin %>% filter(Parameter==param,Scenario==scen) %>%
      mutate(label=paste0(Basin,"~(",r2,")"))
    order <- dfplot %>% group_by(Basin,label) %>% summarise() %>% arrange(Basin)
    dfplot$label<-factor(dfplot$label, levels=order$label)
    
    if(param=="C3"){
      dfplot<-dfplot %>% filter(Basin %in% basins3)
      figh<-7
      figw<-15
    }else{
      figh<-12
      figw<-24
    }
    
    p<-ggplot(dfplot,aes(x=ER_obs,y=ER)) + facet_wrap(~label, nrow=2, ncol=5, scales="free",labeller = label_parsed) +
      geom_point(shape=1,colour="#000000", alpha=0.6)  + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.1,fill="#0000ff") +
      geom_line(stat='smooth',method="lm",alpha=0.6,formula=y~x,colour="#0000ff") +
      labs(title=desc,
           subtitle=paste0("Observations vs. BALTSEM"," [",scen,"]"),
           y="Eutrophication Ratio (Model)",
           x="Eutrophication Ratio (Observed)") +
      theme_minimal() + theme(strip.text.x = element_text(size=8)) +
      scale_color_brewer(palette="Set1") +
      coord_cartesian(xlim=c(0,4),ylim=c(0,3))
    
    print(p)
    
    fig<-paste0("./figures/obs_vs_model_",scen,"_",param,".png")
    if(bPrint){ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
    
  }
}

# ------------------ end ------------------------

models<-dfBasin %>%filter(!is.na(ER_obs),!is.na(ER)) %>%
  group_by(Scenario,Basin,Parameter) %>% 
  do(mod=lm(ER~ER_obs,data = .))

for(i in 1:nrow(models)){
  p<-anova(models$mod[[i]])[1,5]
  models$p[i]<-p
  r2<-summary(models$mod[[i]])[["adj.r.squared"]]
  models$r2[i]<-r2
}

models <- models %>% ungroup() %>% 
  filter(Scenario==scen) %>% select(-c(mod,p)) %>%
  mutate(r2=round(r2,2)) %>%
  spread(key="Parameter",value="r2")



# ------- Baltic Averages---------------------------------
nyears<-5
transp<-0.1

dfBaltic <- dfBasin %>% #
  ungroup() %>% 
  filter(Parameter=="HEAT") %>%
  group_by(Scenario,Year) %>% 
  summarise(ER=mean(ER,na.rm=T), ER_obs=mean(ER_obs,na.rm=T)) %>%
  mutate(ER_obs=ifelse(is.nan(ER_obs),NA,ER_obs)) %>%
  rename(obs=ER_obs,model=ER) 

nmax<- nrow(distinct(ungroup(dfBaltic),Year))
nscen<- nrow(distinct(ungroup(dfBaltic),Scenario))
#nmax<-nrow(dfBaltic)
for(s in 1:nscen){
for(i in 1:nmax){
  noffset<-(s-1)*nmax
  nfrom<- i-2
  nto<- i+2
  nfrom<-ifelse(nfrom<1,1,nfrom)
  nto<-ifelse(nto>nmax,nmax,nto)
  nfrom=nfrom+noffset
  nto=nto+noffset
  if(sum(is.na(dfBaltic$model[nfrom:nto]))<3){
    dfBaltic$model_5yr[i+noffset] <- mean(dfBaltic$model[nfrom:nto],na.rm=T)
  }else{dfBaltic$model_5yr[i+noffset] <- NA}
  if(sum(is.na(dfBaltic$obs[nfrom:nto]))<3){
    dfBaltic$obs_5yr[i+noffset] <- mean(dfBaltic$obs[nfrom:nto],na.rm=T)
  }else{dfBaltic$obs_5yr[i+noffset] <- NA}
}}

dfBaltic<-dfBaltic %>% 
  mutate(obs_5yr=ifelse(is.nan(obs_5yr),NA,obs_5yr),
         model_5yr=ifelse(is.nan(model_5yr),NA,model_5yr)
         )

dfBaltic5<-dfBaltic %>% select(Scenario,Year,model=model_5yr,obs=obs_5yr) %>%
  gather(key="Param",value="ER_5yr",obs,model)
dfBaltic<-dfBaltic %>% select(Scenario,Year,model,obs) %>%
  gather(key="Param",value="ER",obs,model) %>%
  left_join(dfBaltic5,by=c("Scenario","Year","Param"))

dfplot<-dfBaltic %>% filter(Scenario==scen, Year %in% c(1900:2020))
p1<-ggplot(dfplot) + 
  theme_minimal() +
  geom_point(aes(x=Year,y=ER,colour=Param),shape=1)  + 
  geom_line(aes(x=Year,y=ER_5yr,colour=Param)) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5))  +
  scale_color_brewer(palette="Set1",name="HEAT") + 
  labs(y="Eutrophication Ratio", 
       title="HEAT Baltic",subtitle="Observations vs. BALTSEM")
p1


dfplot<-dfBaltic %>% filter(Param=="model")
p2<-ggplot(dfplot) + 
  theme_minimal() +
  geom_point(aes(x=Year,y=ER,colour=Scenario),shape=1)  + 
  geom_line(aes(x=Year,y=ER_5yr,colour=Scenario)) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  scale_color_brewer(palette="Set1") + 
  labs(y="Eutrophication Ratio", 
       title="HEAT Baltic",subtitle="BALTSEM Scenarios")
p2

p2a<-ggplot(dfplot) + 
  theme_minimal() +
  # geom_ribbon(aes(ymin=0,ymax=0.5,x=Year),fill="#007eff",alpha="0.2")+
  # geom_ribbon(aes(ymin=0.5,ymax=1,x=Year),fill="#00d600",alpha="0.2")+
  # geom_ribbon(aes(ymin=1,ymax=1.5,x=Year),fill="#ffff00",alpha="0.2")+
  # geom_ribbon(aes(ymin=1.5,ymax=2,x=Year),fill="#ff8c2b",alpha="0.2")+
  # geom_ribbon(aes(ymin=2,ymax=2.5,x=Year),fill="#ff0000",alpha="0.2")+
  geom_ribbon(aes(ymin=0,ymax=1,x=Year),fill="#00d600",alpha="0.2")+
  geom_ribbon(aes(ymin=1,ymax=2.5,x=Year),fill="#ff0000",alpha="0.2")+
  geom_point(aes(x=Year,y=ER,colour=Scenario),shape=1)  + 
  geom_line(aes(x=Year,y=ER_5yr,colour=Scenario)) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  scale_color_brewer(palette="Set1") + 
  geom_text(label="GOOD",x=1860,y=0.2,hjust="left") +
  geom_text(label="NOT GOOD",x=1860,y=2.3,hjust="left") +
  labs(y="Eutrophication Ratio", 
       title="HEAT Baltic",subtitle="BALTSEM Scenarios") 
p2a

figh<-12
figw<-24
fig<-paste0("./figures/obs_vs_model_Baltic.png")
if(bPrint){ggsave(p1,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
fig<-paste0("./figures/Scenarios_Baltic.png")
if(bPrint){ggsave(p2,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
fig<-paste0("./figures/Scenarios_Baltic_A.png")
if(bPrint){ggsave(p2a,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}


# ---------- plot scenario results for basins --------------------------

dfPlotBasin<- dfBasin %>% 
  mutate(ER_5yr=ER,ER_10yr=ER,ER_30yr=ER,ER_obs_5yr=ER_obs,ER_obs_10yr=ER_obs,ER_obs_30yr=ER_obs) %>%
  arrange(Scenario,Parameter,Basin,Year)
#  select(-c(ER_obs,r2)) %>%
#  filter(Parameter=="HEAT") %>%
  

nmax<- nrow(distinct(ungroup(dfPlotBasin),Year))
nbasin<- nrow(distinct(ungroup(dfPlotBasin),Basin))
npar<- nrow(distinct(ungroup(dfPlotBasin),Parameter))
nscen<- nrow(distinct(ungroup(dfPlotBasin),Scenario))

#nmax<-nrow(dfBaltic)
for(s in 1:nscen){
  for(pa in 1:npar){
  for(b in 1:nbasin){
    for(i in 1:nmax){
      noffset<-(s-1)*npar*nbasin*nmax+(pa-1)*nbasin*nmax+(b-1)*nmax
      
      # 5yr avg
      nfrom<- i-2
      nto<- i+2
      nfrom<-ifelse(nfrom<1,1,nfrom)
      nto<-ifelse(nto>nmax,nmax,nto)
      nfrom=nfrom+noffset
      nto=nto+noffset
      dfPlotBasin$ER_5yr[i+noffset] <- mean(dfPlotBasin$ER[nfrom:nto],na.rm=T)
      if(sum(is.na(dfPlotBasin$ER_obs[nfrom:nto]))<3){
        dfPlotBasin$ER_obs_5yr[i+noffset] <- mean(dfPlotBasin$ER_obs[nfrom:nto],na.rm=T)
      }else{dfPlotBasin$ER_obs_5yr[i+noffset] <- NA}
      
      #10 yr avg
      nfrom<- i-4
      nto<- i+5
      nfrom<-ifelse(nfrom<1,1,nfrom)
      nto<-ifelse(nto>nmax,nmax,nto)
      nfrom=nfrom+noffset
      nto=nto+noffset
      dfPlotBasin$ER_10yr[i+noffset] <- mean(dfPlotBasin$ER[nfrom:nto],na.rm=T)
      if(sum(is.na(dfPlotBasin$ER_obs[nfrom:nto]))<5){
        dfPlotBasin$ER_obs_10yr[i+noffset] <- mean(dfPlotBasin$ER_obs[nfrom:nto],na.rm=T)
      }else{dfPlotBasin$ER_obs_10yr[i+noffset] <- NA}
      
      #30 yr avg
      nfrom<- i-14
      nto<- i+15
      nfrom<-ifelse(nfrom<1,1,nfrom)
      nto<-ifelse(nto>nmax,nmax,nto)
      nfrom=nfrom+noffset
      nto=nto+noffset
      dfPlotBasin$ER_30yr[i+noffset] <- mean(dfPlotBasin$ER[nfrom:nto],na.rm=T)
      if(sum(is.na(dfPlotBasin$ER_obs[nfrom:nto]))<15){
        dfPlotBasin$ER_obs_30yr[i+noffset] <- mean(dfPlotBasin$ER_obs[nfrom:nto],na.rm=T)
      }else{dfPlotBasin$ER_obs_30yr[i+noffset] <- NA}
    }}}}


if(bPrint){saveRDS(dfPlotBasin, file="data/HEAT_Results.rds")}

p3<-ggplot(filter(dfPlotBasin,Parameter=="HEAT")) + 
  theme_minimal() + facet_wrap(~Basin, nrow=2, ncol=5, scales="free",labeller = label_parsed) +
  #geom_point(aes(x=Year,y=ER,colour=Scenario),shape=1, alpha=0.1)  + 
  geom_line(aes(x=Year,y=ER,colour=Scenario, alpha=0.1),show_guide = FALSE) +
  geom_line(aes(x=Year,y=ER_10yr,colour=Scenario)) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  scale_color_brewer(palette="Set1") + 
  labs(y="Eutrophication Ratio", 
       title="HEAT Baltic Basins",subtitle="BALTSEM Scenarios")
p3

figh<-12
figw<-24
fig<-paste0("./figures/Scenarios_basins.png")
if(bPrint){ggsave(p3,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}

for(b in basins2){
  btitle<-paste0("HEAT ",basins[basins2==b])
  dfplot<-dfPlotBasin %>% filter(Basin == b,Parameter=="HEAT")
  p4<-ggplot(dfplot) + 
  theme_minimal() +
  geom_point(aes(x=Year,y=ER,colour=Scenario, alpha=0.1),shape=1,show_guide=F)  + 
  geom_line(aes(x=Year,y=ER_10yr,colour=Scenario)) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  scale_color_brewer(palette="Set1") + 
  labs(y="Eutrophication Ratio", 
       title=btitle,subtitle="BALTSEM Scenarios")
print(p4)

figh<-12
figw<-24
fig<-paste0("./figures/Scenarios_basin_",b,".png")
if(bPrint){ggsave(p4,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
}
#bPrint=T
#----------------------------------------------------
for(b in c("Bornholm~Basin")){
  btitle<-paste0("HEAT ",basins[basins2==b])
  dfplot<-dfPlotBasin %>% filter(Basin == b)
  p5<-ggplot(dfplot) + 
    theme_minimal() +
    geom_point(aes(x=Year,y=ER,colour=Scenario, alpha=0.1),shape=1,show_guide=F)  + 
    geom_line(aes(x=Year,y=ER_10yr,colour=Scenario)) +
    geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
    geom_segment(x=2057,xend=2057,y=0,yend=1,linetype=2,colour="#000000",size=1,alpha=0.3) +
    #geom_segment(x=2072,xend=2072,y=0,yend=1,linetype=2,colour="#000000",size=1,alpha=0.3) +
    geom_text(label="2057",x=2037,y=0.1) +
    #geom_text(label="2072",x=2092,y=0.1) +
    coord_cartesian(ylim=c(0,2.5)) +
    scale_color_brewer(palette="Set1") + 
    labs(y="Eutrophication Ratio", 
         title=btitle,subtitle="BALTSEM Scenarios")
  print(p5)
  
  figh<-12
  figw<-24
  fig<-paste0("./figures/Scenarios_basin_",b,"_1.png")
  if(bPrint){ggsave(p5,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
}
#----------------------------------------------------

#------- obs vs model timeseries by Basin C1, C2, C3 -----------------------------------------

for(scen in c("BSAP")){
  for(b in basins2){
    #for(param in c("C1","C2","C3")){
    #for(param in c("HEAT")){

    dfplot<-dfPlotBasin %>% filter(Basin==b,
                               Scenario==scen,Year %in% c(1900:2020)) #%>%

    dfplot5<-dfplot %>% select(Scenario,Year,Basin,Parameter,model=ER_5yr,obs=ER_obs_5yr) %>%
      gather(key="Param",value="ER_5yr",obs,model)
    dfplot<-dfplot %>% select(Scenario,Year,Basin,Parameter,model=ER,obs=ER_obs) %>%
      gather(key="Param",value="ER",obs,model) %>%
      left_join(dfplot5,by=c("Scenario","Year","Parameter","Basin","Param"))    
    
     if(b %in% basins2[c(4,5,7)]){
       dfplot<-dfplot %>% filter(Parameter %in% c("C1","C2","C3"))
       figh<-12
       figw<-24
     }else{
       dfplot<-dfplot %>% filter(Parameter %in% c("C1","C2"))
      figh<-12
      figw<-17
    }

p6<-ggplot(dfplot) + 
  theme_minimal() +
  facet_wrap(~Parameter, nrow=1, ncol=3, scales="free",labeller=label_parsed) +
  geom_point(aes(x=Year,y=ER,colour=Param),shape=1)  + #
  geom_line(aes(x=Year,y=ER_5yr,colour=Param)) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5))  +
  scale_color_brewer(palette="Set1",name="") + 
  labs(y="Eutrophication Ratio",title=paste0("HEAT ",gsub("~"," ",b))
       ,subtitle="Observations vs. BALTSEM")
print(p6)

fig<-paste0("./figures/obs_vs_model_",scen,"_bas_",b,".png")
if(bPrint){ggsave(p6,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}

  }}


# ------------- recovery -------------------------------------------

recovery<-dfPlotBasin %>% 
  ungroup() %>%
  filter(Year > 2000,ER_10yr<=1) %>%
  group_by(Basin,Scenario) %>% summarise(Recovers=min(Year))

recovery<-dfPlotBasin %>% 
  ungroup() %>%
  group_by(Basin,Scenario) %>% summarise() %>%
  left_join(recovery, by=c("Basin","Scenario")) %>%
  ungroup() %>%
  spread(key=Scenario,value=Recovers) %>% 
  mutate(Basin=gsub("~"," ",Basin))

if(bPrint){saveRDS(recovery,file="results/recovery.rds")}

