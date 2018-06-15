
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

#Source the HEAT assessment calculation routine
#source('HEAT.R')
#AreaWeights<-TRUE
AreaWeights<-FALSE


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


# --------------- import HEAT file for 1850-2200 --------------
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
dfBasin$Basin <- factor(dfBasin$Basin,levels=basins)

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
  mutate(p=ifelse(p<0.001,"p<0.001",paste0("p=",round(p,3))),r2=(paste0("R2=",round(r2,2))),text=paste0(r2,"\n",p))

dfBasin <- dfBasin %>% left_join(select(models,Scenario,Basin,Parameter,r2),by=c("Scenario","Basin","Parameter")) %>%
  mutate(r2=ifelse(is.na(r2),"",r2)) %>%
  mutate(ER_obs=ifelse(Parameter=="C3" & Basin %in% c("Kattegat","Danish Straits","Arkona Basin","Gulf of Riga","Bothnian Sea","Bothnian Bay"),
                       NA,ER_obs))
         
 

scen<-"BSAP"
#for(scen in c("BAU30","PLC55","BSAP","BSAP30")){
  for(param in c("HEAT","C1","C2","C3")){
    desc<-dfparam %>% filter(Parameter==param)
    desc<-desc[1,2]
    dfplot<-dfBasin %>% filter(Parameter==param,Scenario==scen) #%>% filter(!is.na(ER_obs)) 

    p<-ggplot(dfplot) + facet_wrap(Basin~r2, nrow=2, ncol=5, scales="free") +#labeller = "label_parsed"
      geom_point(aes(x=ER,y=ER_obs),shape=1)  + 
      geom_smooth(aes(x=ER,y=ER_obs),method="lm",formula=y~x) +
      labs(title=paste0(desc," [",scen,"]"),y=paste0("Observed"),x=paste0("Model")) +
      theme_minimal()
    
      #labs(title=paste0(scen," ",param),y=paste0(param, " obs"),x=paste0(param, " model")) +
      #+ coord_cartesian(xlim=c(0,2),ylim=c(0,2))+ geom_text(aes(label=text))
    print(p)
    
    fig<-paste0("./figures/obs_vs_model_",scen,"_",param,".png")
    figh<-15
    figw<-25
    ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)
    
  }
#}


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

  
# mod<-lm(ER~ER_obs,data =dfBasin)
# sum<-summary(mod)
# sum[["adj.r.squared"]]

# for(i in 1:nrow(models)){
#   p<-anova(models$mod[[i]])[1,5]
#   models$p[i]<-p
#   r2<-summary(models$mod[[i]])[["adj.r.squared"]]
#   models$r2[i]<-r2
# }


#sum<-summary(models$mod[[1]])






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
  dfBaltic$model_5yr[i+noffset] <- mean(dfBaltic$model[nfrom:nto],na.rm=T)
  dfBaltic$obs_5yr[i+noffset] <- mean(dfBaltic$obs[nfrom:nto],na.rm=T)
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
  coord_cartesian(ylim=c(0,2.5))  +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1)
p1


dfplot<-dfBaltic %>% filter(Param=="model")
p2<-ggplot(dfplot) + 
  theme_minimal() +
  geom_point(aes(x=Year,y=ER,colour=Scenario),shape=1)  + 
  geom_line(aes(x=Year,y=ER_5yr,colour=Scenario)) +
  coord_cartesian(ylim=c(0,2.5)) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1)
p2

figh<-15
figw<-25
fig<-paste0("./figures/obs_vs_model_Baltic.png")
ggsave(p1,filename=fig, width = figw, height = figh, units = "cm", dpi=300)
fig<-paste0("./figures/Scenarios_Baltic.png")
ggsave(p2,filename=fig, width = figw, height = figh, units = "cm", dpi=300)

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
