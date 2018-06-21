
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

#source("compare_BALTSEM_vs_obs_preprocess.R")

bPrint<-F

df<-readRDS(file="results/model_vs_obs_variables.rds")

scen<-"BSAP"
df<-df %>% filter(Scenario==scen) %>% select(-Scenario)

basins<-c("Kattegat","Danish Straits","Arkona Basin","Bornholm Basin","Baltic Proper","Gulf of Riga","Gulf of Finland","Bothnian Sea","Bothnian Bay")
basins2<-gsub(" ","~",basins)
basins3<-c("Bornholm~Basin","Baltic~Proper","Gulf~of~Finland")
vars<-c("din_winter","dip_winter","chl_summer","secchi_summer","O2debt")
vars2<-c("Winter DIN","Winter DIP","Summer Chl a","Summer Secchi","O2 debt")

# --------------------- Time series plots --------------------- 

for(v in vars2){
  dfplot<-df %>% filter(Variable == v,Year<2020,Year>1900)
  desc<-paste0(dfplot[1,"Variable"]," [",dfplot[1,"Unit"],"]")
  ylab<-paste0(v," [",dfplot[1,"Unit"],"]")
  p<-ggplot(dfplot) + 
    theme_minimal() + 
    facet_wrap(~Basin, nrow=2, ncol=5, scales="free",labeller = label_parsed) +
    geom_point(aes(x=Year,y=Value,colour=Param, alpha=0.1),shape=1,show_guide=F)  + 
    geom_line(aes(x=Year,y=Value_5yr,colour=Param)) +
    geom_line(aes(x=Year,y=Target),linetype=2,colour="#ff0000",size=1,alpha=1) +
    scale_color_brewer(palette="Dark2",name="") + 
    labs(y=ylab,title=desc,subtitle=paste0("Timeseries BALTSEM [",scen,"] vs. observations")) +
    theme(legend.position = c(0.9, 0.1))
    #scale_color_brewer(palette="Set1")
    #labs(y="Eutrophication Ratio", title=btitle,subtitle="BALTSEM Scenarios")
  print(p)
  
  figh<-12
  figw<-24
  fig<-paste0("./figures/compare_model_obs_",vars[vars2==v],".png")
  if(bPrint){ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
}


# --------------------- Regression plots --------------------- 
df<-readRDS(file="results/model_vs_obs_variables_regressions.rds") %>% 
  filter(Year>1900,Year<2020) %>%
  filter(!is.na(Obs),!is.na(Model))

for(scen in c("BSAP")){
  for(v in vars2){

    dfplot<-df %>% filter(Variable == v,Scenario==scen) %>%
      mutate(label=paste0(Basin,"~(",r2,")"))
    
    desc<-paste0(dfplot[1,"Variable"]," [",dfplot[1,"Unit"],"]")

    order <- dfplot %>% group_by(Basin,label) %>% summarise() %>% arrange(Basin)
    dfplot$label<-factor(dfplot$label, levels=order$label)
    
     if(v=="O2 debt"){
       dfplot<-dfplot %>% filter(Basin %in% basins3)
       figh<-7
       figw<-15
     }else{
       figh<-12
       figw<-24
     }
    
    p<-ggplot(dfplot,aes(x=Obs,y=Model)) + facet_wrap(~label, nrow=2, ncol=5, scales="free",labeller = label_parsed) +
      geom_point(shape=1, alpha=0.3,colour="#000000")  + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.1,fill="#0000ff") +
      geom_line(stat='smooth',method="lm",alpha=0.6,formula=y~x,colour="#0000ff") +
      labs(title=desc,
           subtitle=paste0("Regression of BALTSEM [",scen,"] on observations"),
           y="Model",
           x="Observed") +
      theme_minimal() + theme(strip.text.x = element_text(size=8)) +
      theme(legend.position = c(0.9, 0.1)) 
    
    print(p)
    
    fig<-paste0("./figures/compare_model_obs_linreg_",vars[vars2==v],".png")
    if(bPrint){ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)}
    
  }
}

df<-readRDS(file="results/model_vs_obs_variables_regressions.rds") %>% 
  select(StnID,Basin,Year,Variable,Unit,Target,Model,Obs) %>%
  mutate(OK=ifelse(Variable=="O2 debt" & !Basin %in% basins3 ,0,1)) %>%
  mutate(Basin=gsub("~"," ",Basin)) %>%
  arrange(Variable,Basin,Year) %>%
  filter(OK==1) %>%
  select(-OK)

write.table(df,file=paste0("results/HEAT_vs_BALTSEM.csv"), row.names=FALSE,quote=FALSE,sep=';', na="")

  

#ggplot(df,aes(x=Year,y=Value)) +
  