# run HEAT350.R first
library(tidyverse)
library(grid)

dfBasin<-readRDS("data/HEAT_Results_Basin.rds")

# ------- Baltic Averages---------------------------------
nyears<-5
transp<-0.1

dfBalticCat <- dfBasin %>% #
  ungroup() %>% 
  group_by(Parameter,Scenario,Year) %>% 
  summarise(ER=mean(ER,na.rm=T), ER_obs=mean(ER_obs,na.rm=T)) %>%
  mutate(ER_obs=ifelse(is.nan(ER_obs),NA,ER_obs)) %>%
  rename(obs=ER_obs,model=ER) %>%
  gather(key="Type",value="ER",c("model","obs")) %>% 
  arrange(Scenario,Parameter,Type,Year)

nmax<- nrow(distinct(ungroup(dfBalticCat),Year))
nscen<- nrow(distinct(ungroup(dfBalticCat),Scenario))
npar<- nrow(distinct(ungroup(dfBalticCat),Parameter))
ntyp<- nrow(distinct(ungroup(dfBalticCat),Type))

#nmax<-nrow(dfBalticCat)
dfBalticCat$ER_5yr<-NA
dfBalticCat$ER_10yr<-NA

for(s in 1:nscen){
  for(pa in 1:npar){
    for(tp in 1:ntyp){
      for(i in 1:nmax){
    noffset<-(s-1)*npar*ntyp*nmax+(pa-1)*ntyp*nmax+(tp-1)*nmax
    # 5yr avg
    nfrom<- i-2
    nto<- i+2
    nfrom<-ifelse(nfrom<1,1,nfrom)
    nto<-ifelse(nto>nmax,nmax,nto)
    nfrom=nfrom+noffset
    nto=nto+noffset
    if(sum(is.na(dfBalticCat$ER[nfrom:nto]))<3){
      dfBalticCat$ER_5yr[i+noffset] <- mean(dfBalticCat$ER[nfrom:nto],na.rm=T)
    }else{dfBalticCat$ER_5yr[i+noffset] <- NA}

    #10 yr avg
    nfrom<- i-4
    nto<- i+5
    nfrom<-ifelse(nfrom<1,1,nfrom)
    nto<-ifelse(nto>nmax,nmax,nto)
    nfrom=nfrom+noffset
    nto=nto+noffset
    if(sum(is.na(dfBalticCat$ER[nfrom:nto]))<3){
      dfBalticCat$ER_10yr[i+noffset] <- mean(dfBalticCat$ER[nfrom:nto],na.rm=T)
    }else{dfBalticCat$ER_10yr[i+noffset] <- NA}
  }}}
}

dfBalticCat<-dfBalticCat %>% 
  mutate(ER_5yr=ifelse(is.nan(ER_5yr),NA,ER_5yr),
         ER_10yr=ifelse(is.nan(ER_10yr),NA,ER_10yr)
  )

dfplot <- dfBalticCat %>% 
  filter(Type=="model") %>%
  filter(Scenario %in% c("BSAP","BAU30")) %>%
  select(Parameter,Scenario,Year,ER,ER_10yr)



figure6<-ggplot(dfplot) + 
  theme_minimal() + facet_wrap(~Parameter, nrow=4, ncol=1, scales="free",labeller = label_parsed) +
  geom_line(aes(x=Year,y=ER,colour=Scenario, alpha=0.1),show.legend = FALSE) +
  geom_line(aes(x=Year,y=ER_10yr,colour=Scenario),show.legend = FALSE) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  scale_color_brewer(palette="Set1") + 
  labs(y="Eutrophication Ratio") 

figure6

figh<-15
figw<-15

filefig6<-paste0("./figures_article/figure_6A.png")
ggsave(figure6,filename=filefig6, width = figw, height = figh, units = "cm", dpi=300)
filefig6<-paste0("./figures_article/figure_6.png")
png(filefig6,width = figw, height = figh, units = "cm",res=300)
figure6

labtext<-c("a","b","c","d","e","f","g","h","i")
x<-c(0.09,0.41,0.73)
x<-c(x,x,x)
y<-c(0.97,0.97,0.97,0.65,0.65,0.65,0.33,0.33,0.33)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=15), check=TRUE)

dev.off()




