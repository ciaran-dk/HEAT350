library(tidyverse)
library(grid)


filename<-"data/bärbel/loads/loadsummary_Final_BSAP.csv"
df1<-read.table(filename, header=TRUE,sep=",", stringsAsFactors=FALSE)
filename<-"data/bärbel/loads/loadsummary_Final_PLC55.csv"
df2<-read.table(filename, header=TRUE,sep=",", stringsAsFactors=FALSE)
filename<-"data/bärbel/loads/loadsummary_PLCtoBAU30years.csv"
df3<-read.table(filename, header=TRUE,sep=",", stringsAsFactors=FALSE)
filename<-"data/bärbel/loads/loadsummary_PLCtoBSAP30years.csv"
df4<-read.table(filename, header=TRUE,sep=",", stringsAsFactors=FALSE)
filename<-"data/bärbel/loads/loadsummary_Hindcast.csv"
df5<-read.table(filename, header=TRUE,sep=",", stringsAsFactors=FALSE)

#PLC6
filename<-"data/plc6_tn_tp.txt"
df6<-read.table(filename, header=TRUE,sep="\t", stringsAsFactors=FALSE) 



df1<-df1 %>% 
  select(Year,Ntot,Ptot) %>%
  mutate(Scenario="BSAP")
df2<-df2 %>% 
  select(Year,Ntot,Ptot) %>%
  mutate(Scenario="PLC55")
df3<-df3 %>% 
  select(Year,Ntot,Ptot) %>%
  mutate(Scenario="BAU30")
df4<-df4 %>% 
  select(Year,Ntot,Ptot) %>%
  mutate(Scenario="BSAP30")

df6<-df6 %>% 
  select(Year,Ntot,Ptot) %>%
  mutate(Scenario="PLC6")


df<-bind_rows(df1,df2,df4,df6) %>%
  gather(key="Param",value="Load",c("Ntot","Ptot")) 

df$Load<-df$Load/1000
df$Param<-factor(df$Param,levels=c("Ntot","Ptot"),labels=c("N~Total","P~Total"))

df$Scenario<-factor(df$Scenario,levels=c("PLC6","PLC55","BSAP30","BSAP"))

df<-df %>% filter(Year>2004,Year<2020)

fig<-ggplot(df) + 
  theme_minimal(base_size=7) + facet_wrap(~Param, nrow=2, ncol=1, scales="free", labeller = label_parsed) +
  geom_line(aes(x=Year,y=Load,colour=Scenario),show.legend = FALSE) +
  #coord_cartesian(ylim=c(0,2.5)) +
  scale_color_brewer(palette="Set1") + 
  labs(y="Load [000's tons]") +
  theme(axis.text=element_text(colour="#000000"))

fig

# averages 2008-2015
df %>% filter(Year>2007,Year<2016) %>%
  group_by(Param,Scenario) %>%
  summarise(Load_avg=mean(Load,na.rm=F))

# regressions
df<-bind_rows(df1,df2,df4) %>%
  gather(key="Param",value="Load",c("Ntot","Ptot")) 
df$Load<-df$Load/1000
df$Param<-factor(df$Param,levels=c("Ntot","Ptot"),labels=c("N~Total","P~Total"))

dfobs <- df6 %>%
  select(-Scenario) %>% 
  gather(key="Param",value="Load_obs",c("Ntot","Ptot")) 
dfobs$Load_obs<-dfobs$Load_obs/1000
dfobs$Param<-factor(dfobs$Param,levels=c("Ntot","Ptot"),labels=c("N~Total","P~Total"))

df<-df %>% 
  left_join(dfobs,by=c("Year","Param")) %>%
  filter(Year>2007,Year<2016)


df <- df %>% mutate(err=Load_obs-Load,err2 = err^2) 

dferr <- df %>% filter(Year>2007,Year<2016) %>%
  group_by(Param,Scenario) %>%
  summarise(sum_err2=sum(err2,na.rm=F),err=mean(err,na.rm=F)) %>% 
  mutate(RMSE=round(sqrt(sum_err2),1), err=round(err,1))



models<-df %>%
  group_by(Scenario,Param) %>% 
  do(mod=lm(Load~Load_obs,data = .))

for(i in 1:nrow(models)){
  p<-anova(models$mod[[i]])[1,5]
  models$p[i]<-p
  r2<-summary(models$mod[[i]])[["adj.r.squared"]]
  models$r2[i]<-r2
}

models <- models %>% select(Scenario,Param,p,r2) %>%
  arrange(Param,desc(r2))
