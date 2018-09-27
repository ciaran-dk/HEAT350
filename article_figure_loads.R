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

df<-bind_rows(df1,df2,df3,df4) %>%
  gather(key="Param",value="Load",c("Ntot","Ptot")) 

df$Load<-df$Load/1000
df$Param<-factor(df$Param,levels=c("Ntot","Ptot"),labels=c("N~Total","P~Total"))

df$Scenario<-factor(df$Scenario,levels=c("BAU30","PLC55","BSAP30","BSAP"))
#, nrow=2, ncol=1 
fig<-ggplot(df) + 
  theme_minimal(base_size=6) + facet_wrap(~Param,ncol=1,labeller=label_parsed,scales="free_y") +
  geom_line(aes(x=Year,y=Load,colour=Scenario),show.legend = FALSE) +
  #coord_cartesian(ylim=c(0,2.5)) +
  scale_color_brewer(palette="Set1") + 
  labs(y="Load [000's tons]") +
  theme(axis.text=element_text(colour="#000000"),
        strip.text = element_blank(),
        #strip.text.y = element_text(size=rel(1.),vjust = 0.0, ),
        panel.spacing.y = unit(0, "lines"))
fig

figh<-5
figw<-6.8

file<-paste0("./figures_article/figure_loads.png")
png(file,width = figw, height = figh, units = "cm",res=300)
fig

labtext<-c("a","b")
 x<-c(0.16,0.16)
 y<-c(0.95,0.51)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=15), check=TRUE)
labtext<-c("TN","TP")
x<-c(0.27,0.27)
y<-c(0.94,0.50)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=12), check=TRUE)

dev.off()


figh<-5
figw<-8
file<-paste0("./figures_article/figure_loads_v2.png")
png(file,width = figw, height = figh, units = "cm",res=300)
fig

labtext<-c("a","b")
x<-c(0.16,0.16)
y<-c(0.95,0.51)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=15), check=TRUE)
labtext<-c("N Total","P Total")
x<-c(0.19,0.19)
y<-c(0.75,0.31)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=7), check=TRUE)

dev.off()

