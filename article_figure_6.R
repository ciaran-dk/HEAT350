# run HEAT350.R first
library(tidyverse)
library(grid)
library(lattice)

mypal<-c("#FF0000","#000000","#70AD47","#0070C0")

dfBaltic<-readRDS("data/HEAT_Results_Baltic.rds")

dfplot<-dfBaltic %>% filter(Param=="model")

textcol <- "grey40"
p2<-ggplot(dfplot) + 
  theme_minimal(base_size=9) +
  #geom_point(aes(x=Year,y=ER,colour=Scenario),shape=1,show.legend = FALSE)  + 
  geom_line(aes(x=Year,y=ER,colour=Scenario,alpha=0.1),show.legend = FALSE) +
  geom_line(aes(x=Year,y=ER_10yr,colour=Scenario),show.legend = FALSE) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  #scale_color_brewer(palette="Set1") + 
  scale_color_manual(values=mypal) + 
  labs(y="Eutrophication Ratio", 
       title="") +
  theme(
    axis.ticks=element_line(size=0.4,colour=textcol),
    axis.line=element_line(size=0.4,colour=textcol),
    axis.text=element_text(colour="#000000"))
p2


figh<-8
figw<-8

fig<-paste0("./figures_article/figure_6.png")
#ggsave(p2,filename=fig, width = figw, height = figh, units = "cm", dpi=300)


Year<-c(2220,2247)
ER0<-c(0,0)
ER05<-c(0.5,0.5)
ER10<-c(1,1)
ER15<-c(1.5,1.5)
ER20<-c(2,2)
ERmax<-c(2.5,2.5)
df<-data.frame(Year,ER0,ER05,ER10,ER15,ER20,ERmax)

catlab<-c("High","Good","Moderate","Poor","Bad")
catx<-2233
catx<-c(catx,catx,catx,catx,catx)
caty<-c(0.25,0.75,1.25,1.75,2.25)

dfplot2<-bind_rows(dfplot,df)

p2<-ggplot(dfplot2) + 
  theme_minimal(base_size=9) +
  #geom_point(aes(x=Year,y=ER,colour=Scenario),shape=1,show.legend = FALSE)  + 
  geom_line(aes(x=Year,y=ER,colour=Scenario,alpha=0.1),show.legend = FALSE) +
  geom_line(aes(x=Year,y=ER_10yr,colour=Scenario),show.legend = FALSE) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  #geom_hline(yintercept=0.5,linetype=3,colour="#000000",size=1) +
  #geom_hline(yintercept=1.5,linetype=3,colour="#000000",size=1) +
  #geom_hline(yintercept=2,linetype=3,colour="#000000",size=1) +
  scale_x_continuous(breaks=seq(1900, 2200, by=100)) +
  geom_ribbon(aes(ymin=ER0,ymax=ER05,x=Year),fill="#007eff",alpha="1")+
  geom_ribbon(aes(ymin=ER05,ymax=ER10,x=Year),fill="#00d600",alpha="1")+
  geom_ribbon(aes(ymin=ER10,ymax=ER15,x=Year),fill="#ffff00",alpha="1")+
  geom_ribbon(aes(ymin=ER15,ymax=ER20,x=Year),fill="#ff8c2b",alpha="1")+
  geom_ribbon(aes(ymin=ER20,ymax=ERmax,x=Year),fill="#ff0000",alpha="1")+
  #scale_color_brewer(palette="Set1") + 
  annotate(geom="text",x=catx,y=caty,label=catlab,angle = 90, size=2.2) + 
  scale_color_manual(values=mypal) + 
  labs(y="Eutrophication Ratio", 
       title="") +
  theme(
    axis.ticks=element_line(size=0.4,colour=textcol),
    axis.line=element_line(size=0.4,colour=textcol),
    axis.text=element_text(colour="#000000")) +
  coord_cartesian(xlim=c(1865,2230),ylim=c(0,2.5)) 
p2


figh<-8
figw<-8

fig<-paste0("./figures_article/figure_6.png")
ggsave(p2,filename=fig, width = figw, height = figh, units = "cm", dpi=300)



