# run HEAT350.R first
library(tidyverse)
library(grid)
library(lattice)

dfPlotBasin <- readRDS("data/HEAT_Results.rds")

pal<-c("#FF2121","#FAB70E","#FFFF00","#8FFF07","#2C96F6","#FFFFFF")
#pal<-c("#2C96F6","#8FFF07","#FFFF00","#FAB70E","#FF2121","#FFFFFF")

dfgrid<-dfPlotBasin %>% 
  filter(Parameter=="HEAT") %>%
  select(StnID,Scenario,Year,ER) %>%
  filter(Year>1899) 

bas<-levels(dfgrid$StnID)
bas[bas=="GS"]<-"BP"
dfgrid$StnID <- as.character(dfgrid$StnID)
dfgrid[dfgrid$StnID=="GS","StnID"]<-"BP"
dfgrid$StnID<-factor(dfgrid$StnID,levels=bas)

dfgrid$cat <- cut(dfgrid$ER,
                  breaks = c(0,0.5,1,1.5,2,max(dfgrid$ER,na.rm=T)),
                  labels=c("1","2","3","4","5"))
dfgrid$cat <- factor(as.character(dfgrid$cat),
                             levels=rev(levels(dfgrid$cat)))

dfgrid$Year <- factor(dfgrid$Year)
dfgrid$Year <- factor(dfgrid$Year,levels=rev(levels(dfgrid$Year)))

#dfgrid<-dfgrid %>% filter(Year<9999,Year>1984) 
dfgrid<-dfgrid %>% mutate(x=NA)

bas<-levels(dfgrid$StnID)
sc<-rev(levels(dfgrid$Scenario))
yr<-distinct(dfgrid,Year)
yr<-yr$Year

xmin<-0
dx<-1

for(is in 1:length(sc)){
  for(ib in 1:length(bas)){
      x=xmin+dx*(((is-1)*(0.0+length(bas)))+ib-1)
      cat(paste0("x=",x,"\n"))
      dfgrid[dfgrid$Scenario==sc[is]&dfgrid$StnID==bas[ib],"x"]=x
    }
}


#xlabs<-c(paste0(bas,"_1"),paste0(bas,"_2"),paste0(bas,"_3"),paste0(bas,"_4"))
xlabs<-c(bas,bas,bas,bas)
dfgrid$x <- factor(dfgrid$x)



#define a colour for fonts
textcol <- "grey40"

figure4<-ggplot(dfgrid, aes(x, Year,fill = cat)) +
  geom_tile(show.legend=FALSE) +
  #scale_y_reverse(lim=c(2200,1850)) + 
  scale_y_discrete(expand=c(0,0),
                   breaks=c("2200","2150","2100","2050","2000","1950","1900","1850"))+
                   #breaks=c("2200","2180","2160","2140","2120","2100","2080","2060","2040","2020","2000"))+
  scale_x_discrete(expand=c(0,0),labels=xlabs,position = "top") +
  scale_fill_manual(values=pal,na.value="grey90")+
  labs(x="",y="",title="")+
  theme_grey(base_size=10)+
  theme(
    plot.margin = unit(c(0,2,5,0), "pt"),
    axis.text.x=element_text(size=6,colour=textcol),
    axis.text.y=element_text(vjust = 0.2,colour=textcol),
    axis.ticks.y=element_line(size=0.4,colour=textcol),
    axis.ticks.x=element_blank(),
    plot.background=element_blank(),
    panel.border=element_blank())  +
  geom_vline(aes(xintercept = 9.5),size=0.1,alpha=1) +
  geom_vline(aes(xintercept = 18.5),size=0.1,alpha=1) +
  geom_vline(aes(xintercept = 27.5),size=0.1,alpha=1)
figure4

figw<-16
figh<-22
filefig4<-paste0("./figures_article/figure_4A.png")
#ggsave(figure4,filename=filefig4, width = figw, height = figh, units = "cm", dpi=300)
filefig4<-paste0("./figures_article/figure_4.png")
png(filefig4,width = figw, height = figh, units = "cm",res=300)
figure4

labtext<-sc
labtext<-c("1","2","3","4")
lx<-c(0.19,0.42,0.65,0.88)
ly<-rep(0.97,4)
grid.text(labtext,x=lx,y=ly,rot=0,gp=gpar(fontsize=15), check=TRUE)

dev.off()
