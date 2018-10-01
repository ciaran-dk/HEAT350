# run HEAT350.R first
library(tidyverse)
library(grid)

mypal<-c("#FF0000","#000000","#70AD47","#0070C0")

dfPlotBasin <- readRDS("data/HEAT_Results.rds")

fig<-ggplot(filter(dfPlotBasin,Parameter=="HEAT")) + 
  theme_minimal() + facet_wrap(~Basin, nrow=3, ncol=3, scales="free",labeller = label_parsed) +
  geom_line(aes(x=Year,y=ER,colour=Scenario, alpha=0.1),show.legend = FALSE) +
  geom_line(aes(x=Year,y=ER_10yr,colour=Scenario),show.legend = FALSE) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  #scale_color_brewer(palette="Set1") + 
  scale_color_manual(values=mypal) + 
  labs(y="Eutrophication Ratio")   +
  theme(axis.text=element_text(colour="#000000"))

fig

figh<-15
figw<-15

file<-paste0("./figures_article/figure_4_without_labels.png")
ggsave(fig,filename=file, width = figw, height = figh, units = "cm", dpi=300)
file<-paste0("./figures_article/figure_4.tif")
tiff(file,width = figw, height = figh, units = "cm",res=300)
fig

labtext<-c("a","b","c","d","e","f","g","h","i")
x<-c(0.09,0.41,0.73)
x<-c(x,x,x)
y<-c(0.97,0.97,0.97,0.65,0.65,0.65,0.33,0.33,0.33)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=15), check=TRUE)

dev.off()


