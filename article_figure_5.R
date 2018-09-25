# run HEAT350.R first
library(tidyverse)
library(grid)
library(lattice)


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
  scale_color_brewer(palette="Set1") + 
  labs(y="Eutrophication Ratio", 
       title="") +
  theme(
    axis.ticks=element_line(size=0.4,colour=textcol),
    axis.line=element_line(size=0.4,colour=textcol),
    axis.text=element_text(colour="#000000"))
p2


figh<-8
figw<-8

fig<-paste0("./figures_article/figure_5.png")
ggsave(p2,filename=fig, width = figw, height = figh, units = "cm", dpi=300)
