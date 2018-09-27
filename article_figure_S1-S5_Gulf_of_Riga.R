# run HEAT350.R first
library(tidyverse)
library(grid)


df<-readRDS(file="data/Parameter_ER_Basin.rds")
Basins<-c("Kattegat","Danish Straits","Arkona Basin","Bornholm Basin","Baltic Proper",
          "Gulf of Riga","Gulf of Finland","Bothnian Sea","Bothnian Bay")    

df$Basin<-factor(df$Basin,levels=Basins)

nyears<-5
transp<-0.1

params<-c("DIN","PO4","Chla","Secchi","O2debt")
parlong<-c(Winter~DIN,Winter~DIP,Summer~Chl~italic(a),
           Summer~Secchi~Depth,Oxygen~Debt)

#for(ip in 1:length(params)){

Unit<-filter(df,Parameter==params[ip])[1,"Unit"]

dfplot<- df %>% 
  filter(Basin=="Gulf of Riga") %>% 
  mutate(Variable=paste0(Variable," [",Unit,"]"))


  figh<-12
  figw<-16
  
  p<-ggplot(dfplot) + 
    theme_minimal() + facet_wrap(~Variable, nrow=3, ncol=3, scales="free")+ #,labeller = label_parsed) +
    geom_line(aes(x=Year,y=Status,colour=Scenario),show.legend = TRUE) +
    geom_line(aes(x=Year,y=Target),colour="#000000",linetype=3,show.legend = FALSE) +
    #coord_cartesian(ylim=c(0,2.5)) +
    scale_color_brewer(palette="Set1") + 
    labs(y="",x="") + 
    theme(legend.position = c(.82,.15))
  p

  fig<-paste0("./figures_article/figure_Gulf_of_Riga.png")
  ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)
  



