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

for(ip in 1:length(params)){

Unit<-filter(df,Parameter==params[ip])[1,"Unit"]

dfplot<- filter(df,Parameter==params[ip])
if(params[ip]=="O2debt"){
  figh<-5.5
  figw<-16
  
  dfplot<- filter(dfplot,Basin %in% c("Bornholm Basin","Baltic Proper","Gulf of Finland"))

  p<-ggplot(dfplot) + 
    theme_minimal() + facet_wrap(~Basin, nrow=3, ncol=3, scales="free")+ #,labeller = label_parsed) +
    geom_line(aes(x=Year,y=Status,colour=Scenario, alpha=1),show.legend = FALSE) +
    geom_line(aes(x=Year,y=Target),colour="#000000",linetype=3,show.legend = FALSE) +
    #coord_cartesian(ylim=c(0,2.5)) +
    scale_color_brewer(palette="Set1") + 
    labs(y=Unit)
  p
  
  figfile<-paste0("./figures_article/figure_S",ip,"_",params[ip],".png")
  png(figfile,width = figw, height = figh, units = "cm",res=300)
  print(p)
  
  labtext<-c("a","b","c")
  x<-c(0.09,0.41,0.73)
  y<-c(0.92,0.92,0.92)
  grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=15), check=TRUE)
  
  dev.off()    
}else{
  figh<-16
  figw<-16
  
  p<-ggplot(dfplot) + 
    theme_minimal() + facet_wrap(~Basin, nrow=3, ncol=3, scales="free")+ #,labeller = label_parsed) +
    geom_line(aes(x=Year,y=Status,colour=Scenario, alpha=1),show.legend = FALSE) +
    geom_line(aes(x=Year,y=Target),colour="#000000",linetype=3,show.legend = FALSE) +
    #coord_cartesian(ylim=c(0,2.5)) +
    scale_color_brewer(palette="Set1") + 
    labs(y=Unit)
  p

  figfile<-paste0("./figures_article/figure_S",ip,"_",params[ip],".png")
  png(figfile,width = figw, height = figh, units = "cm",res=300)
  print(p)
  
  labtext<-c("a","b","c","d","e","f","g","h","i")
  x<-c(0.09,0.41,0.73)
  x<-c(x,x,x)
  y<-c(0.97,0.97,0.97,0.65,0.65,0.65,0.33,0.33,0.33)
  grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=15), check=TRUE)
  
  dev.off()
  
}#if O2debt

}
