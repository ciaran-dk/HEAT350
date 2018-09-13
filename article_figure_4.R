# run HEAT350.R first
library(tidyverse)
library(grid)

dfPlotBasin <- readRDS("data/HEAT_Results.rds")

EutCat<-function(n){
  cat<-ifelse(n<0.5,1,
              ifelse(n<1,2,
                     ifelse(n<1.5,3,
                            ifelse(n<2,4,5))))
  return(cat)
}

#pal<-c("#FF2121","#FAB70E","#FFFF00","#8FFF07","#2C96F6")
pal<-c("#2C96F6","#8FFF07","#FFFF00","#FAB70E","#FF2121","#FFFFFF")

dfgrid<-dfPlotBasin %>% 
  filter(Parameter=="HEAT") %>%
  select(StnID,Scenario,Year,ER) %>%
  mutate(cat=as.integer(EutCat(ER))) %>%
  mutate(col=pal[cat])
  

dfgrid<-filter(Year<2011,Year>1980)
  
bas<-levels(dfgrid$StnID)
sc<-levels(dfgrid$Scenario)
yr<-distinct(dfgrid,Year)
yr<-yr$Year

ymin<-0.05
ymax<-0.95
xmin<-0.05
xmax<-0.95
nx<-length(bas)*length(sc)
ny<-length(yr)
dy<-(ymax-ymin)/ny
dx<-(xmax-xmin)/nx




grid.newpage()
vp <- viewport(width=0.9, height=0.9) #x=0.05, y=0.05, 

#for(is in 1:1){

for(is in 1:length(sc)){
  for(ib in 1:length(bas)){
    for(iy in 1:length(yr)){
      x=xmin+dx*(((is-1)*length(bas))+ib-1)
      y=ymin+dy*(length(yr)-iy)
      col<-dfgrid %>%
        filter(Scenario==sc[is],StnID==bas[ib],Year==yr[iy])
      col <- col$col
      cat(paste0("x=",x,",y=",y,",col=",col,"\n"))
      vp <- viewport(x=x, y=y, width=dx, height=dy)
      pushViewport(vp)
      grid.rect(gp=gpar(col=NA, fill=col))
      upViewport()
    }
  }
}







grid.newpage()
vp <- viewport(x=0.05, y=0.05, width=0.9, height=0.9)
pushViewport(vp)
grid.rect(gp=gpar(lty="dashed"))
vp2 <- viewport(x=0.05, y=0.05, width=0.9, height=0.9)
pushViewport(vp2)
grid.rect()
lay <- grid.layout(ncol=nx,nrow=ny)
vplay <- viewport(layout=lay)
pushViewport(vplay)
grid.rect()

pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))



grid.newpage()
tree <- vpTree(viewport(width=0.9, height=0.9, name="main"),
               vpList(vplay))
pushViewport(tree)


nf <- layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE), respect = TRUE)
layout.show(nf)

layout.show(lay)









vp <- viewport(width=.5, height=.5)
pushViewport(vp)
grid.rect(gp=gpar(col=NA, fill="grey80"))