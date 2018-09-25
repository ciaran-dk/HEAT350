# run HEAT350.R first
library(tidyverse)
library(grid)


#pal<-c("#FF2121","#FAB70E","#FFFF00","#8FFF07","#2C96F6","#FFFFFF")

pal<-c("#FF2121","#8FFF07","#2C96F6","#FFFFFF")
pal<-c("#FF0000","#00FF00","#2C96F6","#FFFFFF")

titlefontsize<-6
basefontsize<-7



#------------------------ N & P loads ---------------------------------------------

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
df3<-df3 %>% 
  select(Year,Ntot,Ptot) %>%
  mutate(Scenario="BAU30")

dfload<-bind_rows(df1,df3) %>%
  gather(key="Param",value="Load",c("Ntot","Ptot")) 

dfload$Load<-dfload$Load/1000
#dfload$Param<-factor(dfload$Param,levels=c("Ntot","Ptot"),labels=c("N~Total","P~Total"))
#dfload$Scenario<-factor(dfload$Scenario,levels=c("BAU30","BSAP"))

dfload<- dfload %>%
  rename(Parameter=Param,Value=Load)

# ------- Baltic Averages for HEAT score ---------------------------------

dfBaltic<-readRDS("data/HEAT_Results_Baltic.rds")

dfplot<-dfBaltic %>% filter(Param=="model",Scenario %in% c("BSAP","BAU30"))

textcol <- "grey40"
p6i<-ggplot(dfplot) + 
  theme_minimal(base_size=basefontsize) +
  geom_line(aes(x=Year,y=ER,colour=Scenario, alpha=0.1),show.legend = FALSE) +
  geom_line(aes(x=Year,y=ER_10yr,colour=Scenario),show.legend = FALSE) +
  geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
  coord_cartesian(ylim=c(0,2.5)) +
  scale_color_manual(values=pal) + 
  labs(y="ER",x="") +
  ggtitle("HEAT")+
  theme(
    axis.ticks=element_line(size=0.4,colour=textcol),
    axis.line=element_line(size=0.4,colour=textcol),
    axis.text=element_text(colour="#000000"),
    title=element_text(size=titlefontsize),
    plot.margin = unit(c(0.05,0.05,0.05,0.05), "cm"))
p6i


# ------- Baltic Averages for parameters ---------------------------------

df<-readRDS(file="data/Parameter_ER_Basin.rds")

nyears<-5
transp<-0.1

dfParam <- df %>% #
  ungroup() %>% 
  group_by(Parameter,Scenario,Year) %>% 
  summarise(ER=mean(EUT_Ratio,na.rm=T)) %>%
  mutate(ER=ifelse(is.nan(ER),NA,ER)) %>%
  arrange(Scenario,Parameter,Year) %>% 
  ungroup()

nmax<- nrow(distinct(ungroup(dfParam),Year))
nscen<- nrow(distinct(ungroup(dfParam),Scenario))
npar<- nrow(distinct(ungroup(dfParam),Parameter))
ntyp<- 1 #nrow(distinct(ungroup(dfParam),Type))

#nmax<-nrow(dfParam)
dfParam$ER_5yr<-NA
dfParam$ER_10yr<-NA

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
    if(sum(is.na(dfParam$ER[nfrom:nto]))<3){
      dfParam$ER_5yr[i+noffset] <- mean(dfParam$ER[nfrom:nto],na.rm=T)
    }else{dfParam$ER_5yr[i+noffset] <- NA}

    #10 yr avg
    nfrom<- i-4
    nto<- i+5
    nfrom<-ifelse(nfrom<1,1,nfrom)
    nto<-ifelse(nto>nmax,nmax,nto)
    nfrom=nfrom+noffset
    nto=nto+noffset
    if(sum(is.na(dfParam$ER[nfrom:nto]))<3){
      dfParam$ER_10yr[i+noffset] <- mean(dfParam$ER[nfrom:nto],na.rm=T)
    }else{dfParam$ER_10yr[i+noffset] <- NA}
  }}}
}

dfParam<-dfParam %>% 
  mutate(ER_5yr=ifelse(is.nan(ER_5yr),NA,ER_5yr),
         ER_10yr=ifelse(is.nan(ER_10yr),NA,ER_10yr)
  )

dfplot <- dfParam %>% 
  filter(Scenario %in% c("BSAP","BAU30")) %>%
  select(Parameter,Scenario,Year,ER,ER_10yr) %>%
  rename(Value=ER,Value10yr=ER_10yr) %>% 
  mutate(Unit="ER") #%>%
  
source('UN population.R') 

dfpop <- dfpop %>% 
  mutate(Parameter="Pop",Scenario="BSAP",Unit="millions") %>%
  rename(Value=Population)

dfplot <- dfplot %>% bind_rows(dfpop)

parlevels<-c("Pop","DIN","PO4","Chla","Secchi","O2debt")
dfplot$Parameter <- factor(dfplot$Parameter,levels=parlevels)
parlong<-c("Population","Winter~DIN","Winter~DIP","Summer~Chl~italic(a)","Summer~Secchi~Depth","Oxygen~Debt")
parlevels<-c("Pop","DIN","PO4","Chla","Secchi","O2debt")
par<-data.frame(Parameter=factor(parlevels,levels=parlevels),
                ParLong=factor(parlong,levels=parlong))
dfplot<-dfplot %>% left_join(par,by="Parameter")


myplot<-function(df,par,mytitle,ylabel="ER",ylimits=c(0,2.5),textcol="grey40"){
  cat(paste0(mytitle,"\n"))
  p<-ggplot(filter(df,Parameter==par)) + 
    theme_minimal(base_size=basefontsize) +
    geom_line(aes(x=Year,y=Value,colour=Scenario, alpha=0.1),show.legend = FALSE) +
    geom_line(aes(x=Year,y=Value10yr,colour=Scenario),show.legend = FALSE) +
    geom_hline(yintercept=1,linetype=3,colour="#000000",size=1) +
    coord_cartesian(ylim=ylimits,xlim=c(1850,2200)) +
    scale_color_manual(values=pal) + 
    labs(y=ylabel,x="")+
    theme(
      axis.ticks=element_line(size=0.4,colour=textcol),
      axis.line=element_line(size=0.4,colour=textcol),
      axis.text=element_text(colour="#000000"),
      title=element_text(size=titlefontsize),
      plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
     ggtitle(eval(bquote(.(mytitle))))
    #ggtitle(ParLong)+
  return(p)
}
myplotpop<-function(df,par,mytitle,ylabel="millions",ylimits=c(0,80),textcol="grey40"){
  p<-ggplot(filter(df,Parameter==par)) + 
    theme_minimal(base_size=basefontsize) +
    geom_line(aes(x=Year,y=Value,alpha=1),colour="#000000" ,show.legend = FALSE) +
    coord_cartesian(ylim=ylimits,xlim=c(1850,2200)) +
    labs(y=ylabel,x="")+
    theme(
      axis.ticks=element_line(size=0.4,colour=textcol),
      axis.line=element_line(size=0.4,colour=textcol),
      axis.text=element_text(colour="#000000"),
      title=element_text(size=titlefontsize),
      plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
    ggtitle(eval(bquote(.(mytitle))))
  #ggtitle(ParLong)+
  return(p)
}
myplotload<-function(df,par,mytitle,ylabel="[000's tons]",ylimits=c(0,1600),textcol="grey40"){
  cat(paste0(mytitle,"\n"))
  p<-ggplot(filter(df,Parameter==par)) + 
    theme_minimal(base_size=basefontsize) +
    geom_line(aes(x=Year,y=Value,colour=Scenario),show.legend = FALSE) +
    coord_cartesian(ylim=ylimits,xlim=c(1850,2200)) +
    scale_color_manual(values=pal) + 
    labs(y=ylabel,x="")+
    theme(
      axis.ticks=element_line(size=0.4,colour=textcol),
      axis.line=element_line(size=0.4,colour=textcol),
      axis.text=element_text(colour="#000000"),
      title=element_text(size=titlefontsize),
      plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))+
    ggtitle(eval(bquote(.(mytitle))))
  return(p)
}



p6a<-myplotpop(dfplot,"Pop","Population")
p6b<-myplotload(dfload,"Ntot","N Load")
p6c<-myplotload(dfload,"Ptot","P Load",ylimits=c(0,80))
p6d<-myplot(dfplot,"DIN",Winter~DIN~Eutrophication~Ratio)
p6e<-myplot(dfplot,"PO4",Winter~DIP~Eutrophication~Ratio)
p6f<-myplot(dfplot,"Chla",Summer~Chl~italic(a)~Eutrophication~Ratio)
p6g<-myplot(dfplot,"Secchi",Summer~Secchi~Depth~Eutrophication~Ratio)
p6h<-myplot(dfplot,"O2debt",Oxygen~Debt~Eutrophication~Ratio)

figh<-21
figw<-15
filefig6<-paste0("./figures_article/figure_6.png")

if(F){
png(filefig6,width = figw, height = figh, units = "cm",res=300)

grid.newpage()
pushViewport(viewport(layout=grid.layout(37,2)))
print(p6a,vp=viewport(layout.pos.row=1:9, layout.pos.col=1))
print(p6b,vp=viewport(layout.pos.row=1:9, layout.pos.col=2))
print(p6c,vp=viewport(layout.pos.row=10:18, layout.pos.col=1))
print(p6d,vp=viewport(layout.pos.row=10:18, layout.pos.col=2))
print(p6e,vp=viewport(layout.pos.row=19:27, layout.pos.col=1))
print(p6f,vp=viewport(layout.pos.row=19:27, layout.pos.col=2))
print(p6g,vp=viewport(layout.pos.row=28:36, layout.pos.col=1))


labtext<-c("a","b","c","d","e","f","g")
x<-c(0.03,0.53,0.03,0.53,0.03,0.53,0.03)
y<-c(0.99,0.99,0.75,0.75,0.505,0.505,0.265)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=15), check=TRUE)
grid.text("Year",x=0.5,y=0.02,rot=0,gp=gpar(fontsize=12), check=TRUE)
dev.off()
}

figh<-21
figw<-8
filefig6<-paste0("./figures_article/figure_6B.png")
png(filefig6,width = figw, height = figh, units = "cm",res=300)

grid.newpage()
pushViewport(viewport(layout=grid.layout(45,1)))
print(p6a,vp=viewport(layout.pos.row=1:5, layout.pos.col=1))
print(p6b,vp=viewport(layout.pos.row=6:10, layout.pos.col=1))
print(p6c,vp=viewport(layout.pos.row=11:15, layout.pos.col=1))
print(p6d,vp=viewport(layout.pos.row=16:20, layout.pos.col=1))
print(p6e,vp=viewport(layout.pos.row=21:25, layout.pos.col=1))
print(p6f,vp=viewport(layout.pos.row=26:30, layout.pos.col=1))
print(p6g,vp=viewport(layout.pos.row=31:35, layout.pos.col=1))
print(p6h,vp=viewport(layout.pos.row=36:40, layout.pos.col=1))
print(p6i,vp=viewport(layout.pos.row=41:45, layout.pos.col=1))

x<-rep(0.05,7)
labtext<-c("a","b","c","d","e","f","g","h","i")
#y<-c(0.990,0.848,0.705,0.563,0.420,0.278,0.135)
y<-c(0.99,0.88,0.77,0.66,0.55,0.44,0.33,0.22,0.11)
#y<-c(0.99,0.852,0.714,0.576,0.438,0.3,0.162)
grid.text(labtext,x=x,y=y,rot=0,gp=gpar(fontsize=14), check=TRUE)
#grid.text("Year",x=0.5,y=0.02,rot=0,gp=gpar(fontsize=12), check=TRUE)
dev.off()



