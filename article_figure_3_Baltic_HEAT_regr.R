# run HEAT350.R first
library(tidyverse)
library(grid)
library(lattice)
library(RColorBrewer)
library(graphics)

dfBaltic<-readRDS("data/HEAT_Results_Baltic.rds")

scen<-"BSAP"
scen<-"BSAP30"

df<-dfBaltic %>% 
  ungroup() %>%
  filter(Scenario==scen) %>%
  select(Year,Param,ER) %>%
  spread(key="Param",value="ER") %>%
  filter(!is.na(model),!is.na(obs))

mod=lm(model~obs,data=df)

df$r2<-summary(mod)[["adj.r.squared"]]
df$p<-anova(mod)[1,5]

df <- df %>%
  mutate(p=ifelse(p<0.001,"p<0.001",paste0("p=",round(p,3))),r2=(paste0("R^2*'='~'",round(r2,2),"'")),text=paste0(r2,"\n",p))

figh<-5
figw<-8
basefontsize<-7
figh<-8
figw<-12
basefontsize<-8

#notetext<-df$text[1]

textcol <- "grey40"

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1900, 2020))

p<-ggplot(df,aes(x=obs,y=model))+ sc+
  geom_point(shape=21,alpha=1,aes(fill=Year),colour="grey40",show.legend=T,size=1.5)  + #shape=1, 
  geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.1,fill="#000000",show.legend=F) +
  geom_line(stat='smooth',method="lm",alpha=0.4,formula=y~x,colour="#000000",show.legend=F) + #,linetype=2,size=1
  labs(y="Model",x="Observed") +
  coord_cartesian(ylim=c(0.5,2.0),xlim=c(0.5,2.5)) +
  theme_minimal(base_size=basefontsize)  +
  theme(axis.text=element_text(colour="#000000"))

p


fig<-paste0("./figures_article/figure_Baltic_HEAT_regr.png")
#ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)

str<-paste0("Scenario:",scen,"\n","y = ",round(mod[["coefficients"]][["obs"]],3),"*x + ",round(mod[["coefficients"]][["(Intercept)"]],3),"\n",
            "R^2 = ",round(summary(mod)[["adj.r.squared"]],3),"\n",
            "p = ",round(anova(mod)[1,5],4),"\n")

cat(str)



# ------ plotting with annotation in figure -----------------------


str1<-substitute(y == A*x + B, list(A = round(mod[["coefficients"]][["obs"]],3), B=round(mod[["coefficients"]][["(Intercept)"]],3)))
str2<-substitute(R^2 == A, list(A = round(summary(mod)[["adj.r.squared"]],3)))
str3<-substitute(p < 0.001)


fig<-paste0("./figures_article/figure_3.tif")

tiff(fig,width = figw, height = figh, units = "cm",res=300)
p


x1<-c(0.15)
y1<-c(0.9)
x2<-c(0.147)
y2<-c(0.84)
x3<-c(0.15)
y3<-c(0.76)

grid.text(just="left",str1,x=x1,y=y1,rot=0,gp=gpar(fontsize=10), check=TRUE)
grid.text(just="left",str2,x=x2,y=y2,rot=0,gp=gpar(fontsize=10), check=TRUE)
grid.text(just="left",str3,x=x3,y=y3,rot=0,gp=gpar(fontsize=10), check=TRUE)

dev.off()