# run HEAT350.R first
library(tidyverse)
library(grid)
library(lattice)


dfBaltic<-readRDS("data/HEAT_Results_Baltic.rds")



df<-dfBaltic %>% 
  ungroup() %>%
  filter(Scenario=="BSAP") %>%
  select(Year,Param,ER) %>%
  spread(key="Param",value="ER") %>%
  filter(!is.na(model),!is.na(obs))

mod=lm(model~obs,data=df)

df$r2<-summary(mod)[["adj.r.squared"]]
df$p<-anova(mod)[1,5]

df <- df %>%
  mutate(p=ifelse(p<0.001,"p<0.001",paste0("p=",round(p,3))),r2=(paste0("R^2*'='~'",round(r2,2),"'")),text=paste0(r2,"\n",p))

figh<-8
figw<-12

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
  theme_minimal(base_size=10) #+ 

p


fig<-paste0("./figures_article/figure_Baltic_HEAT_regr.png")
ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)

# 
# 
# p<-ggplot(df,aes(x=obs,y=model))+
#   geom_point(alpha=0.4,colour="#000000",show.legend=F)  + #shape=1, 
#   geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.1,fill="#000000",show.legend=F) +
#   geom_line(stat='smooth',method="lm",alpha=0.4,formula=y~x,colour="#000000",show.legend=F) +
#   labs(y="Model",x="Observed") +
#   coord_cartesian(ylim=c(0.5,2.0),xlim=c(0.5,2.5)) +
#   theme_minimal()
# 
# p
# 
# fig<-paste0("./figures_article/figure_Baltic_HEAT_regr_v2.png")
# ggsave(p,filename=fig, width = figw, height = figh, units = "cm", dpi=300)
# 
# 
