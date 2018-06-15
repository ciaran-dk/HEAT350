
# Save the results from the Biological Reviews article as an R data file

rm(list=ls())

library(dplyr)
library(plyr)
library(tidyr)
library(sqldf)
library(ggplot2)

filename<-"data/bio_reviews_results.txt"
df<-read.table(filename, header=TRUE,sep="\t", stringsAsFactors=FALSE) %>% 
  gather(key="Year", value="ER",X1900:X2012) %>%
  mutate(Year=as.numeric(substr(Year,2,5))) %>%
  spread(key="nYrs",value="ER",sep="")
  #%>% filter(Basin=="Baltic_Proper", Parameter=="HEAT")

for(p in c("HEAT","C1","C2","C3")){
  dfplot<-df %>% filter(Parameter==p)
  p<-ggplot(dfplot) + facet_wrap(~ Basin, nrow=3, ncol=3, scales="free_y") +
    geom_point(aes(x=Year,y=nYrs1)) +
    geom_line(aes(x=Year,y=nYrs5)) +
    ylab(paste0("ER (",p,")")) + 
    xlab("Year") +
    theme_minimal()
  print(p)
}

saveRDS(df, file="data/BioReviews.rds")
