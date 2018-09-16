

# United Nations, Department of Economic and Social Affairs, Population Division (2017). World Population Prospects: The 2017 Revision, DVD Edition.        
# Predictions from 2015: Medium fertility variant, 2015 - 2100
# Note (16) Finland population is Including Åland Islands.
# Total population, both sexes combined, as of 1 July (thousands)
filename<-"data/UN population estimates.txt"
dfpop<-read.table(filename, header=TRUE,sep="\t", stringsAsFactors=FALSE)

dfpop <- dfpop %>% 
  select("Year","Denmark","Estonia","Finland","Latvia","Lithuania","Sweden","Poland") %>%
  gather(key="Country",value="Population",c("Denmark","Estonia","Finland","Latvia","Lithuania","Sweden","Poland")) %>%
  group_by(Year) %>%
  summarise(Population=sum(Population,na.rm=T)) %>%
  mutate(Population=Population/1000)

p<-ggplot(dfpop) +
  theme_minimal() +
  geom_point(aes(x=Year,y=Population),shape=1,show.legend = FALSE)  +
  geom_line(aes(x=Year,y=Population),show.legend = FALSE)

p