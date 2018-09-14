Parameter<-c("DIN","PO4","Chla","Secchi","O2debt","HEAT")
Variable<-c("Winter\nDIN [µM]","Winter\nDIP\n[µM]","Summer\nChl a\n[µg/l]","Summer\nSecchi\n[m]","O2\ndebt\n[mg/l]","HEAT")
Variable<-factor(Variable,levels=Variable)
Parameters<-data.frame(Parameter,Variable)

dfeea <- df %>%
  filter(Scenario=="BSAP") %>%
  group_by(Year,Parameter) %>% 
  summarise(Value=mean(Status,na.rm=T)) %>%
  ungroup()

dfeea <- HEAT %>% 
  ungroup() %>%
  filter(Scenario=="BSAP") %>%
  select(Basin,Year,EUT_Ratio) %>%
  group_by(Year) %>%
  summarise(Value=mean(EUT_Ratio,na.rm=T)) %>%
  ungroup() %>%
  mutate(Parameter="HEAT") %>%
  select(Year,Parameter,Value) %>%
  bind_rows(dfeea) %>%
  left_join(Parameters,by="Parameter") %>%
  arrange(Variable,Year) %>%
  mutate(Value10yr=NA) %>%
  mutate(Threshold=ifelse(Parameter=="HEAT",1,NA))
  
# --------- running average -------------------------------

nmax<- nrow(distinct(ungroup(dfeea),Year))
npar<- nrow(distinct(ungroup(dfeea),Parameter))

#nmax<-nrow(dfBaltic)
  for(pa in 1:npar){
       for(i in 1:nmax){
        noffset<-(pa-1)*nmax

        #10 yr avg
        nfrom<- i-4
        nto<- i+5
        nfrom<-ifelse(nfrom<1,1,nfrom)
        nto<-ifelse(nto>nmax,nmax,nto)
        nfrom=nfrom+noffset
        nto=nto+noffset
        dfeea$Value10yr[i+noffset] <- mean(dfeea$Value[nfrom:nto],na.rm=T)
        if(sum(is.na(dfPlotBasin$Value[nfrom:nto]))<5){
          dfeea$Value10yr[i+noffset] <- mean(dfeea$Value[nfrom:nto],na.rm=T)
        }else{dfeea$Value10yr[i+noffset] <- NA}

      }}

# ------------- plot ---------------------------------------

peea<-ggplot(dfeea) + 
  theme_minimal(base_size = 6) +
  facet_grid(Variable~., scales="free") +
  geom_point(aes(x=Year,y=Value),shape=1, alpha=0.3,size=0.3)  + 
  geom_line(aes(x=Year,y=Value10yr),size=0.3) +
  geom_line(aes(x=Year,y=Threshold),linetype=2,colour="#ff0000",size=0.4) +
  labs(y="", 
       title="HEAT Baltic Sea")
peea

fig<-paste0("./figures/ETC_ICM/HEAT_350yrs.png")
ggsave(peea,filename=fig, width = 7.5, height = 10, units = "cm", dpi=300)

