setwd("C:/Users/fight/Documents/MIT/Research Projects/Preventive War")
library(tidyverse)
library(patchwork)

allies<-read.csv("alliance_v4.1_by_directed_yearly.csv")

countries<-read.csv("COW-country-codes.csv")
# add regions indicators for left side of dyad
allies<-left_join(allies, countries, by=c("ccode1"="CCode") )
# drop state name fields
drop<-c("StateAbb", "StateNme")
allies<-allies[,!names(allies) %in% drop]
# rename regions columns to identify them as left side of dyad
names(allies)[names(allies)=="region1"]<-"left_region1"
names(allies)[names(allies)=="region2"]<-"left_region2"

# add regions indicators for right side of dyad
allies<-left_join(allies, countries, by=c("ccode2"="CCode") )
allies<-allies[,!names(allies) %in% drop]
# rename regions columns to identify them as left side of dyad
names(allies)[names(allies)=="region1"]<-"right_region1"
names(allies)[names(allies)=="region2"]<-"right_region2"

allies$external<-ifelse(allies$left_region1!=allies$right_region1 || 
                          allies$left_region1!=allies$right_region2 ||
                          allies$left_region2!=allies$right_region1 ||
                          allies$left_region2!=allies$right_region2, 0, 1)

test<-allies %>% filter(external==1)
