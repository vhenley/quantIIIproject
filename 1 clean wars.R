setwd("C:/Users/fight/Documents/MIT/Research Projects/Preventive War")
library(knitr)
library(tidyverse)
library(patchwork)
library(formatR)
library(haven)
library(xtable)
library(stargazer)


data.og<-read.csv("Inter-StateWarData_v4.0.csv")

data.wars<-data.og %>% group_by(WarNum) %>% summarize(WarName=min(WarName), 
                                                      StartYear=min(StartYear1), 
                                                      EndYear=max(StartYear2, StartYear1), 
                                                      States=paste(StateName, collapse="; "),
                                                      BatDeath=sum(BatDeath))

write.csv(data.wars, file = "COW Inter-state Wars.csv")


# Create Categorical Outcomes Dataset -------------------------------------

data.2<-read.csv("COW Data_updated regions v2.csv")

Region<-c(rep("Europe", 192), rep("East Asia", 192), rep("South Asia", 192), rep("Oceania", 192), 
          rep("North America", 192), rep("South America", 192), rep("MENA", 192), rep("Africa", 192))
RegionNum<-c(rep(1, 192), rep(2, 192), rep(3, 192), rep(4, 192), 
          rep(5, 192), rep(6, 192), rep(7, 192), rep(8, 192))
Year<-rep(seq(1816, 2007, 1), 8)
data.year<-data.frame(cbind(Year, Region, RegionNum))
data.year$WarCount<-0

## count wars ongoing in each year 
# for (i in 1:nrow(data.year)) {
#   data.year$WarCount[i]<-nrow(data.2 %>% filter(RegionNum==data.year$RegionNum[i] & 
#                                         StartYear<=data.year$Year[i] & 
#                                         EndYear>=data.year$Year[i]))
#   data.year$WarCountP[i]<-nrow(data.2 %>% filter(RegionNum==data.year$RegionNum[i] & 
#                                                   StartYear<=data.year$Year[i] & 
#                                                   EndYear>=data.year$Year[i] &
#                                                   Preventive==1))
#   data.year$WarCountA[i]<-nrow(data.2 %>% filter(RegionNum==data.year$RegionNum[i] & 
#                                                    StartYear<=data.year$Year[i] & 
#                                                    EndYear>=data.year$Year[i] & 
#                                                    Preventive==0))
# }

# count wars that start in each year
for (i in 1:nrow(data.year)) {
  data.year$WarCount[i]<-nrow(data.2 %>% filter(RegionNum==data.year$RegionNum[i] & 
                                                  StartYear==data.year$Year[i]))
  data.year$WarCountP[i]<-nrow(data.2 %>% filter(RegionNum==data.year$RegionNum[i] & 
                                                   StartYear==data.year$Year[i] & 
                                                   Preventive==1))
  data.year$WarCountA[i]<-nrow(data.2 %>% filter(RegionNum==data.year$RegionNum[i] & 
                                                   StartYear==data.year$Year[i] & 
                                                   Preventive==0))
}

max(data.year$WarCount)

table1 <- data.2 %>% group_by(Region) %>% summarize(WarCountA=n()-sum(Preventive), WarCountP=sum(Preventive))

xtable(table1)

f1a<-ggplot(data.year, aes(x=as.numeric(Year), y=WarCountA, colour=Region))+
  geom_point()+
  #geom_smooth(se=FALSE, method="loess", span=0.1)+
  ylab("Number of Aggrandizing Wars")+
  xlab("Year")+
  theme_bw()

f1b<-ggplot(data.year, aes(x=as.numeric(Year), y=WarCountP, colour=Region))+
  geom_point()+
  #geom_smooth(se=FALSE, method="loess", span=0.1)+
  ylab("Number of Preventive Wars")+
  xlab("Year")+
  ylim(0,3)+
  theme_bw()

f1a / f1b + plot_layout(guides = 'collect')

write.csv(data.year, "Categorical Outcomes v1.csv")


# Create Event Count Dataset ----------------------------------------------



data.year$Period<-rep(ceiling(seq(1,192,1)/10),8)

data.period<-data.year %>% group_by(Period, Region, RegionNum) %>% summarize(StartYear=min(Year), 
                                             EndYear=max(Year)
                                             )

data.period$WarCount<-0
data.period$WarCountA<-0
data.period$WarCountP<-0

# # count wars ongoing per period
# for (i in 1:nrow(data.period)) {
#   data.period$WarCount[i]<-nrow(data.2 %>% filter(RegionNum==data.period$RegionNum[i] & 
#                                                   StartYear<=data.period$EndYear[i] & 
#                                                   EndYear>=data.period$StartYear[i]))
#   data.period$WarCountP[i]<-nrow(data.2 %>% filter(RegionNum==data.period$RegionNum[i] & 
#                                                      StartYear<=data.period$EndYear[i] & 
#                                                      EndYear>=data.period$StartYear[i] &
#                                                    Preventive==1))
#   data.period$WarCountA[i]<-nrow(data.2 %>% filter(RegionNum==data.period$RegionNum[i] & 
#                                                     StartYear<=data.period$EndYear[i] & 
#                                                     EndYear>=data.period$StartYear[i] & 
#                                                    Preventive==0))
# }

# count wars started in each period

for (i in 1:nrow(data.period)) {
  data.period$WarCount[i]<-nrow(data.2 %>% filter(RegionNum==data.period$RegionNum[i] & 
                                                    StartYear<=data.period$EndYear[i] & 
                                                    StartYear>=data.period$StartYear[i]))
  data.period$WarCountP[i]<-nrow(data.2 %>% filter(RegionNum==data.period$RegionNum[i] & 
                                                     StartYear<=data.period$EndYear[i] & 
                                                     StartYear>=data.period$StartYear[i] &
                                                     Preventive==1))
  data.period$WarCountA[i]<-nrow(data.2 %>% filter(RegionNum==data.period$RegionNum[i] & 
                                                     StartYear<=data.period$EndYear[i] & 
                                                     StartYear>=data.period$StartYear[i] & 
                                                     Preventive==0))
}
  
  
mean(data.period$WarCountA)
var(data.period$WarCountA)

mean(data.period$WarCountP)
var(data.period$WarCountP)

max(data.period$WarCountA)

bin<-c(0:5)
pois.a<-dpois(c(0:5), lambda=mean(data.period$WarCountA))
obs.a<-c(length(data.period$WarCountA[data.period$WarCountA==0]),
       length(data.period$WarCountA[data.period$WarCountA==1]),
       length(data.period$WarCountA[data.period$WarCountA==2]),
       length(data.period$WarCountA[data.period$WarCountA==3]),
       length(data.period$WarCountA[data.period$WarCountA==4]),
       length(data.period$WarCountA[data.period$WarCountA==5]))/nrow(data.period)

data.f2a <- data.frame(cbind(bin, pois.a, obs.a))

pois.p<-dpois(c(0:5), lambda=mean(data.period$WarCountP))
obs.p<-c(length(data.period$WarCountP[data.period$WarCountP==0]),
         length(data.period$WarCountP[data.period$WarCountP==1]),
         length(data.period$WarCountP[data.period$WarCountP==2]),
         length(data.period$WarCountP[data.period$WarCountP==3]),
         length(data.period$WarCountP[data.period$WarCountP==4]),
         length(data.period$WarCountP[data.period$WarCountP==5]))/nrow(data.period)

data.f2p <- data.frame(cbind(bin, pois.p, obs.p))

f2a<-ggplot(data.f2a)+
  geom_col(aes(x=bin, y=obs.a))+
  geom_point(aes(x=bin, y=pois.a), color="cornflowerblue", size=3)+
  geom_segment(aes(x=bin, y=0, xend=bin, yend=pois.a), color="cornflowerblue")+
  xlab("Number of Aggrandizing Wars")+
  ylab("Density")+
  ylim(0,0.9)+
  theme_bw()

f2b<-ggplot(data.f2p)+
  geom_col(aes(x=bin, y=obs.p))+
  geom_point(aes(x=bin, y=pois.p), color="cornflowerblue", size=3)+
  geom_segment(aes(x=bin, y=0, xend=bin, yend=pois.p), color="cornflowerblue")+
  xlab("Number of Preventive Wars")+
  ylab("Density")+
  ylim(0,0.9)+
  theme_bw()

f2a/f2b


# Wars per Year per Region ------------------------------------------------


