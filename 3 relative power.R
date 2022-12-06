setwd("C:/Users/fight/Documents/MIT/Research Projects/Preventive War")
library(tidyverse)
library(patchwork)


# Separate Regions --------------------------------------------------------

# read in raw data

data.raw<-read.csv("NMC_regions_v2.csv")

# fill missing values in with NA
data.raw$milex[data.raw$milex==-9]<-NA
data.raw$milper[data.raw$milper==-9]<-NA
data.raw$irst[data.raw$irst==-9]<-NA
data.raw$pec[data.raw$pec==-9]<-NA
data.raw$tpop[data.raw$tpop==-9]<-NA
data.raw$upop[data.raw$upop==-9]<-NA

# function to separate regions

sep_regions<-function(i){
  data.temp<-data.raw %>% filter(home1==i | home2==i | home3==i | 
                                   territory1==i | territory2==i | 
                                   base1==i | base2==i | base3==i | 
                                   colony1==i | colony2==i | colony3==i |
                                   colony4==i | colony5==i | colony6==i |
                                   colony7==i | colony8==i)
  #create binary variables for types of presence
  data.temp$home<-ifelse(data.temp$home1==i | data.temp$home2==i | data.temp$home3==i, 1, 0)
  data.temp$territory<-ifelse(data.temp$territory1==i | data.temp$territory2==i, 1, 0)
  data.temp$base<-ifelse(data.temp$base1==i | data.temp$base2==i | data.temp$base3==i, 1, 0)
  data.temp$colony<-ifelse(data.temp$colony1==i | data.temp$colony2==i | data.temp$colony3==i |
                             data.temp$colony4==i | data.temp$colony5==i | data.temp$colony6==i |
                             data.temp$colony7==i | data.temp$colony8==i, 1, 0)
  #remove old columns
  data.temp<-data.temp[,-which(names(data.temp) %in% c("home1", "home2", "home3",
                                                       "territory1", "territory2",
                                                       "base1", "base2", "base3",
                                                       "colony1", "colony2", "colony3",
                                                       "colony4", "colony5", "colony6",
                                                       "colony7", "colony8"))]
  return(data.temp)
}

# run function on each region
data.1<-sep_regions(1)
data.2<-sep_regions(2)
data.3<-sep_regions(3)
data.4<-sep_regions(4)
data.5<-sep_regions(5)
data.6<-sep_regions(6)
data.7<-sep_regions(7)
data.8<-sep_regions(8)


# Calculate Relative Power ---------------------------------------------------------

#set weights to adjust different types of presence
home.weight<-1
territory.weight<-0.5
colony.weight<-0.5
base.weight<-0.25

data.temp=data.1

#function to calculate relative power on each regional dataframe
rel_power<-function(data.temp){
  data.temp$home<-data.temp$home*home.weight
  data.temp$territory<-data.temp$territory*territory.weight
  data.temp$colony<-data.temp$colony*colony.weight
  data.temp$base<-data.temp$base*base.weight
  
  #calculate adjusted capability elements by type of presence
  data.temp$weight<-apply(data.temp[11:14],MARGIN = 1,FUN = sum, na.rm=TRUE)
  data.temp$weight[data.temp$weight>1]<-1
  
  data.temp$a_milex<-data.temp$weight*data.temp$milex
  data.temp$a_milper<-data.temp$weight*data.temp$milper
  data.temp$a_irst<-data.temp$weight*data.temp$irst
  data.temp$a_pec<-data.temp$weight*data.temp$pec
  data.temp$a_tpop<-data.temp$weight*data.temp$tpop
  data.temp$a_upop<-data.temp$weight*data.temp$upop
  
  #calculate by year sums
  data.temp.sum <- data.temp %>% group_by(year) %>% summarize(usum_milex=sum(milex, na.rm = TRUE), 
                                                              usum_milper=sum(milper, na.rm = TRUE),
                                                              usum_irst=sum(irst, na.rm = TRUE), 
                                                              usum_pec=sum(pec, na.rm = TRUE), 
                                                              usum_tpop=sum(tpop, na.rm = TRUE), 
                                                              usum_upop=sum(upop, na.rm = TRUE),
                                                              asum_milex=sum(a_milex, na.rm = TRUE), 
                                                              asum_milper=sum(a_milper, na.rm = TRUE),
                                                              asum_irst=sum(a_irst, na.rm = TRUE), 
                                                              asum_pec=sum(a_pec, na.rm = TRUE), 
                                                              asum_tpop=sum(a_tpop, na.rm = TRUE), 
                                                              asum_upop=sum(a_upop, na.rm = TRUE))
  
  
  #join sums to data
  data.temp.wsums <- left_join(data.temp, data.temp.sum, by="year")
  
  #calculate proportion of each capability element
  data.temp.wsums$uprop_milex<-data.temp.wsums$milex/data.temp.wsums$usum_milex
  data.temp.wsums$uprop_milper<-data.temp.wsums$milper/data.temp.wsums$usum_milper
  data.temp.wsums$uprop_irst<-data.temp.wsums$irst/data.temp.wsums$usum_irst
  data.temp.wsums$uprop_pec<-data.temp.wsums$pec/data.temp.wsums$usum_pec
  data.temp.wsums$uprop_tpop<-data.temp.wsums$tpop/data.temp.wsums$usum_tpop
  data.temp.wsums$uprop_upop<-data.temp.wsums$upop/data.temp.wsums$usum_upop
  
  data.temp.wsums$aprop_milex<-data.temp.wsums$a_milex/data.temp.wsums$asum_milex
  data.temp.wsums$aprop_milper<-data.temp.wsums$a_milper/data.temp.wsums$asum_milper
  data.temp.wsums$aprop_irst<-data.temp.wsums$a_irst/data.temp.wsums$asum_irst
  data.temp.wsums$aprop_pec<-data.temp.wsums$a_pec/data.temp.wsums$asum_pec
  data.temp.wsums$aprop_tpop<-data.temp.wsums$a_tpop/data.temp.wsums$asum_tpop
  data.temp.wsums$aprop_upop<-data.temp.wsums$a_upop/data.temp.wsums$asum_upop
  
  
  #average proportions to get regional cinc
  data.temp.wsums$ur_cinc<-apply(data.temp.wsums[,c("uprop_milex",
                                                    "uprop_milper",
                                                    "uprop_irst",
                                                    "uprop_pec",
                                                    "uprop_tpop",
                                                    "uprop_upop")], MARGIN = 1, FUN=mean, na.rm=TRUE)
  
  data.temp.wsums$ar_cinc<-apply(data.temp.wsums[,c("aprop_milex",
                                                    "aprop_milper",
                                                    "aprop_irst",
                                                    "aprop_pec",
                                                    "aprop_tpop",
                                                    "aprop_upop")], MARGIN = 1, FUN=mean, na.rm=TRUE)
  data.temp.out<-data.temp.wsums[,c("stateabb",
                                    "ccode",
                                    "year",
                                    "milex",
                                    "milper",
                                    "irst",
                                    "pec",
                                    "tpop",
                                    "upop",
                                    "cinc",
                                    "home",
                                    "territory",
                                    "base",
                                    "colony",
                                    "ur_cinc",
                                    "ar_cinc")]
  return(data.temp.out)
}

data.1.cinc<-rel_power(data.1)
data.2.cinc<-rel_power(data.2)
data.3.cinc<-rel_power(data.3)
data.4.cinc<-rel_power(data.4)
data.5.cinc<-rel_power(data.5)
data.6.cinc<-rel_power(data.6)
data.7.cinc<-rel_power(data.7)
data.8.cinc<-rel_power(data.8)



# Calculate Powers in the Sub-System --------------------------------------

#function to calculate number of great powers with adjusted and unadjusted relative power
great_power<-function(data.temp){
  data.temp<-data.temp %>% group_by(year) %>% mutate(umax_power=max(ur_cinc), amax_power=max(ar_cinc))
  # great powers are those with at least 1/3 of the power of the top power in the system
  data.temp$ugreat_power<-ifelse(data.temp$ur_cinc>=1/2*data.temp$umax_power,1,0)
  data.temp$agreat_power<-ifelse(data.temp$ar_cinc>=1/2*data.temp$amax_power,1,0)
  data.out<-data.temp[,-which(names(data.temp) %in% c("umax_power", "amax_power"))]
  return(data.out)
}

# calculate great powers per region
data.1.powers<-great_power(data.1.cinc)
data.2.powers<-great_power(data.2.cinc)
data.3.powers<-great_power(data.3.cinc)
data.4.powers<-great_power(data.4.cinc)
data.5.powers<-great_power(data.5.cinc)
data.6.powers<-great_power(data.6.cinc)
data.7.powers<-great_power(data.7.cinc)
data.8.powers<-great_power(data.8.cinc)

# summarize great powers per year in each region
data.1.sum.powers<-data.1.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))
data.2.sum.powers<-data.2.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))
data.3.sum.powers<-data.3.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))
data.4.sum.powers<-data.4.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))
data.5.sum.powers<-data.5.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))
data.6.sum.powers<-data.6.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))
data.7.sum.powers<-data.7.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))
data.8.sum.powers<-data.8.powers%>% group_by(year) %>% summarize(usys_gpowers=sum(ugreat_power), asys_gpowers=sum(agreat_power))

# test individual years
test<-data.4.powers[data.4.powers$year==1900,]

# check distributions per region
plot1<-ggplot(data.1.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="Europe")+
  theme_bw()

plot2<-ggplot(data.2.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="East Asia")+
  theme_bw()

plot3<-ggplot(data.3.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="South Asia")+
  theme_bw()

plot4<-ggplot(data.4.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="Oceania")+
  theme_bw()

plot5<-ggplot(data.5.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="North America")+
  theme_bw()

plot6<-ggplot(data.6.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="South America")+
  theme_bw()

plot7<-ggplot(data.7.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="MENA")+
  theme_bw()

plot8<-ggplot(data.8.sum.powers)+
  geom_line(aes(x=year, y=usys_gpowers), color="cornflowerblue")+
  geom_line(aes(x=year, y=asys_gpowers))+
  ylab("great powers")+
  ylim(0,7)+
  labs(title="Africa")+
  theme_bw()

(plot1+plot2)/(plot3+plot4)/(plot5+plot6)/(plot7+plot8)

# check id of great powers
plot1gp<-ggplot(data.1.powers[data.1.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="Europe")+
  theme_bw()

plot2gp<-ggplot(data.2.powers[data.2.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="East Asia")+
  theme_bw()

plot3gp<-ggplot(data.3.powers[data.3.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="South Asia")+
  theme_bw()

plot4gp<-ggplot(data.4.powers[data.4.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="Oceania")+
  theme_bw()

plot5gp<-ggplot(data.5.powers[data.5.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="North America")+
  theme_bw()

plot6gp<-ggplot(data.6.powers[data.6.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="South America")+
  theme_bw()

plot7gp<-ggplot(data.7.powers[data.7.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="MENA")+
  theme_bw()

plot8gp<-ggplot(data.8.powers[data.8.powers$agreat_power==1,])+
  geom_line(aes(x=year, y=ar_cinc, color=stateabb), size=1)+
  ylab("CINC")+
  labs(title="Africa")+
  theme_bw()

# Calculate Great Powers in World -----------------------------------------

data.world<-data.raw %>% group_by(year) %>% mutate(max_power=max(cinc))
# great powers are those with at least 1/3 of the power of the top power in the system
data.world$great_power<-ifelse(data.world$cinc>=1/2*data.world$max_power,1,0)

world.great.powers<-data.world %>% filter(great_power==1)

ggplot(world.great.powers, aes(x=year, y=cinc, color=stateabb))+
  geom_line(size=1)+
  theme_bw()
