setwd("C:/Users/fight/Documents/MIT/Research Projects/Preventive War")
library(tidyverse)
library(patchwork)


data<-read.csv("NMC-60-abridged.csv")
regions<-read.csv("COW-country-codes.csv")

data.regions<-left_join(data,regions,by=c("ccode"="CCode"))

# Add Overseas Territories ------------------------------------------------

# Japan in Oceania 1914-1945, incl Marshall Islands and North Mariana
data.regions$region2[data.regions$ccode==740 & data.regions$year>1913 & data.regions$year<1946]<-4

# US in Oceania 1898-present, incl Hawaii and Guam
data.regions$region2[data.regions$ccode==2 & data.regions$year>1897]<-4

# US in East Asia 1898-present, incl Phillipines and Korea
data.regions$region3[data.regions$ccode==2 & data.regions$year>1897]<-2

# US in Europe 1918-1919 and 1942-present
data.regions$region4[data.regions$ccode==2 & (data.regions$year>1942 |data.regions$year==1918|data.regions$year==1919)]<-1

# France in Oceania 1842-present, French Polynesia, New Caledonia, Wallis & Futuna
data.regions$region3[data.regions$ccode==220 & data.regions$year>1841]<-4

# Add COLDAT --------------------------------------------------------------

colonizers<-read.csv("COLDAT_dyads_truncated.csv")

colonized.regions<-colonizers %>% group_by(colonizer, target_region) %>% summarize(ccode=min(colonizer_code), start=min(colstart_mean), end=max(colend_max))

colonized.region.year<-colonized.regions %>% mutate(year=map2(start, end, `:`)) %>% select(-start, -end) %>% unnest(cols = year)

colonized.regions.year<-colonized.region.year %>% pivot_wider(names_from = target_region, names_prefix="colony", values_from = target_region, values_fill = 0)

data.regions.colonies<-left_join(data.regions,colonized.regions.year,by=c("ccode"="ccode", "year"="year"))

#remove excess columns
data.regions.colonies<-data.regions.colonies %>% 
  select(-colonizer,-version,-StateNme,-StateAbb) 



# Export ------------------------------------------------------------------

write.csv(data.regions.colonies, "NMC_regions_victoria.csv")


