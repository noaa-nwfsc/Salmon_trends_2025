
##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024

#### Make plots of trends in freshwater environmental data - Figure 10

library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(forcats)
library(gridExtra)


# get the data.  There were downloaded from the CCIEA website: https://www.integratedecosystemassessment.noaa.gov/regions/california-current/california-current-ecosystem-component-habitat using the ERDDAP links and then downloading each dataset for all ecoregions


setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files")

swe = read.csv("swe-eco-cciea_HB_SWE_2015_c81f_624c.csv",stringsAsFactors = F)
aug = read.csv("AugMx-eco-cciea_HB_AUGMX_a44c_6d24_b783.csv",stringsAsFactors = F)
min = read.csv("7day min - eco cciea_HB_FLO7_6941_f70a_fd3e.csv",stringsAsFactors = F)
max = read.csv("1daymx-eco-cciea_HB_FLO1_0735_8029_ef9e.csv",stringsAsFactors = F)

sst = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/NEPSST_arc 1995-2020.csv",stringsAsFactors = F)

# set wd to output location
setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive")

# make plots and evaluate trends

swe = swe[-1,]
names(swe)
swe$SWE_anomaly = as.numeric(swe$SWE_anomaly)
swe$year = substr(swe$time,1,4)
swe$year = as.integer(swe$year)

aug = aug[-1,]
names(aug)
aug$aug_mean_max = as.numeric(aug$aug_mean_max)
aug$year = substr(aug$time,1,4)
aug$year = as.integer(aug$year)
head(aug)

min = min[-1,]
names(min)
min$flow_anomaly_7_day_min = as.numeric(min$flow_anomaly_7_day_min)
min$year = substr(min$time,1,4)
min$year = as.integer(min$year)
head(min)

max = max[-1,]
names(max)
max$flow_anomaly_1_day_max = as.numeric(max$flow_anomaly_1_day_max)
max$year = substr(max$time,1,4)
max$year = as.integer(max$year)
head(max)

####
swe = subset(swe,year>=1995 & year <=2020)
head(swe)
swe.plot = ggplot(data=swe,aes(x=year,y=SWE_anomaly)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~location,ncol=1,labeller=labeller(location=label_wrap_gen(width=25))) + theme_minimal_grid() + xlab("") + theme(axis.text=element_text(size=10),text=element_text(size=10)) + ylab("Snow water anomaly")
swe.plot

swe.model = lm(SWE_anomaly~year + location,data=swe)
summary(swe.model)


#### 

aug = subset(aug,year>=1995 & year <=2020)
head(aug)
aug.plot = ggplot(data=aug,aes(x=year,y=aug_mean_max)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~location,ncol=1,labeller=labeller(location=label_wrap_gen(width=25))) + theme_minimal_grid() + xlab("") + theme(axis.text=element_text(size=10),text=element_text(size=10)) + ylab("August mean max temp (C)")
aug.plot

aug.model = lm(aug_mean_max~year + location,data=aug)
summary(aug.model)

by(data=aug,INDICES = aug[,"location"],FUN = function(x) summary(lm(aug_mean_max~year,data=x) ))

####

min = subset(min,year>=1995 & year <=2020)
head(min)
min.plot = ggplot(data=min,aes(x=year,y=flow_anomaly_7_day_min)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~location,ncol=1,labeller=labeller(location=label_wrap_gen(width=25))) + theme_minimal_grid() + xlab("")  + ylab("7-day minimum flow anomaly") + theme(axis.text=element_text(size=10),text=element_text(size=10))
min.plot

min.model = lm(flow_anomaly_7_day_min~year + location,data=min)
summary(min.model)

by(data=min,INDICES = min[,"location"],FUN = function(x) summary(lm(flow_anomaly_7_day_min~year,data=x) ))

####

max = subset(max,year>=1995 & year <=2020)
head(max)
max.plot = ggplot(data=max,aes(x=year,y=flow_anomaly_1_day_max)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~location,ncol=1,labeller=labeller(location=label_wrap_gen(width=25))) + theme_minimal_grid() + xlab("")  + ylab("1-day maximum flow anomaly") + theme(axis.text=element_text(size=10),text=element_text(size=10))
max.plot

max.model = lm(flow_anomaly_1_day_max~year + location  ,data=max)
summary(max.model)

by(data=max,INDICES = max[,"location"],FUN = function(x) summary(lm(flow_anomaly_1_day_max~year,data=x) ))

#####
sst
sst.plot = ggplot(data=sst,aes(x=year,y=SST_arc)) + geom_point() + geom_smooth(method=lm) + theme_minimal_grid() +xlab("Year") + ylab("SST arc") + theme(axis.text=element_text(size=10),text=element_text(size=10))
sst.plot

sst.model = lm(SST_arc~year,data=sst)
summary(sst.model)

air.plot = ggplot(data=sst,aes(x=year,y=PNWAirT)) + geom_point() + geom_smooth(method=lm) + theme_minimal_grid() +xlab("Year") + ylab("PNW Air Temp") + theme(axis.text=element_text(size=10),text=element_text(size=10))
air.plot

air.model = lm(PNWAirT~year,data=sst)
summary(air.model)



### combine into one plot

swe_aug.plot = plot_grid(swe.plot,aug.plot,nrow=1)

flow.plot = plot_grid(max.plot,min.plot,nrow=1)

combined.plot2 = plot_grid(swe_aug.plot,flow.plot,nrow=1)

combined.plot2

regional.plot = plot_grid(sst.plot,air.plot,nrow=1)
regional.plot

pdf("combined indicator plot2.pdf",width=10,height=6)
print(combined.plot2)
dev.off()

pdf("sst and air temp plot.pdf",width=6,height=3)
print(regional.plot)
dev.off()

