##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024

###### More DPS summary plots -- Figures 3, 4, S1 and S2  ########



library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(forcats)
library(gridExtra)

### load data

# set working directory where output will be stored
setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive")

# load DPS level trends
trends = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits/DPS_spawners_trends_with_stderr_12.12.24.csv",stringsAsFactors = F)

trends$status2 = ifelse(trends$Status=="U","U","L")

# load population level trends

poptrends = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits/linear_trends_allpops_12.12.24.csv",stringsAsFactors = F)

# load naming convention file 
stone = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/DPS_name_conv_table_with_numbers.csv",stringsAsFactors = F)


poptrends = merge(poptrends,stone,by.x = "ESU",by.y = "DPS_me",all.y=T)

poptrends$status2 = ifelse(poptrends$Status=="U","U","L")


### get aggregate and pop level geo means.  Aggregate were already calculated.
dpsgeo = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits/summary_stats_12.12.24.csv",stringsAsFactors = F)

dpsgeo$status2 = ifelse(dpsgeo$Status=="U","U","L")

# population level geo means need to be calculated

ab = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits/fitted_data_12.12.24.csv",stringsAsFactors = F)

time1 = c(1995,1999)
time2 = c(2016,2020)

set1 = subset(ab,year>=time1[1] & year <= time1[2])
set2 = subset(ab,year>=time2[1] & year <= time2[2])

geo1 = aggregate(Median~SPECIES+ESU+population,data= set1,FUN = function(x) exp(mean(log(x))) )
head(geo1)
geo2 = aggregate(Median~SPECIES+ESU+population,data = set2, FUN = function(x) exp(mean(log(x))))
head(geo2)
geo = merge(geo1,geo2,by = c("SPECIES","ESU","population"))
head(geo)
names(geo)[4] = paste(time1[1],time1[2],sep="-")
names(geo)[5] = paste(time2[1],time2[2],sep="-")
head(geo)

geo$delta = (geo$`2016-2020` - geo$`1995-1999`) / geo$`1995-1999`

geo = merge(geo,stone,by.x = "ESU",by.y="DPS_me",all.y=T)
head(geo)
geo$status2 = ifelse(geo$Status=="U","U","L")

write.csv(geo,"population_geo_means.csv")

#############

#### plots - based on DPS trends - Figure S1 ######

cs=brewer.pal(6,"Dark2")
cs

bystatusplot = ggplot(data = trends,aes(y=trend,x=status2,fill=Species.y)) + geom_violin(draw_quantiles = .5) + theme_minimal_grid() + facet_wrap(~Species.y,nrow=1) + xlab("Status") + ylab("trend") + geom_point(alpha=.5) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")

bydomainplot = ggplot(data = trends,aes(y=trend,x=Region,fill=Species.y)) + geom_violin() + theme_minimal_grid() + facet_wrap(~Species.y,nrow=1) + xlab("Region") + ylab("trend") + geom_point(alpha=.5) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")


comboplot = plot_grid(bystatusplot,bydomainplot,ncol=1,labels = c("A","B"))
pdf("DPS_trend_by_region_species_status_12.16.24.pdf",height=6,width=8)
comboplot
dev.off()


##### plots based on population trends -- Figure 3 #####

bystatusplotpop = ggplot(data = poptrends,aes(y=trend,x=status2,fill=Species.y)) + geom_violin(draw_quantiles = .5) + theme_minimal_grid() + facet_wrap(~Species.y,nrow=1) + xlab("Status") + ylab("trend") + geom_point(alpha=.5) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")

bydomainplotpop = ggplot(data = poptrends,aes(y=trend,x=Region,fill=Species.y)) + geom_violin(draw_quantiles = .5) + theme_minimal_grid() + facet_wrap(~Species.y,nrow=1) + xlab("Region") + ylab("trend") + geom_point(alpha=.5) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")

comboplotpop = plot_grid(bystatusplotpop,bydomainplotpop,ncol=1,labels = c("A","B"))
pdf("pop_trend_by_region_species_status_12.16.24.pdf",height=6,width=8)
comboplotpop
dev.off()


### same plots, but now with geomean abundance (Figures 4 and S2)

absonly = subset(dpsgeo,abn_type == "abs") # only use DPS with relatively complete data
head(absonly)

bystatusgeodps = ggplot(data = absonly,aes(y=X1995.1999,x=status2,fill=Species)) + geom_violin(draw_quantiles = .5,position=position_nudge(x=-.25)) + theme_minimal_grid() + facet_wrap(~Species,nrow=1) + xlab("Status") + ylab("Geomean abundance") + geom_point(alpha=.5,position=position_nudge(x=-.25)) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")  +scale_y_log10(breaks=c(10,100,1000,10000,100000),labels=c("10","","1000","","100,000")) + geom_violin(aes(y=X2016.2020,x=status2,fill=Species,alpha=.7),draw_quantiles = .5,position = position_nudge(x=.25)) + geom_point(aes(y=X2016.2020,x=status2),alpha=.5,position=position_nudge(x=.25))

bydomaingeodps = ggplot(data = absonly,aes(y=X1995.1999,x=Region,fill=Species)) + geom_violin(draw_quantiles = .5,position=position_nudge(x=-.25)) + theme_minimal_grid() + facet_wrap(~Species,nrow=1) + xlab("Status") + ylab("Geomean abundance") + geom_point(alpha=.5,position=position_nudge(x=-.25)) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")  +scale_y_log10(breaks=c(10,100,1000,10000,100000),labels=c("10","","1000","","100,000")) + geom_violin(aes(y=X2016.2020,x=Region,fill=Species,alpha=.7),draw_quantiles = .5,position = position_nudge(x=.25)) + geom_point(aes(y=X2016.2020,x=Region),alpha=.5,position=position_nudge(x=.25))

comboplotDPSgeo = plot_grid(bystatusgeodps,bydomaingeodps,ncol=1,labels = c("A","B"))
pdf("DPS_geo_by_region_species_status_absonly_12.16.24.pdf",height=6,width=8)
comboplotDPSgeo
dev.off()

##### geo plots of based populations

bystatusgeopop = ggplot(data = geo,aes(y=`1995-1999`,x=status2,fill=Species)) + geom_violin(draw_quantiles = .5,position=position_nudge(x=-.25)) + theme_minimal_grid() + facet_wrap(~Species,nrow=1) + xlab("Status") + ylab("Geomean abundance") + geom_point(alpha=.5,position=position_nudge(x=-.25)) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")  +scale_y_log10(breaks=c(10,100,1000,10000,100000),labels=c("10","","1000","","100,000")) + geom_violin(aes(y=`2016-2020`,x=status2,fill=Species,alpha=.7),draw_quantiles = .5,position = position_nudge(x=.25)) + geom_point(aes(y=`2016-2020`,x=status2),alpha=.5,position=position_nudge(x=.25))

bydomaingeopop = ggplot(data = geo,aes(y=`1995-1999`,x=Region,fill=Species)) + geom_violin(draw_quantiles = .5,position=position_nudge(x=-.25)) + theme_minimal_grid() + facet_wrap(~Species,nrow=1) + xlab("Status") + ylab("Geomean abundance") + geom_point(alpha=.5,position=position_nudge(x=-.25)) + scale_fill_manual(values = cs[c(1,2,3,4,5,6)]) + theme(legend.position = "none")  +scale_y_log10(breaks=c(10,100,1000,10000,100000),labels=c("10","","1000","","100,000")) + geom_violin(aes(y=`2016-2020`,x=Region,fill=Species,alpha=.7),draw_quantiles = .5,position = position_nudge(x=.25)) + geom_point(aes(y=`2016-2020`,x=Region),alpha=.5,position=position_nudge(x=.25))

comboplotPOPgeo = plot_grid(bystatusgeopop,bydomaingeopop,ncol=1,labels = c("A","B"))
pdf("POP_geo_by_region_species_status_12.16.24.pdf",height=6,width=8)
comboplotPOPgeo
dev.off()

###### look at relationship between aggregate trends and latitude

lats = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/esu_lat_long.csv",stringsAsFactors = F)
lats
trends

lats = merge(lats,trends,by.x="ESU2",by.y="ESU")
lats
names(lats)

lattrendplot = ggplot(data=lats,aes(x=trend,y=AVE_Y_CENT)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~SPECIES,scales="free") + ylab("latitude")

pdf("SI_FIGURE_trend_latitude_plot.pdf",width=11,height=8.5)
lattrendplot
dev.off()

## model fits
chinook.m.trend = lm(trend~AVE_Y_CENT,data=subset(lats,SPECIES=="Chinook"))
summary(chinook.m.trend)

chum.m.trend = lm(trend~AVE_Y_CENT,data=subset(lats,SPECIES=="Chum"))
summary(chum.m.trend)

coho.m.trend = lm(trend~AVE_Y_CENT,data=subset(lats,SPECIES=="Coho"))
summary(coho.m.trend)

sockeye.m.trend = lm(trend~AVE_Y_CENT,data=subset(lats,SPECIES=="Sockeye"))
summary(sockeye.m.trend)

steelhead.m.trend = lm(trend~AVE_Y_CENT,data=subset(lats,SPECIES=="Steelhead"))
summary(steelhead.m.trend)

##### look at relationship betwee aggregate abundance and latitude

lats = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/esu_lat_long.csv",stringsAsFactors = F)
lats
absonly
lats = merge(lats,absonly,by.x="ESU2",by.y="ESU")
names(lats)

latabnplot = ggplot(data=lats,aes(x=X2016.2020,y=AVE_Y_CENT)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~SPECIES,scales="free") + ylab("latitude") + xlab("geomean abundance 2016 - 2020")

pdf("SI_FIGURE_latitude_by_abundance_plot.pdf",width=11,height=8.5)
latabnplot
dev.off()

## model fits
chinook.m.abn = lm(X2016.2020~AVE_Y_CENT,data=subset(lats,SPECIES=="Chinook"))
summary(chinook.m.abn)

chum.m.abn = lm(X2016.2020~AVE_Y_CENT,data=subset(lats,SPECIES=="Chum"))
summary(chum.m.abn)

coho.m.abn = lm(X2016.2020~AVE_Y_CENT,data=subset(lats,SPECIES=="Coho"))
summary(coho.m.abn)

sockeye.m.abn = lm(X2016.2020~AVE_Y_CENT,data=subset(lats,SPECIES=="Sockeye"))
summary(sockeye.m.abn)

steelhead.m.abn = lm(X2016.2020~AVE_Y_CENT,data=subset(lats,SPECIES=="Steelhead"))
summary(steelhead.m.abn)

