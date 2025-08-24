##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024


#### Script for making the DPS-level abundance trend summary plots - Figure 2 and summary Table S1 

library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(forcats)
library(gridExtra)



####### Load the data file 

# set the working directory to the model output folder 
setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits")

# load the compiled, fitted data file created in the script 2_process_MARSS_output_and_plot_data.R
ts = read.csv("fitted_data_12.12.24.csv",stringsAsFactors = F) 

ts$total.raw = ts$Median # a kluge to make names match with the code below, which was originally used on the raw data but now is being used on the model estimates.
ts$pop = ts$population
ts$Species = ts$SPECIES

### get summed abundance for each DPS
sums.raw = aggregate(cbind(ts$total.raw),list(ts$Species,ts$ESU,ts$year),FUN=sum,na.rm=T)
sums.raw$pop = "sum"
head(sums.raw)
names(sums.raw) = c("Species","ESU","year","total.raw","pop")
head(sums.raw)

#### trend analysis and plots
# goal is to fit a linear model to the log spawner data, one population at a time

head(ts)
ts$index = paste(ts$Species,ts$ESU,ts$pop,sep="_")
ts = subset(ts,year >= 1995 & year <= 2020)
nrow(ts)

tmp = by(ts,list(ts$index),function(x) lm(log(total.raw)~year,data=x))
tmp = sapply(tmp,coef)
str(names(tmp[2,]))
trends = data.frame(name = names(tmp[2,]),stringsAsFactors = F)
trends$trend = tmp[2,]
head(trends)
pop_info = str_split(trends$name,"_",simplify = T)
head(pop_info)
trends$Species = pop_info[,1]
trends$ESU = pop_info[,2]
trends$pop = pop_info[,3]
head(trends)
write.csv(trends,"linear_trends_allpops_12.12.24.csv")

## and of the summed abundance for each ESU

sums.raw = subset(sums.raw,year>=1995 & year <=2020)
sums.raw$index = paste(sums.raw$Species,sums.raw$ESU,sep="_")
tmp = by(sums.raw,list(sums.raw$index),function(x) lm(log(total.raw)~year,data=x))
summary(tmp$`Chinook_California Coastal Chinook salmon`)$coefficients[4]
summary.tmp = lapply(tmp,FUN='summary')
coef.tmp = sapply(summary.tmp,coefficients)
str(coef.tmp)
coef.tmp[2,] ## these are the slopes
names(coef.tmp[2,])
coef.tmp[4,] ## these are the std errs of the slopes

trends.sums = data.frame(name = names(coef.tmp[2,]),stringsAsFactors = F)
trends.sums$trend = coef.tmp[2,]
head(trends.sums)
trends.sums$stderr = coef.tmp[4,]
pop_info = str_split(trends.sums$name,"_",simplify = T)
head(pop_info)
trends.sums$Species = pop_info[,1]
trends.sums$ESU = pop_info[,2]
head(trends.sums)

#create some shorter ESU names - necessary file is in 'input' - adult path as needed
stone = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/DPS_name_conv_table_with_numbers.csv",stringsAsFactors = F)

trends = merge(trends,stone,by.x = "ESU",by.y = "DPS_me",all.y=T)
trends.sums = merge(trends.sums,stone,by.x = "ESU",by.y = "DPS_me",all.y=T)

trends$org = paste(trends$Status,trends$Species.y,trends$DPS_short,sep=":")
trends.sums$org = paste(trends.sums$Status,trends.sums$Species.y,trends.sums$DPS_short,sep=":")

cs=brewer.pal(6,"Dark2")
trends.sums$col = NA
trends.sums$col = ifelse(trends.sums$Species.y=="Chinook",cs[1],trends.sums$col)
trends.sums$col = ifelse(trends.sums$Species.y=="Chum",cs[2],trends.sums$col)
trends.sums$col = ifelse(trends.sums$Species.y=="Coho",cs[3],trends.sums$col)
trends.sums$col = ifelse(trends.sums$Species.y=="Pink",cs[4],trends.sums$col)
trends.sums$col = ifelse(trends.sums$Species.y=="Sockeye",cs[5],trends.sums$col)
trends.sums$col = ifelse(trends.sums$Species.y=="Steelhead",cs[6],trends.sums$col)

trends.sums$shape = NA
trends.sums$shape = ifelse(trends.sums$Status == "U",1,19)
trends$shape = NA
trends$shape = ifelse(trends$Status == "U",1,19)

trends$col = NA
trends$col = ifelse(trends$Species.y=="Chinook",cs[1],trends$col)
trends$col = ifelse(trends$Species.y=="Chum",cs[2],trends$col)
trends$col = ifelse(trends$Species.y=="Coho",cs[3],trends$col)
trends$col = ifelse(trends$Species.y=="Pink",cs[4],trends$col)
trends$col = ifelse(trends$Species.y=="Sockeye",cs[5],trends$col)
trends$col = ifelse(trends$Species.y=="Steelhead",cs[6],trends$col)

lab.colors = data.frame(Species.y = trends.sums$Species.y)
lab.colors
lab.colors$col = NA
lab.colors$col = ifelse(lab.colors$Species.y=="Chinook",cs[1],lab.colors$col)
lab.colors$col = ifelse(lab.colors$Species.y=="Chum",cs[2],lab.colors$col)
lab.colors$col = ifelse(lab.colors$Species.y=="Coho",cs[3],lab.colors$col)
lab.colors$col = ifelse(lab.colors$Species.y=="Pink",cs[4],lab.colors$col)
lab.colors$col = ifelse(lab.colors$Species.y=="Sockeye",cs[5],lab.colors$col)
lab.colors$col = ifelse(lab.colors$Species.y=="Steelhead",cs[6],lab.colors$col)
lab.colors
lab.colors = lab.colors[order(lab.colors$Species.y),]
lab.colors

trends.sums = trends.sums[order(trends.sums$Species.y,trends.sums$Status,trends.sums$DPS_short),]

trends = trends[order(trends$Species.y,trends$Status,trends$DPS_short),]

trends.sums$org = factor(trends.sums$org,ordered = T)
trends$org = factor(trends$org,ordered = T)
trends.sums$org
trends.sums



###### make  tables to summarize trends ######

trend_counts = trends %>% group_by(Species.y) %>% count(ESU)
trends$pos = ifelse(trends$trend>0,1,0)
pos_counts = trends %>% group_by(Species.y,ESU) %>% count(pos)
pos_counts = subset(pos_counts,pos==1)
neg_counts = trends %>% group_by(Species.y,ESU) %>% count(pos)
neg_counts=subset(neg_counts,pos==0)
write.csv(trends.sums,"DPS_spawners_trends_with_stderr_12.12.24.csv")

sum.table = merge(trends.sums,trend_counts,by = c("Species.y","ESU"))
sum.table = merge(sum.table,pos_counts,by = c("Species.y","ESU"),all.x=T,suffixes = c(".tot",".pos"))
sum.table = merge(sum.table,neg_counts,by = c("Species.y","ESU"),all.x=T)

## make a table of geomeans for each ESU for two time periods
time1 = c(1995,1999)
time2 = c(2016,2020)

set1 = subset(sums.raw,year>=time1[1] & year <= time1[2])
set2 = subset(sums.raw,year>=time2[1] & year <= time2[2])

geo1 = aggregate(total.raw~Species+ESU,data= set1,FUN = function(x) exp(mean(log(x))) )
geo2 = aggregate(total.raw~Species+ESU,data = set2, FUN = function(x) exp(mean(log(x))))
geo = merge(geo1,geo2,by = c("Species","ESU"))
names(geo)[3] = paste(time1[1],time1[2],sep="-")
names(geo)[4] = paste(time2[1],time2[2],sep="-")
geo$delta = (geo$`2016-2020` - geo$`1995-1999`) / geo$`1995-1999`

geo = merge(geo,stone,by.x = "ESU",by.y="DPS_me",all.y=T)
geo_for_table = geo[,c(1,3,4,5,6)]
head(geo_for_table)
sum.table$Species = sum.table$Species.y
sum.table = merge(sum.table,geo_for_table,by = c("ESU"),all.x=T)
names(sum.table)
head(sum.table)
sum.table = sum.table[,c(1,4,5,10,11,12,13,16,17:25)]
names(sum.table)
write.csv(sum.table,"summary_stats_12.12.24.csv")

# make the geo mean abundance summary plot
head(geo)
geo$Species.y
geo$Species = geo$Species.y
cs=brewer.pal(6,"Dark2")
geo$col = NA
geo$col = ifelse(geo$Species=="Chinook",cs[1],geo$col)
geo$col = ifelse(geo$Species=="Chum",cs[2],geo$col)
geo$col = ifelse(geo$Species=="Coho",cs[3],geo$col)
geo$col = ifelse(geo$Species=="Pink",cs[4],geo$col)
geo$col = ifelse(geo$Species=="Sockeye",cs[5],geo$col)
geo$col = ifelse(geo$Species=="Steelhead",cs[6],geo$col)

geo.ordered = geo[order(geo$delta),]
geo.ordered$y = c(1:nrow(geo.ordered))

geo_complete = geo # if you want all pops

geo_complete$shape = NA
geo_complete$shape = ifelse(geo_complete$Status == "U",1,19)
geo_complete$org = paste(geo_complete$Status,geo_complete$Species.y,geo_complete$DPS_short,sep=":")
geo_complete = geo_complete[order(geo_complete$Species.y,geo_complete$Status,geo_complete$DPS_short),]
geo_complete$footnote = ifelse(geo_complete$abn_type=="index","LD","")


# make trend plot with change in geo mean abundance
mean.plot = ggplot(data=trends.sums,aes(x=trend,y=fct_inorder(org))) + geom_point(size=3,color=trends.sums$col,shape=trends.sums$shape) + geom_errorbarh(aes(xmin=trend-1.96*stderr,xmax=trend+1.96*stderr),color=trends.sums$col)  + theme_minimal_grid() + theme(axis.text.y=element_blank()) + ylab("") + geom_vline(xintercept=0) +xlab("Trend")

final.plot = mean.plot + geom_point(data=trends,aes(x=trend,y=fct_inorder(org)),alpha=.5,color=trends$col,shape=trends$shape)  

geocomp = ggplot(data=geo_complete,aes(x=`1995-1999`,y=fct_inorder(org))) + geom_point(size=3,color=geo_complete$col,shape=geo_complete$shape) + theme_minimal_grid() + theme(axis.text.y=element_text(hjust=0,color=geo_complete$col),legend.position="none") + ylab("Species and ESU")+ xlab("Geomean abundance, 1995-1999 to 2016-2020")  + geom_segment(aes(x=`1995-1999`,y=org, xend=`2016-2020`,yend=org),color=geo_complete$col,linewidth=1,arrow=arrow(length=unit(0.02,"npc"))) +scale_x_log10(breaks=c(10,100,1000,10000,100000),labels=c("10","","1000","","100,000")) + geom_text(aes(x=1.5,y=fct_inorder(org)),label=geo_complete$footnote,color=geo_complete$col)

pdf("combined abundance and trend_plot.12.12.24.pdf",width=11,height=8.5)
plot_grid(geocomp,final.plot,ncol=2,rel_widths = c(.7,.3))
dev.off()

##### for Chinook salmon only, compare trends for different adult run timing, similar to Atlas et al. 2023

# get run timing data

rt = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits/chinook_run_timing.csv",stringsAsFactors = F)

head(rt)
head(trends)
chin_trends = merge(trends,rt,by="name")
head(chin_trends)
chin_trends$shape_run = ifelse(chin_trends$run=="late",1,19)
dps.order = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits/chinook_dps_geo_order.csv",stringsAsFactors = F)
dps.order
chin_trends = merge(chin_trends,dps.order,by="ESU")
chin_trends = chin_trends[order(chin_trends$order),]
head(chin_trends)


chin.plot = ggplot(data=chin_trends,aes(x=trend,y=fct_inorder(org)))  + geom_point(size=3,shape=chin_trends$shape_run,alpha=.7) + theme_minimal_grid() + geom_vline(xintercept=0) +xlab("Trend")  + ylab("ESU") + geom_violin(data=subset(chin_trends,run=="early"),aes(x=trend,y=0),draw_quantiles = .5) + geom_violin(data=subset(chin_trends,run=="late"),aes(x=trend,y=-1),draw_quantiles = .5) + geom_point(data= subset(chin_trends,run=="early"),aes(x=trend,y=0),shape=19,alpha=.7,size=3) + geom_point(data= subset(chin_trends,run=="late"),aes(x=trend,y=-1),shape=1,alpha=.7,size=3) + annotate("text",y=-1,x=-.26,label="late") + annotate("text",y=0,x=-.26,label="early")

pdf("Chinook_trends_by_run_type.pdf",width=11,height=8.5)
chin.plot
dev.off()

chin_trends
chin_trends$status2 = ifelse(chin_trends$Status=="U","U","L")

chin.violin = ggplot(data=chin_trends,aes(y=trend,x=run)) + geom_violin(draw_quantiles = .5) + geom_point(alpha=.7) + facet_wrap(~status2) 
chin.violin

head(chin_trends)
chin.violin = ggplot(data=chin_trends,aes(x=trend,y=run)) + geom_violin(draw_quantiles = .5) + geom_point(alpha=.7,shape=chin_trends$shape_run) 

pdf("chinook_violin_trends_by_run.pdf",width=11,height=8.5)
chin.violin
dev.off()



write.csv(chin_trends,"chinook_trends_by_run.csv")
