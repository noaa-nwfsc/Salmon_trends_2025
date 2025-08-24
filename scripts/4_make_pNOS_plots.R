##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024

## make trend plots for pNOS - proportion natural origin spawners -- Figure 8, 9 and SI Figures S20-S25 


library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(forcats)
library(gridExtra)

### load pNOS data, which is in the same file as the spawner counts

setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive")

ts = read.csv("input_files/PNW_and_CA_abundance_8.6.24_with_citations_trimmed.csv",stringsAsFactors = F)
head(ts)

# also get the fitted output 

fitted = read.csv("model_fits/fitted_data_12.12.24.csv",stringsAsFactors = F)


#### set up variables for plotting #########


head(ts)
summary(ts$FRACWILD)
nrow(ts)
summary(ts)

ts$year = ts$BROOD_YEAR
ts = subset(ts,year>=2010 & year <=2020) # limit to 2010 on where most pops have data

nrow(ts)
ts$population = ts$COMMON_POPULATION_NAME
ts$Species = ts$SPECIES
ts$ESU = str_replace_all(ts$ESU,"/","-")
summary(ts)
subset(ts,FRACWILD==0)
ts = subset(ts,ESU != "Even-year Pink") # this ESU contains only 3 data points so removed here

####### make plots

head(ts)
head(fitted)
ts = merge(ts,fitted,by=c("SPECIES","ESU","population","year"),all.x=T)
head(ts)
ts$FRACWILD = ts$FRACWILD.x # need to keep the fracwild from the raw data 
head(ts)
# get rid of rows with missing FRACWIlD
ts = subset(ts, !is.na(FRACWILD))
summary(ts)
ts$nwild = ts$FRACWILD*ts$Median

sums.wild = aggregate(nwild~Species+ESU+year,data=ts,FUN=sum)
sums.total = aggregate(Median~Species+ESU+year,data=ts,FUN=sum)
unFW = aggregate(FRACWILD~Species+ESU+year,data=ts,FUN=mean)
head(sums.wild)
head(unFW)
names(unFW)[4] = "FRACWILDuw"
nrow(unFW)
nrow(sums.wild)
nrow(sums.total)

means.pnos = merge(sums.total,sums.wild,by=c("Species","ESU","year"),all.x=T)
head(means.pnos)
nrow(means.pnos)
means.pnos$FRACWILD = means.pnos$nwild/means.pnos$Median
means.pnos = merge(means.pnos,unFW,by = c("Species","ESU","year"))
head(means.pnos)


summary(means.pnos)

sp.list = c("Chinook", "Coho", "Steelhead", "Chum", "Sockeye", "Pink")

for (i in 1:length(sp.list))
{
  sp = sp.list[i]
  ts.sp = subset(ts,Species==sp)
  means.sp = subset(means.pnos,Species==sp)
  
  base = ggplot(data=ts.sp,aes(x=year,y=FRACWILD)) + geom_line(aes(group=population),col="gray",alpha=.7) + theme_light()  + scale_y_continuous(trans='identity',labels = function(x) format(x, scientific = FALSE))  + ylab("pNOS") + xlab("Year")  + geom_line(data=means.sp,col='red',size=2,alpha=.7) + geom_line(data=means.sp,aes(x=year,y=FRACWILDuw),col='black') + scale_x_continuous(breaks=c(2010,2012,2014,2016,2018,2020))
  
  plot = base + facet_wrap(~ESU,labeller=label_wrap_gen(width=25)) + theme(text=element_text(size=12))
  
  pdf(paste(sp,"_pNOSplot_12.12.24.pdf"),width=11,height=8.5)
  print(plot)
  dev.off()
}


#### trend analysis and plots
# goal is to fit a linear model to the pNOS data, one population at a time
summary(ts)
head(ts)
ts$index = paste(ts$Species,ts$ESU,ts$pop,sep="_")
nrow(ts)

tmp = by(ts,list(ts$index),function(x) lm(FRACWILD~year,data=x))
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

## and of the average fracwild for each ESU

head(means.pnos)
means.pnos = subset(means.pnos,year>=2010)

means.pnos$index = paste(means.pnos$Species,means.pnos$ESU,sep="_")
means.pnos$index

tmp = by(means.pnos,list(means.pnos$index),function(x) lm(FRACWILDuw~year,data=x))
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

# make the plots
# load naming convention file. This is in the input_files folder - adjust path as needed
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
trends.sums$footnote = ifelse(is.na(trends.sums$trend),"ND","")

# make plot
mean.plot = ggplot(data=trends.sums,aes(x=trend,y=fct_inorder(org))) + geom_point(size=2,color=trends.sums$col,shape=trends.sums$shape) + geom_errorbarh(aes(xmin=trend-1.96*stderr,xmax=trend+1.96*stderr),color=trends.sums$col)  + theme_minimal_grid() + theme(axis.text.y=element_blank())  + xlab("pNOS trend")+ geom_vline(xintercept=0) + ylab("")

final.plot = mean.plot + geom_point(data=trends,aes(x=trend,y=fct_inorder(org)),alpha=.5,color=trends$col,shape=trends$shape)  


##### add a box plot of average pNOS for populations within each DPS

######  
head(ts)
popmeans = aggregate(cbind(FRACWILD,Median)~Species+ESU+population,data=ts,FUN=mean)

popmeans = merge(popmeans,stone,by.x = "ESU",by.y = "DPS_me",all.y=T)
head(popmeans)
nrow(popmeans)

popmeans$col = NA
popmeans$col = ifelse(popmeans$Species.y=="Chinook",cs[1],popmeans$col)
popmeans$col = ifelse(popmeans$Species.y=="Chum",cs[2],popmeans$col)
popmeans$col = ifelse(popmeans$Species.y=="Coho",cs[3],popmeans$col)
popmeans$col = ifelse(popmeans$Species.y=="Pink",cs[4],popmeans$col)
popmeans$col = ifelse(popmeans$Species.y=="Sockeye",cs[5],popmeans$col)
popmeans$col = ifelse(popmeans$Species.y=="Steelhead",cs[6],popmeans$col)

popmeans$org = paste(popmeans$Status,popmeans$Species.y,popmeans$DPS_short,sep=":")
popmeans = popmeans[order(popmeans$Species.y,popmeans$Status,popmeans$DPS_short),]
popmeans$org = factor(popmeans$org,ordered = T)
popmeans$org
popmeans$footnote = ifelse(is.na(popmeans$FRACWILD),"ND","")
head(popmeans)
nrow(popmeans)
temp.col = subset(trends.sums,!is.na(trend)) # just doing this get the correct colors to match boxplot which removes NAs
temp.col$ESU

bw.plot = ggplot(data=popmeans,aes(x=FRACWILD,y=fct_inorder(org))) + geom_boxplot(color=temp.col$col) + theme_minimal_grid()  + theme(axis.text.y=element_text(hjust=0,color=lab.colors$col),legend.position="none") + xlab("pNOS distribution") + ylab("") + geom_text(data=trends.sums,aes(x=0,y=fct_inorder(org)),label = trends.sums$footnote,color=lab.colors$col)



pdf("combo pNOS plot bw and trends 12.12.24.pdf",height=8,width=10)
plot_grid(bw.plot,final.plot,ncol=2,rel_widths = c(.7,.3))
dev.off()



##### make a violin plot comparing pNOS among species and DPS -- Figure 9

popmeans$status2 = ifelse(popmeans$Status == "U","U","L")

testplot = ggplot(data = subset(popmeans, Species.y != "Pink"),aes(y=FRACWILD,x=status2,fill=Species.y)) + geom_violin() + theme_minimal_grid() + facet_wrap(~Species.y,nrow=1) + xlab("Status") + ylab("pNOS") + scale_fill_manual(values = cs[c(1,2,3,5,6)]) + theme(legend.position = "none") + geom_point(alpha=.5)

pdf("pNOS_by_species_and_status_12.12.24.pdf",height=3,width=7)
testplot
dev.off()


#### examine relationship between pNOS and overall abundance - Figure S25
head(popmeans)

pnos_abn_plot = ggplot(data=subset(popmeans,Species.y !="Pink"),aes(x=FRACWILD,y=Median)) + geom_point() + geom_smooth(method=lm)+ facet_wrap(~Species.y,scales="free")


pdf("SI FIGURE pnos v abundance 12.23.24.pdf",height=8,width=10)
print(pnos_abn_plot)
dev.off()

# model fits for relationship between pNOS and abundance

chinook.m = lm(Median~FRACWILD,data=subset(popmeans,Species.y=="Chinook"))
summary(chinook.m)

chum.m = lm(Median~FRACWILD,data=subset(popmeans,Species.y=="Chum"))
summary(chum.m)

coho.m = lm(Median~FRACWILD,data=subset(popmeans,Species.y=="Coho"))
summary(coho.m)

sockeye.m = lm(Median~FRACWILD,data=subset(popmeans,Species.y=="Sockeye"))
summary(sockeye.m)

steelhead.m = lm(Median~FRACWILD,data=subset(popmeans,Species.y=="Steelhead"))
summary(steelhead.m)


### look at relationship of pNOS with hatchery releases

rel = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/releases and biomass by esu_fixCA.CSV1058211_10.29.24.csv", stringsAsFactors = F)
head(rel)
rel = subset(rel,Year>=2010)
averel = aggregate(releases~Species+ESU,data=rel,FUN=mean)
head(averel)
head(popmeans)
popmeans_rel = merge(popmeans,averel,by.x = c("ESU","Species.y"), by.y = c("ESU","Species"))
nrow(popmeans_rel)
head(popmeans_rel)

dpsmeans = aggregate(FRACWILD~Species.y+ESU,data=popmeans,FUN=mean)
dpsmeans

dpsmeans_rel = merge(dpsmeans,averel,by.x = c("ESU","Species.y"), by.y = c("ESU","Species"))

pnos_rel_plot_pop = ggplot(data=popmeans_rel,aes(x=releases,y=FRACWILD)) + geom_point(alpha=.5) + facet_wrap(~Species.y,scales="free") + geom_smooth(method=lm) 

pdf("SI Figure pnos_release_plot 12.12.24.pdf",height=8,width=10)
pnos_rel_plot_pop
dev.off()

pnos_rel_plot_DPS = ggplot(data=dpsmeans_rel,aes(x=releases,y=FRACWILD)) + geom_point(alpha=.5) + facet_wrap(~Species.y,scales="free") + geom_smooth(method=lm) 

pdf("SI Figure pnos_release_plot_DPS 12.12.24.pdf",height=8,width=10)
pnos_rel_plot_DPS
dev.off()


