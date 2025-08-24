
##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024

####### Make hatchery release plots -- Figure 7, Figures S14-18 #######


library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(stringr)
library(forcats)
library(gridExtra)


### load the data file
# set the correct directory, where plots will be stored

setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive")

# read in data file - adjust path as necessary

rel = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/releases and biomass by esu_fixCA.CSV1058211_10.29.24.csv",stringsAsFactors = F)

## make individual DPS plots (Figures S14-18)

sp.list = c("Chinook", "Coho", "Steelhead", "Sockeye","Chum")

for (i in 1:length(sp.list))
{
  sp = sp.list[i]
  ts.sp = subset(rel,Species==sp)
  
  base = ggplot(data=ts.sp,aes(x=Year,y=releases)) + geom_point(col="gray",alpha=.7) + theme_light() + xlim(1995,2020) + ylim(0,NA)
  
  plot = base + facet_wrap(~ESU,labeller=label_wrap_gen(width=25),scale="free") + theme(text=element_text(size=12))
  
  pdf(paste(sp,"_rel_plot_12.16.24_freescale.pdf"),width=11,height=8.5)
  print(plot)
  dev.off()
}

#### generate trends and overall summary plot (Figure 7)

ts = rel
#remove Clearwater Chinook since it is not in a DPS
table(ts$ESU)
ts = subset(ts,ESU != "CR Snake River Spring-Summer-run Chinook Salmon ESU")
ts$index = paste(ts$Species,ts$ESU,sep="_")
ts = subset(ts,Year >= 1995 & Year <= 2020)
summary(ts$releases)
ts$releases = ifelse(ts$releases == 0,NA,ts$releases)
ts$total.raw = log10(ts$releases) ## can change what is plotted here

tmp = by(ts,list(ts$index),function(x) lm(total.raw~Year,data=x))

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

# load naming conventions
stone = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/DPS_name_conv_table.csv",stringsAsFactors = F)

trends.sums = merge(trends.sums,stone,by.x = "ESU",by.y = "DPS_me",all.y=T)
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
trends.sums$org = factor(trends.sums$org,ordered = T)
trends.sums$org
trends.sums

trend.plot = ggplot(data=trends.sums,aes(x=trend,y=fct_inorder(org))) + geom_point(size=3,color=trends.sums$col,shape=trends.sums$shape) + geom_errorbarh(aes(xmin=trend-1.96*stderr,xmax=trend+1.96*stderr),color=trends.sums$col)  + theme_minimal_grid() + theme(axis.text.y=element_blank())  + xlab("Trend")+ geom_vline(xintercept=0) + ylab("") + theme(axis.text.x=element_text(size=10),axis.title.x=element_text(size=10))

### add a bar chart of  mean annual releases

mean.har = aggregate(cbind(releases,biomass)~Species+ESU,data=ts,FUN="mean")

mean.har = merge(mean.har,stone,by.x = "ESU",by.y = "DPS_me",all.y=T)

mean.har$col = NA
mean.har$col = ifelse(mean.har$Species.y=="Chinook",cs[1],mean.har$col)
mean.har$col = ifelse(mean.har$Species.y=="Chum",cs[2],mean.har$col)
mean.har$col = ifelse(mean.har$Species.y=="Coho",cs[3],mean.har$col)
mean.har$col = ifelse(mean.har$Species.y=="Pink",cs[4],mean.har$col)
mean.har$col = ifelse(mean.har$Species.y=="Sockeye",cs[5],mean.har$col)
mean.har$col = ifelse(mean.har$Species.y=="Steelhead",cs[6],mean.har$col)

mean.har$org = paste(mean.har$Status,mean.har$Species.y,mean.har$DPS_short,sep=":")
mean.har = mean.har[order(mean.har$Species.y,mean.har$Status,mean.har$DPS_short),]
mean.har$org = factor(mean.har$org,ordered = T)
mean.har$org
mean.har
mean.har$ltype = ifelse(mean.har$Status=="U","11","solid")

har.plot = ggplot(data=mean.har,aes(x=0,xend=(releases/1e6),y=fct_inorder(org),yend=fct_inorder(org))) + geom_segment(color=mean.har$col,linewidth=2,linetype=mean.har$ltype) + theme_minimal_grid()  + theme(axis.text.y=element_text(hjust=0,color=lab.colors$col),legend.position="none") + xlab("Releases, millions") + ylab("") + theme(axis.text.x=element_text(size=10),axis.title.x=element_text(size=10))


pdf("combo hatchery release plot means and trends 12.16.24.b.pdf",height=8,width=10)
plot_grid(har.plot,trend.plot,ncol=2,rel_widths = c(2/3,1/3))
dev.off()


#### look relationship between releases and abundance -- Figure S19

# loaded the fitted aggregated abundance data 
abn = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits/fitted_data_sums_12.12.24.csv")

abn = merge(abn,rel,by.x=c("SPECIES","ESU","year"),by.y=c("Species","ESU","Year"))

ave = aggregate(cbind(releases,biomass,tot.Median)~SPECIES+ESU,data=abn,FUN=mean)
head(ave)

p = ggplot(data=subset(ave,SPECIES != "Pink"),aes(x=releases,y=tot.Median)) + geom_point() + geom_smooth(method=lm) + facet_wrap(~SPECIES,scales="free")

pdf("SI Figure plot of rel v total abundance by ESU_12.16.24.pdf",height=8,width= 10)
print(p)
dev.off()

# model fits for relationship between releases and abundance

 chinook.m = lm(tot.Median~releases,data=subset(ave,SPECIES=="Chinook"))
summary(chinook.m)

chum.m = lm(tot.Median~releases,data=subset(ave,SPECIES=="Chum"))
summary(chum.m)

coho.m = lm(tot.Median~releases,data=subset(ave,SPECIES=="Coho"))
summary(coho.m)

sockeye.m = lm(tot.Median~releases,data=subset(ave,SPECIES=="Sockeye"))
summary(sockeye.m)

steelhead.m = lm(tot.Median~releases,data=subset(ave,SPECIES=="Steelhead"))
summary(steelhead.m)
