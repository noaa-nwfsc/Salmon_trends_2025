##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024


### code to make salmon harvest plots -- Figure 6 and Figures S9-S13

library(ggplot2)
library(openxlsx)
library(tesseract)
library(stringr)
library(cowplot)
library(RColorBrewer)
library(forcats)
library(gridExtra)

### load the data file
# set the correct directory, where plots will be stored

setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive")

# read in data file - adjust path as necessary
harvest = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/compiled harvest data 12.11.24_with_citations.csv",stringsAsFactors = F) 

#### make individual DPS summary plots (SI Figures)


means.har = aggregate(harvest$ER,list(harvest$Species,harvest$ESU,harvest$Year),FUN=mean,na.rm=T)
head(means.har)
means.har$Stock = "mean"
names(means.har) = c("Species","ESU","Year","ER","Stock")
head(means.har)

sp.list = c("Chinook", "Coho", "Steelhead", "Sockeye","Chum")


for (i in 1:length(sp.list))
{
  sp = sp.list[i]
  ts.sp = subset(harvest,Species==sp)
  means.sp = subset(means.har,Species==sp)
  
  base = ggplot(data=ts.sp,aes(x=Year,y=ER,group=Stock)) + geom_line(col="gray",alpha=.7) + theme_light() + geom_line(data=means.sp,col='red',size=2,alpha=.7) + scale_y_continuous(trans='identity',labels = function(x) format(x, scientific = FALSE))  + ylab("Harvest rate") + xlab("Year") + xlim(1995,2020)
  
  plot = base + facet_wrap(~ESU,labeller=label_wrap_gen(width=25)) + theme(text=element_text(size=18))
  
  pdf(paste(sp,"_ERplot_dec16.24.pdf"),width=11,height=8.5)
  print(plot)
  dev.off()
}

#### generate trends and overall summary plot (Figure 6)

ts = harvest
ts$pop = ts$Stock

head(ts)
ts$index = paste(ts$Species,ts$ESU,ts$pop,sep="_")
nrow(ts)
ts = subset(ts,Year >= 1995 & Year <= 2020)
nrow(ts)
ts$total.raw = ts$ER

tmp = by(ts,list(ts$index),function(x) lm(total.raw~Year,data=x))
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

## and of the mean for each DPS


sums.raw = means.har
head(sums.raw)
sums.raw = subset(sums.raw,Year>=1995 & Year <=2020)

sums.raw$index = paste(sums.raw$Species,sums.raw$ESU,sep="_")
sums.raw$index
sums.raw$total.raw = sums.raw$ER



tmp = by(sums.raw,list(sums.raw$index),function(x) lm(total.raw~Year,data=x))

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

# load the naming convention file
stone = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/DPS_name_conv_table_with_numbers.csv",stringsAsFactors = F)
stone

head(stone)
trends.sums

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

mean.plot = ggplot(data=trends.sums,aes(x=trend,y=fct_inorder(org))) + geom_point(size=2,color=trends.sums$col,shape=trends.sums$shape) + geom_errorbarh(aes(xmin=trend-1.96*stderr,xmax=trend+1.96*stderr),color=trends.sums$col)  + theme_minimal_grid() + theme(axis.text.y=element_blank())  + xlab("Harvest rate, trend")+ geom_vline(xintercept=0) + ylab("")

final.plot = mean.plot + geom_point(data=trends,aes(x=trend,y=fct_inorder(org)),alpha=.5,color=trends$col,shape=trends$shape)  


##### make a bar chart of average harvest
head(sums.raw)
sums.raw$Year
mean.har = aggregate(ER~Species+ESU,data=sums.raw,FUN="mean")
mean.har
mean.har = merge(mean.har,stone,by.x = "ESU",by.y = "DPS_me",all.y=T)
head(mean.har)

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
mean.har$footnote = ifelse(is.na(mean.har$ER),"ND","")
mean.har

mean.har$ltype = ifelse(mean.har$Status=="U","11","solid")

har.plot = ggplot(data=mean.har,aes(x=0,xend=ER,y=fct_inorder(org),yend=fct_inorder(org))) + geom_segment(color=mean.har$col,linewidth=2,linetype=mean.har$ltype) + theme_minimal_grid()  + theme(axis.text.y=element_text(hjust=0,color=lab.colors$col),legend.position="none") + xlab("Harvest rate, mean") + ylab("") + geom_text(label = mean.har$footnote,color=mean.har$col)
har.plot
final.plot

pdf("combo harvest plot means and trends 12.16.24.pdf",height=8,width=10)
plot_grid(har.plot,final.plot,ncol=2,rel_widths = c(2/3,1/3))
dev.off()


## make tables
write.csv(trends.sums,"harvest trends 1995 2020 12.16.24.csv")

write.csv(mean.har,"harvest means 1995 2020 12.16.24.csv")




