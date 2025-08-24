##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024


#### script for compiling all of the MARSS output into two files, one with abundance estimates per population and one with the aggregated abundance per DPS


library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(perm)
library(gridExtra)

#### load data -- fitted
# set the working directory to the location of the model output


setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/model_fits")

# get the relevant output files

f = list.files()
f
f = f[grep("_pop.csv",f)]
f
ts = vector(mode='list',length=length(f))
for(i in 1:length(f))
{
  ts[[i]]=read.csv(f[i],stringsAsFactors = F)
}
length(ts)
row.names(ts[[1]])
ts = do.call(rbind,ts)

head(ts)

# add a "species" column
ts$SPECIES = NA
ts$SPECIES[grep("Chinook",ts$ESU)] = "Chinook" 
ts$SPECIES[grep("Steelhead",ts$ESU)] = "Steelhead"
ts$SPECIES[grep("steelhead",ts$ESU)] = "Steelhead"
ts$SPECIES[grep("Coho",ts$ESU)] = "Coho"
ts$SPECIES[grep("coho",ts$ESU)] = "Coho"
ts$SPECIES[grep("Sockeye",ts$ESU)] = "Sockeye"
ts$SPECIES[grep("Chum",ts$ESU)] = "Chum"
ts$SPECIES[grep("Pink",ts$ESU)] = "Pink"

table(ts$SPECIES,useNA="ifany")

# same thing, but now load the aggregated (summed) abundance

f = list.files()
f
f = f[grep("_agg.csv",f)]
f
ts.agg = vector(mode='list',length=length(f))
for(i in 1:length(f))
{
  ts.agg[[i]]=read.csv(f[i],stringsAsFactors = F)
}
length(ts.agg)
ts.agg = do.call(rbind,ts.agg)

head(ts.agg)

# add a "species" column

ts.agg$SPECIES = NA
ts.agg$SPECIES[grep("Chinook",ts.agg$ESU)] = "Chinook" 
ts.agg$SPECIES[grep("Steelhead",ts.agg$ESU)] = "Steelhead"
ts.agg$SPECIES[grep("steelhead",ts.agg$ESU)] = "Steelhead"
ts.agg$SPECIES[grep("Coho",ts.agg$ESU)] = "Coho"
ts.agg$SPECIES[grep("coho",ts.agg$ESU)] = "Coho"
ts.agg$SPECIES[grep("Sockeye",ts.agg$ESU)] = "Sockeye"
ts.agg$SPECIES[grep("Chum",ts.agg$ESU)] = "Chum"
ts.agg$SPECIES[grep("Pink",ts.agg$ESU)] = "Pink"


table(ts.agg$SPECIES,useNA="ifany")

# correct the year values for pink salmon, which were just 1..n to deal with the 2-year gen time
head(ts)
p = subset(ts,SPECIES=="Pink")
head(p)
p$year = p$year - 1
p$year = p$year*2
p$year = p$year + 1995
ts = subset(ts,SPECIES != "Pink")
ts = rbind(ts,p)

head(ts.agg)
p.agg = subset(ts.agg,SPECIES=="Pink")
p.agg$year = p.agg$year - 1
p.agg$year = p.agg$year*2
p.agg$year = p.agg$year + 1995
ts.agg = subset(ts.agg,SPECIES != "Pink")
ts.agg = rbind(ts.agg,p.agg)
#####

write.csv(ts,"fitted_data_12.12.24.csv")
write.csv(ts.agg,"fitted_data_sums_12.12.24.csv")

####################################

esus = unique(ts$ESU)
esus
esustorun = esus
esustorun

# make abundance plots for each population - one page per ESU 
head(ts)
for(i in 1:length(esustorun))
{
  temp = subset(ts,ESU==esustorun[i])
  base = ggplot(data=temp,aes(x=year,y=obs_spawners)) + geom_point() + geom_line(data=temp,aes(y=Mean)) +  geom_ribbon(data=temp,aes(ymin=X.05,ymax=X.95),alpha=.2) + geom_line(data=temp,aes(y=Median),color="blue") + theme_light()
  plot = base + facet_wrap(~population,labeller=label_wrap_gen(width=25),scales="free") + theme(text=element_text(size=11))
  
  pdf(paste(esustorun[i],"_pop_plots_fitted_12.12.24.pdf"),width=11,height=8.5)
  print(plot)
  dev.off()
}


# plot the aggregated DPS abundance - one page per species - Figures S3 - S8

sp.list = c("Chinook", "Coho", "Steelhead", "Chum", "Sockeye", "Pink")

for(i in 1:length(sp.list))
{
  temp = subset(ts.agg,SPECIES==sp.list[i])
  base = ggplot(data=temp,aes(x=year,y=tot.Median)) + geom_line(color="blue") +  geom_ribbon(data=temp,aes(ymin=tot.X.05,ymax=tot.X.95),alpha=.2) 
  plot = base + facet_wrap(~ESU,labeller=label_wrap_gen(width=25),scales="free")+ theme(text=element_text(size=11))
  
  pdf(paste(sp.list[i],"_agg_plots_fitted_12.12.24.pdf"),width=11,height=8.5)
  print(plot)
  dev.off()
}

# plot the aggregated abundance, all in one plot -- Figure 5
# get naming convention file, locating in the 'input' folder--adjust path as needed.
stone = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/DPS_name_conv_table_with_numbers.csv",stringsAsFactors = F)
head(stone)
ts.agg = merge(ts.agg,stone,by.x="ESU",by.y = "DPS_me")
head(ts.agg)

ts.agg$Status2 = ifelse(ts.agg$Status=="U",ts.agg$Status,"T/E")
ts.agg$Status3 = ts.agg$Status
ts.agg$Status = ts.agg$Status2
ts.agg$DPS = ts.agg$DPS_short

# standardize the agg abundance by the mean within each ESU
means = aggregate(tot.Median~ESU,data=ts.agg,FUN="mean")
names(means) = c("ESU","tot.Median.mean")
ts.agg = merge(ts.agg,means,by = "ESU")
head(ts.agg)

ts.agg$std = ts.agg$tot.Median/ts.agg$tot.Median.mean
####

ts.aggnop = subset(ts.agg,SPECIES != "Pink") # optional - get rid of pink


#### make one plot per species, group by status


sp.list = c("Chinook", "Coho", "Steelhead", "Chum", "Sockeye")
for(i in 1:length(sp.list))
{
  
temp = subset(ts.agg,SPECIES == sp.list[i])

plot[[i]] = ggplot(data = temp,aes(x=year,y=std)) + geom_line(data=temp, aes(x=year,y=std,color=Status,group=ESU),alpha=.8,show.legend = F) + geom_smooth(data = temp,aes(x=year,y=std,group=Status,color=Status,fill=Status),linewidth=2,span=.3,show.legend = F) + scale_color_manual(values=c("darkgoldenrod2","deepskyblue1"))+scale_fill_manual(values=c("darkgoldenrod2","deepskyblue1")) + theme_minimal() + ylab("") + xlab("") + ggtitle(sp.list[i])

}

## get PDO data - located in 'input folder' - adjust path as needed
pdo = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/pdo.csv",stringsAsFactors = F)
head(pdo)
pdo = subset(pdo,year>=1995 & year <=2020)
pdo$col = ifelse(pdo$may_sep<0,"royalblue3","tomato3")
pdo
pdoplot = ggplot(data=pdo,aes(xend=year,x=year,y=0,yend=may_sep)) + geom_segment(color=pdo$col,linewidth=2) +  theme_minimal() + ylab("PDO")



pdf("agg_plot_all_species_withpdo_12.12.24.pdf",width=6,height=10)
plot_grid(plot[[1]],plot[[2]],plot[[3]],plot[[4]],plot[[5]],pdoplot,ncol=1)
dev.off()

pdf("agg_plot_all_species_no_env_1.16.25.pdf",width=6,height=10)
templot = plot_grid(plot[[1]],plot[[2]],plot[[3]],plot[[4]],plot[[5]],ncol=1)
finalplot = ggdraw(templot) + draw_label("Year",x=.5,y=0,vjust=-.30,hjust=.5,size=14) + draw_label("Relative abundance",x=.025,y=.5,angle=90,vjust=0,hjust=0,size=14)
finalplot
dev.off()

### get SST data
sst  = read.csv("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive/input_files/NEPSST_arc 1995-2020.csv",stringsAsFactors = F)

sst
sst$col = ifelse(sst$SST_arc<0,"royalblue3","tomato3")
sst
sstplot = ggplot(data=sst,aes(xend=year,x=year,y=0,yend=SST_arc)) + geom_segment(color=sst$col,linewidth=2) +  theme_minimal() + ylab("SSTarc")
sstplot


pdf("agg_plot_all_species_withsst_1.15.25.pdf",width=6,height=10)
plot_grid(plot[[1]],plot[[2]],plot[[3]],plot[[4]],plot[[5]],sstplot,ncol=1)
dev.off()






