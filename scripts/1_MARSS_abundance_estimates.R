##### Ford MJ et al. Abundance trends of Pacific salmon during a quarter century of ESA protection.  2024


#### Script running a MARSS-like model for salmon abundance data.  

library(tidyverse)
library(stringr)


#################################################
## SETUP FOR RUNNING MARSS MODEL WRITTEN BY OLE SHELTON
#################################################


# load input data.  Set the working directory to the base folder

setwd("/Users/mike.ford/Documents/MikeFord/Recovery planning/5 year status reviews/2020 five year status update/salmon status paper 2022/code_and_data_for_archive")

bigdata = read.csv("input_files/PNW_and_CA_abundance_8.6.24_with_citations_trimmed.csv",na.strings = c("NA","-99"))

# change any FRACWILD = 0 to NA and NUMBEROFSPAWNER = 0 to NA
bigdata$FRACWILD = ifelse(bigdata$FRACWILD == 0, NA, bigdata$FRACWILD)
bigdata$NUMBER_OF_SPAWNERS = ifelse(bigdata$NUMBER_OF_SPAWNERS == 0, NA, bigdata$NUMBER_OF_SPAWNERS)
# get rid of '/' in ESU names
bigdata$ESU = str_replace_all(bigdata$ESU,"/","-")

# create a unique identifier
bigdata$u.name = paste(bigdata$ESU,bigdata$COMMON_POPULATION_NAME,sep="_")

# total list of ESU (DPS) prior to any filtering
prefilter = unique(bigdata$ESU)
prefilter

# do some data prunning.  limit to 1995 to 2020.  get rid of populations with insufficient data
head(bigdata)
nrow(bigdata)
bigdata = subset(bigdata,BROOD_YEAR>=1995 & BROOD_YEAR <=2020)
nrow(bigdata)
bigdata$missing = ifelse(is.na(bigdata$NUMBER_OF_SPAWNERS),"yes","no")

bigdata_noNA = subset(bigdata,is.na(NUMBER_OF_SPAWNERS)==F)
nrow(bigdata_noNA)
pops_nalt = table(bigdata$u.name,bigdata$missing)
head(pops_nalt)
pops_nalt = as.data.frame(pops_nalt,stringsAsFactors = F)
names(pops_nalt) = c("u.name","not_missing","n_data")
lowdatapopsalt = subset(pops_nalt,n_data<12 & not_missing=="no")$u.name
lowdatapopsalt

bigdata = subset(bigdata,u.name %in% lowdatapopsalt == F)
nrow(bigdata)
summary(bigdata)
unique(bigdata$ESU)


# path for output folder
outpath = "model_fits/"

# get list of ESU and select the ones to run
esus = unique(bigdata$ESU)
esus
prefilter[prefilter %in% esus == F] # these are the ESUs that were dropped due to missing data

esustorun = esus   ### select what you want here
esustorun = esus[c(1)]
esustorun

##### for pink salmon only, need to change from annual data to generational data - only do this for pink salmon, which need to be run separately####
bigdata = subset(bigdata,ESU %in% esustorun==T)
head(bigdata)
table(bigdata$BROOD_YEAR)
bigdata$gen = bigdata$BROOD_YEAR-1995
bigdata$gen = bigdata$gen/2
bigdata$gen = bigdata$gen + 1
table(bigdata$gen)
bigdata$BROOD_YEAR = bigdata$gen
############################################################


### set up time periods for calculations.  
# geomean calculations
lenbands =5
min.year = 1995
max.year = 2020
min.band.points= 2

geomean.table.control = data.frame(start.year = seq(min.year,max.year,by=lenbands)) %>%
  mutate(stop.year = start.year + lenbands-1) %>% 
  filter(stop.year <= max.year)
start.stop.pairs = c(1995,2020)

trend.table.control = as.data.frame(matrix(start.stop.pairs,ncol=2,byrow = T))
colnames(trend.table.control) <- c("start.year","stop.year")
trend.table.control$ID <- 1:nrow(trend.table.control)

### loop over ESUs and call the model fitting script

for(i in 1:length(esustorun))
{
  print(esustorun[i])
  dat.ts = subset(bigdata,ESU == esustorun[i])
  dat.ts = subset(dat.ts,BROOD_YEAR >=min.year & BROOD_YEAR <=max.year)
  GROUP = esustorun[i]

  # define observation error prior
  sigma2_R_prior = c(1,1)
  #define which Stan model to run.
  NOM <- "scripts/time_series_stan.stan"
  #sampler details
  N_CHAIN = 5
  Warm = 2000
  Iter = 10000
  Treedepth = 12
  Adapt_delta = 0.8
  
  source("scripts/run_Stan_timeseries.R")
}



 