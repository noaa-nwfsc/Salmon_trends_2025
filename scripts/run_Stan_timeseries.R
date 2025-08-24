library(rstan)
library(tidyverse)
library(reshape2)
library(ggplot2)
# This reads in and runs a Bayesian version of the MARSS model.  Modified a bit from script from Ole Shelton for OC and SONCC Chinook

# add indicies for year and population,
pop <- dat.ts %>% distinct(COMMON_POPULATION_NAME) %>% rename(pop=COMMON_POPULATION_NAME )
pop$pop_idx <- 1:nrow(pop)
N_pop <- nrow(pop)
N_pop
pop


min.year = dat.ts %>% rename(year = BROOD_YEAR) %>% summarise(min(year)) %>% pull()
max.year = dat.ts %>% rename(year = BROOD_YEAR) %>% summarise(max(year)) %>% pull()
year <- data.frame(year=min.year:max.year)
year$year_idx <- 1:nrow(year)
N_year <- nrow(year)
year


dat.mod <- dat.ts %>% rename(pop=COMMON_POPULATION_NAME,year=BROOD_YEAR) %>%
              left_join(.,pop) %>% left_join(.,year) %>%
              filter(NUMBER_OF_SPAWNERS >=0) %>% 
              mutate(FRACWILD = ifelse(FRACWILD < 0,NA,FRACWILD))
head(dat.mod)

# Define data for reading into Stan
stan_data <- list(
  N_pop = N_pop,   # Number of populations included.
  N_year = N_year, # Number of years of data occurring in at least one population
  N_obs = nrow(dat.mod), # total number of observations.
  
  # Observations
  spawners = dat.mod$NUMBER_OF_SPAWNERS,
  
  # Indexes for individual crab observations
  pop_idx = dat.mod$pop_idx,
  year_idx = dat.mod$year_idx,
  sigma2_R_prior =sigma2_R_prior
)


stan_pars <- c(
  # state parameters
  "log_X",  # of size N_pop x N_year
  "log_X0", # initial state (of length N_pop)
  # 
  "mu", # initial state (of length N_pop)
  # 
  "sigma2_R", # observation variance (diagonal)
  "sigma2_Q", # process variance (log-scale)
  "theta",  # process correclation (log-scale)
  #"rho",
  
  #"nu",      # realization of observation error     
  "epsilon", # realization of process error
  # 
  "pred_log_X", # predictions, log-space
  "pred_X"      # predictions, regular space.
)

###################################################
# N_CHAIN = 5
# Warm = 1000
# Iter = 2000
# Treedepth = 12
# Adapt_delta = 0.99

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


stanMod = stan(file = NOM  ,data = stan_data,
               verbose = FALSE, chains = N_CHAIN, thin = 1,
               warmup = Warm, iter = Warm + Iter,
               control = list(#adapt_init_buffer = 175,
                 max_treedepth=Treedepth,
                 #stepsize=0.01,
                 #adapt_delta=Adapt_delta,
                 metric="diag_e"),
               pars = stan_pars,
               #refresh = 10,
               boost_lib = NULL,
               #init = stan_init_f2(n.chain=N_CHAIN,N_species=N_species),
               sample_file = paste0("./tmpB.csv")
)

pars <- rstan::extract(stanMod, permuted = TRUE)
samp_params <- get_sampler_params(stanMod)
stanMod_summary <- list()

stanMod_summary[["All"]] <- summary(stanMod)$summary


# Saving the posterior estimates for parameters
stanMod_summary[["main_param"]] <- summary(stanMod,pars=c("mu","sigma2_R","sigma2_Q","theta"))$summary
stanMod_summary[["pred_log_X"]] <- summary(stanMod,pars="pred_log_X")$summary
stanMod_summary[["pred_X"]] <- summary(stanMod,pars="pred_X")$summary
#stanMod_summary[["pred_X_mean"]] <- summary(stanMod,pars="pred_X_mean")$summary
# traceplot(stanMod,pars=c("mu","sigma2_R","sigma2_Q","theta"))
# traceplot(stanMod,pars="log_X0")
# Making abundance predictions

stanMod_summary[["log_X"]] <- summary(stanMod,pars="log_X")$summary
dimnames(pars$log_X)[[2]] <- year$year
dimnames(pars$log_X)[[3]] <- pop$pop

log_X_long_samp <- melt(pars$log_X)
colnames(log_X_long_samp)[2] <- "year"
colnames(log_X_long_samp)[3] <- "population"
log_X_long_samp$group = GROUP
head(log_X_long_samp)
nrow(log_X_long_samp)

sigma2_R <- pars$sigma2_R %>% as.data.frame() %>% rename(sigma2_R =".") %>% mutate(iterations=1:length(sigma2_R))
log_X_long_samp <- log_X_long_samp %>% left_join(.,sigma2_R) %>%
                      mutate(pred_X = exp(value - 0.5 * sigma2_R))

log_X_long_summ <- log_X_long_samp %>% 
                      group_by(year,population,group) %>%
                      summarise(Mean=mean(pred_X),SD=sd(pred_X),Median=median(pred_X),
                                X.025 = quantile(pred_X,probs=c(0.025)),
                                X.975 = quantile(pred_X,probs=c(0.975)),
                                X.05 = quantile(pred_X,probs=c(0.05)),
                                X.95 = quantile(pred_X,probs=c(0.95))) %>%
                      left_join(.,dat.mod %>% dplyr::select(year,population=pop,FRACWILD,obs_spawners=NUMBER_OF_SPAWNERS)) %>%
                      mutate(nat.Mean = FRACWILD * Mean,
                             nat.X.025 =FRACWILD * X.025, 
                            nat.X.975 =FRACWILD * X.975,  
                            nat.X.05 =FRACWILD * X.05,  
                            nat.X.95 =FRACWILD * X.95)
head(log_X_long_summ)

log_X_long_samp2 <- log_X_long_samp

log_X_long_summ_agg <- log_X_long_samp2 %>% 
                          left_join(.,dat.mod %>% dplyr::select(year,population=pop,FRACWILD,obs_spawners=NUMBER_OF_SPAWNERS)) %>%
                          mutate(nat.pred_X=pred_X*FRACWILD) %>%
                          group_by(iterations,year) %>%
                          summarise(tot.sum = sum(pred_X),
                                    nat.sum = sum(nat.pred_X),
                                    raw.sum = sum(obs_spawners)) %>%
                          group_by(year) %>%
                          summarise(tot.Mean = mean(tot.sum),tot.Median=median(tot.sum),
                                    tot.X.025 = quantile(tot.sum,probs=c(0.025)),
                                    tot.X.975 = quantile(tot.sum,probs=c(0.975)),
                                    tot.X.05 = quantile(tot.sum,probs=c(0.05)),
                                    tot.X.95 = quantile(tot.sum,probs=c(0.95)),
                                    nat.Mean = mean(nat.sum,na.rm=T),
                                    nat.X.025 = quantile(nat.sum,probs=c(0.025),na.rm=T),
                                    nat.X.975 = quantile(nat.sum,probs=c(0.975),na.rm=T),
                                    nat.X.05 = quantile(nat.sum,probs=c(0.05),na.rm=T),
                                    nat.X.95 = quantile(nat.sum,probs=c(0.95),na.rm=T),
                                    obs.spawn = mean(raw.sum))



### DERIVE SUMMARY STATISTICS FROM posterior distributions.
# 15 year trend and 5 year geomeans.

all_geomean <- NULL
for(i in 1:nrow(geomean.table.control)){
  min.year = geomean.table.control$start.year[i]
  max.year = geomean.table.control$stop.year[i]
  
  temp <- log_X_long_summ %>% filter(year>=min.year,year<=max.year)
  geomean1 = temp %>% filter(is.na(Mean)==F) %>% group_by(population,group) %>%
                summarise(tot.mean=exp(mean(log(Mean),na.rm=TRUE)),
                          nat.mean=exp(mean(log(nat.Mean),na.rm=TRUE)),
                          obs.mean=exp(mean(log(obs_spawners),rm.na=TRUE))) %>%
                mutate(start.year=min.year,stop.year=max.year)

  all_geomean <- rbind(all_geomean,geomean1)
}


all_15_trend <- NULL
for(i in 1:nrow(trend.table.control)){
  min.year = trend.table.control$start.year[i]
  max.year = trend.table.control$stop.year[i]
  
  for(j in 1:N_pop){
  temp <- log_X_long_summ %>% filter(year>=min.year,year<=max.year,population == pop$pop[j])
  
  # total spawners
  mod <- lm(log(Mean) ~ year,data=temp)
  temp.fit <- data.frame(population = pop$pop[j],
                         type="Total",
                         start.year = min.year, 
                         stop.year = max.year,
                         trend.mean = summary(mod)$coefficients["year",c("Estimate")],
                         trend.se = summary(mod)$coefficients["year",c("Std. Error")])
  # natural spawners
  if(GROUP != "SONC_Spring"){
    if(temp %>% filter(!is.na(nat.Mean)) %>% nrow(.)>=5){
    
    mod <- lm(log(nat.Mean) ~ year,data=temp)
    temp.fit2 <- data.frame(population = pop$pop[j],
                         type="Natural",
                         start.year = min.year, 
                         stop.year = max.year,
                         trend.mean = summary(mod)$coefficients["year",c("Estimate")],
                         trend.se = summary(mod)$coefficients["year",c("Std. Error")])
  
    temp.fit <- rbind(temp.fit,temp.fit2)
    }
  }
    all_15_trend = rbind(all_15_trend,temp.fit)
  
  }
}

## Save model output as a list()
log_X_long_summ_agg$ESU = log_X_long_summ$ESU = all_15_trend$ESU = all_geomean$ESU =  GROUP

Output_full <- list(
  dat.mod = dat.mod, # data used for model fitting
  pars = pars, # MCMC output
  samp_params = samp_params, # sampler information,
  stanMod_summary = stanMod_summary, #  parameter summaries
  log_X_long_samp = log_X_long_samp,
  log_X_long_summ_agg = log_X_long_summ_agg, # predicted abundances for each population, long form MCMC samples.
  log_X_long_summ = log_X_long_summ, # summaries of predicted abundances.
  all_15_trend = all_15_trend,
  geomean_5yr = all_geomean
)




Output <- list(
  stanMod_summary = stanMod_summary, #  parameter summaries
  log_X_long_summ_agg = log_X_long_summ_agg, # predicted abundances for each population, long form MCMC samples.
  log_X_long_summ = log_X_long_summ, # summaries of predicted abundances.
  all_15_trend = all_15_trend,
  geomean_5yr = all_geomean
)



save(Output_full,file=paste0(outpath,GROUP,"_fitted.RData"))
#save(Output_full,file=paste0("../Data/Fitted_timeseries/",GROUP,"_model_and_fitted2.RData"))

write.csv(Output[[2]],paste0(outpath,GROUP,"_agg.csv"))
write.csv(Output[[3]],paste0(outpath,GROUP,"_pop.csv"))
write.csv(Output[[4]],paste0(outpath,GROUP,"_poptrend.csv"))
write.csv(Output[[5]],paste0(outpath,GROUP,"_popgeo.csv"))
write.csv(stanMod_summary[[2]],paste0(outpath,GROUP,"_stansum.csv"))


# base = ggplot(data=Output[[3]],aes(x=year,y=obs_spawners)) + geom_point() + geom_line(data=Output[[3]],aes(y=Mean))
# plot = base + facet_wrap(~population,ncol=1,scales="free")
# 
# plot


