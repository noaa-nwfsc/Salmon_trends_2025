data {
  int<lower=0> N_pop ; // Number of surveys included.
  int<lower=0> N_year ;
  int<lower=0> N_obs ; // total number of observations.

  // Observations
  vector<lower=0>[N_obs] spawners;
  
  // Indexes for individual spawner observations
  int pop_idx[N_obs];
  int year_idx[N_obs];
  real sigma2_R_prior[2];
}
parameters {
   vector[N_pop] mu; // mean pop growth rate (log-scale)
   real<lower=0> sigma2_R ; // diagonal observation variance
   real<lower=0> sigma2_Q ; // shared single process variance
   real<lower=-1,upper=1> theta ; // correlation among stocks, 
   //vector<lower=-1,upper=1>[N_pop] rho;
   vector[N_pop] log_X0;
   vector[N_pop] epsilon[N_year]; // realization of process error
}
transformed parameters{
  vector[N_pop] log_X[N_year]; // latent state 
  matrix[N_pop,N_pop] Sigma;
  
  for(i in 1:N_pop){
    for(j in 1:N_pop){
    Sigma[i,j] = theta * sigma2_Q ;
    }
    Sigma[i,i] = sigma2_Q;
  }

  
  // print("Sigma ;", Sigma);
  // print("log_X0 ",log_X0);
  // print("mu ",mu);
  // print("epsilon ",epsilon[1]);
  
  //epsilon = -0.5*pow(tau,2) + epsilon_raw * tau ;
  // Update states
  for(i in 1:N_year){
    if(i==1){
      log_X[i] = log_X0 + mu + epsilon[i];
    }
    if(i>1){
      log_X[i] = log_X[i-1] + mu + epsilon[i];
    }
  }
 
 
 //print("log_X ",log_X[35]);
  
}

model {
    //likelihood
    for (i in 1:N_obs){
      spawners[i] ~ lognormal(log_X[year_idx[i],pop_idx[i]] - 0.5*sigma2_R, sqrt(sigma2_R));  
    }
    //print(spawners[1]," pred:",exp(log_X[year_idx[1],pop_idx[1]]));
    
    //priors
    mu ~ normal(0,1);  // normal prior on the decay 
    sigma2_R ~ gamma(sigma2_R_prior[1], sigma2_R_prior[2]);
    sigma2_Q ~ gamma(1, 1);
    theta ~ uniform(-1,1);
    
    // Process errors:
    for(i in 1:N_year){
      epsilon[i] ~ multi_normal(rep_vector(0,N_pop),Sigma) ;
    }
    // Prior on initial spawner abundance
        log_X0 ~ normal(7,5) ;
}

generated quantities{
  vector[N_obs] pred_log_X;
  vector[N_obs] pred_X;
 
  
  for (i in 1:N_obs){
      pred_log_X[i]  = log_X[year_idx[i],pop_idx[i]] ;
      pred_X[i] = lognormal_rng(log_X[year_idx[i],pop_idx[i]] - 0.5*sigma2_R,
                   sqrt(sigma2_R)) ;
  }
 
}

