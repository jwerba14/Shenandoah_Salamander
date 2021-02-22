## simulate temperatures (200 sites, 10 years, spatially correlated with small change over time)
library(lme4)
library(rjags)
library(tidyverse)

## 200 sites, 5 observations per site each for 10 years
expdat <- expand.grid(sites = factor(1:200), obs = factor(1:5), year = seq(1,10, by =1))

expdat$obs <- factor(seq(nrow(expdat)))

#Parameters: baseline range , effect of treatment is to decrease log-odds of range by 0.4 (approx 0.4/4 = 10%).
#Random effects standard deviations of individual and observation are both 1.0 (on the same scale as the fixed-effect parameters).

set.seed(101)
nsim <- 10
beta <- c(20, 0.2)
theta <- c(0.1)
sigma <- 0.1

ss <- simulate(~ year + (1 | sites) , nsim = nsim,  family = gaussian,
               weights = rep(25, nrow(expdat)), newdata = expdat, newparams = list(theta = theta, 
                                                                                   beta = beta,
                                                                                   sigma = sigma))



ss$year <- expdat$year
ss$site <- expdat$site

## simulate occurrence data based on temperature
## dataframe of effect sizes
effect_size_temp <- .5

## sim cov
ss1 <- ss %>% 
  dplyr::select(sim_1,site, year) %>% 
  group_by(year, site) %>% 
  summarize(norm_temp = (sim_1 - mean(sim_1))/sd(sim_1))  ## normalize temperature

ss1$sample <- rep(seq(1,5, by =1), 2000, each = TRUE)

ss2 <- ss1 %>% dplyr::arrange(year,sample, site)

## set up temp in array like occupancy 


TEMP <- array(data = ss2$norm_temp, dim = c(200,5,10))
  
## set up empty dataframe to simulate occupancy data
sp1_occ <- array(data = NA, dim= c(200,5,10)) 

sp1_occ[, ,] <- as.numeric(sp1_occ[, , ])

#bundle data
dat_sim <-  list(
  site = dim(sp1_occ)[1],
  TEMP = TEMP,
  year = dim(sp1_occ)[3],
  obs = dim(sp1_occ)[2],
  y = sp1_occ
  
)


## simulate occupancy 
library(R2jags)
params <- c("a", "b1", "y")
sim_occ <- jags.model(data = dat_sim,  
                     #parameters.to.save = params, 
                     file = "temp_over_time.txt", 
                     n.chains = 3, 
                     #n.iter = 10000, 
                     #n.thin = 10,
                     n.adapt = 1000
                )

samp_occ <- coda.samples(sim_occ, variable.names = params, n.iter = 5000, thin = 1)

## estimate occupancy based on annual temp
#newdat <- samp_occ[[1]][1,-c(1,2)]
#nd1 <- array(data = names(newdat), dim = c(200,5,10))
#nd1 <- array(data = newdat, dim = c(200,5,10))

## want to loop over a hundred random draws and estimate a, b1 and coverage?? 
set.seed(101)
tt <- sample(seq(1, 100, by = 1))

for(i in 1:length(tt)){
  newdat <- samp_occ[[1]][tt[i],-c(1,2)]
  nd1 <- array(data = newdat, dim = c(200,5,10))
  
  dat_est1 <-  list(
    site = dim(sp1_occ)[1],
    TEMP = TEMP,
    year = dim(sp1_occ)[3],
    obs = dim(sp1_occ)[2],
    y = nd1
    
  )
  
  est_occ_temp <- jags.model(data = dat_est1,  
                             #parameters.to.save = params, 
                             file = "temp_over_time_est.txt", 
                             n.chains = 3, 
                             #n.iter = 10000, 
                             #n.thin = 10,
                             n.adapt = 1000 )
  
  
  
  samp_est <- coda.samples(est_occ_temp, variable.names = params, n.iter = 5000, thin = 1)
  
  if(i == 1){
    out <- data.frame(
      sample = 1,
      a = median(samp_est[[1]][,1]),
      a_upr = quantile(samp_est[[1]][,1], 0.975),
      a_lwr = quantile(samp_est[[1]][,1], 0.025),
      b1 = median(samp_est[[1]][,2]),
      b1_upr = quantile(samp_est[[1]][,2], 0.975),
      b1_lwr = quantile(samp_est[[1]][,2], 0.025)
    )
    
  } else {
    
    tdf <- data.frame(
      sample = i,
      a = median(samp_est[[1]][,1]),
      a_upr = quantile(samp_est[[1]][,1], 0.975),
      a_lwr = quantile(samp_est[[1]][,1], 0.025),
      b1 = median(samp_est[[1]][,2]),
      b1_upr = quantile(samp_est[[1]][,2], 0.975),
      b1_lwr = quantile(samp_est[[1]][,2], 0.025)
    )
    out <- rbind(out, tdf)
  }
  
  
}

## based on mean by site temperature

TEMP2 <- apply(TEMP,c(1,2), mean) 

## replicate TEMP2 to have full structure
library(xts)
TEMP3 <- do.call(rbind, replicate(10, coredata(TEMP2), simplify = FALSE))
TEMP4 <- array(data = TEMP3,dim = c(200,5,10) )

for(i in 1:length(tt)) {
  
newdat <- samp_occ[[1]][tt[i],-c(1,2)]
nd1 <- array(data = newdat, dim = c(200,5,10))


## based on single temp
dat_med <-  list(
  site = dim(sp1_occ)[1],
  TEMP = TEMP4,
  year = dim(sp1_occ)[3],
  obs = dim(sp1_occ)[2],
  y = nd1
  
)

est_occ_med_temp <- jags.model(data = dat_med,  
                           #parameters.to.save = params, 
                           file = "temp_over_time.txt", 
                           n.chains = 3, 
                           #n.iter = 10000, 
                           #n.thin = 10,
                           n.adapt = 1000 )





samp_est_med <- coda.samples(est_occ_med_temp, variable.names = params, n.iter = 5000, thin = 1)


if(i == 1){
  out_med <- data.frame(
    sample = 1,
    a = median(samp_est[[1]][,1]),
    a_upr = quantile(samp_est[[1]][,1], 0.975),
    a_lwr = quantile(samp_est[[1]][,1], 0.025),
    b1 = median(samp_est[[1]][,2]),
    b1_upr = quantile(samp_est[[1]][,2], 0.975),
    b1_lwr = quantile(samp_est[[1]][,2], 0.025)
  )
  
} else {
  
  tdf_med <- data.frame(
    sample = i,
    a = median(samp_est[[1]][,1]),
    a_upr = quantile(samp_est[[1]][,1], 0.975),
    a_lwr = quantile(samp_est[[1]][,1], 0.025),
    b1 = median(samp_est[[1]][,2]),
    b1_upr = quantile(samp_est[[1]][,2], 0.975),
    b1_lwr = quantile(samp_est[[1]][,2], 0.025)
  )
  out_med <- rbind(out_med, tdf_med)
 }


}

out$coverage <- 0

for(i in 1:nrow(out)){
if(out$a_upr[i] > 3 & out$a_lwr[i] < 3 & out$b1_upr[i] > 2 & out$b1_lwr[i] < 2 ) {
  out$coverage[i] <- 1
} else {
  out$coverage[i] <- 0
}
}


saveRDS(list("out_med", "out"), file = "temperature_over_time.RDS")

readRDS(file = "temperature_over_time.RDS")
