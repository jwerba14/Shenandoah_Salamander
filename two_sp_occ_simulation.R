## abiotic and biotic effects

## simulate temperatures (200 sites, 10 years, spatially correlated with small change over time)
library(lme4)
library(rjags)
library(tidyverse)

## 200 sites, 5 observations per site each for 10 years
expdat <- expand.grid(sites = factor(1:200), year = seq(1,10, by =1))

#expdat$obs <- factor(seq(nrow(expdat)))

#Parameters: baseline range , effect of treatment is to decrease log-odds of range by 0.4 (approx 0.4/4 = 10%).
#Random effects standard deviations of individual and observation are both 1.0 (on the same scale as the fixed-effect parameters).

set.seed(101)
nsim <- 1
beta <- c(15, 0.2)
theta <- c(0.1)
sigma <- 0.1

ss <- simulate(~ year + (1 | sites) , nsim = nsim,  family = gaussian,
               weights = rep(25, nrow(expdat)), newdata = expdat, newparams = list(theta = theta, 
                                                                                   beta = beta,
                                                                                   sigma = sigma))



ss$year <- expdat$year
ss$site <- expdat$site

TEMP <- ss  %>% pivot_wider(c(year, site), names_from = year , values_from = sim_1) %>% dplyr::select(-site)


## simulate occupancy data



#fit
ni <- 5000
nb <- 20
nt <- 1
nc <- 3
na <- 10000
mi <- 1000
n.chains = 3

sp1_occ <- array(data = NA, dim= c(200,5,10)) 

sp1_occ[, ,] <- as.numeric(sp1_occ[, , ])

sp2_occ <- sp1_occ



## simulate occupancy 
library(R2jags)
params <- c( #"alpha.psiSC", 
            #"alpha.psiSc",
            #"alpha.psiC",
            
            # Intercept
            #"alpha.survAloneS",
            #"alpha.survBothS" ,
            #"alpha.colEmptyS",
            #"alpha.colOccS" ,
            
            # TEMP slope
            "beta.survAloneS1" ,
            "beta.survBothS1" ,
            "beta.colEmptyS1",
            "beta.colOccS1" ,
            
            
            # Intercept
           # "alpha.survAloneC",
           # "alpha.survBothC",
            #"alpha.colEmptyC" ,
            #"alpha.colOccC",
            
            # TEMP slope
            "beta.survAloneC1",
            "beta.survBothC1",
            "beta.colEmptyC1",
            "beta.colOccC1",
            
            ############## Detection parameters- Assume constant across years
            
            #"alpha.pS",
            #"alpha.rSC",
           # "alpha.rSc",
           # "alpha.rC",
            #"alpha.pC",
             
            "muv",
            "muz")


## effect size dataframe to loop over ## which parameters should be included?? 
## lets start with 
library(pomp)
eff_size <- sobolDesign(lower =c(true_beta.survAloneS1 =0,
                          true_beta.survBothS1 =0,
                          true_beta.colEmptyS1 = 0,
                          true_beta.colOccS1 = 0,
                          true_beta.survAloneC1 =0,
                          true_beta.survBothC1 = 0,
                          true_beta.colEmptyC1 = 0,
                          true_beta.colOccC1 =0), 
                        upper =c(true_beta.survAloneS1 =1,
                                 true_beta.survBothS1 =1,
                                 true_beta.colEmptyS1 = 1,
                                 true_beta.colOccS1 = 1,
                                 true_beta.survAloneC1 =1,
                                 true_beta.survBothC1 = 1,
                                 true_beta.colEmptyC1 = 1,
                                 true_beta.colOccC1 =1),
                        nseq = 200
    )

## dataframe for occupancy prob
 occupancy_prob <- data.frame(
   occ_v = 0,
  occ_z = 0,
  sample = 0)

  
 
for (i in 1:nrow(eff_size)){

#bundle data
dat_sim <-  list(
  N = dim(sp1_occ)[1],
  TEMP = TEMP,
  Yr = dim(sp1_occ)[3],
  #J = dim(sp1_occ)[2],
  #yS= sp1_occ,
  #yC = sp2_occ,
  
  true_alpha.psiSC = 0,
  true_alpha.psiSc = 0,
  true_alpha.psiC = 0,
  true_alpha.survAloneS = 0, 
  true_alpha.survBothS = 0,
  true_alpha.colEmptyS = 0,
  true_alpha.colOccS = 0,
  
  # TEMP slope
  true_beta.survAloneS1 = eff_size$true_beta.survAloneS1[i] ,
  true_beta.survBothS1 = eff_size$true_beta.survBothS1[i] ,
  true_beta.colEmptyS1 = eff_size$true_beta.colEmptyS1[i],
  true_beta.colOccS1 = eff_size$true_beta.colOccS1[i],
  
  # Intercept
  true_alpha.survAloneC = 0,
  true_alpha.survBothC = 0,
  true_alpha.colEmptyC = 0,
  true_alpha.colOccC = 0,
  
  # TEMP slope
  true_beta.survAloneC1 = eff_size$true_beta.survAloneC1[i],
  true_beta.survBothC1 = eff_size$true_beta.survBothC1[i],
  true_beta.colEmptyC1 = eff_size$true_beta.colEmptyC1[i],
  true_beta.colOccC1 = eff_size$true_beta.colOccC1[i]
  
)




sim_occ <- jags.model(data = dat_sim,  
                      #parameters.to.save = params, 
                      file = "models/two_species_temp_simulation.txt", 
                      n.chains = 3, 
                      #n.iter = 10000, 
                      #n.thin = 10,
                      n.adapt = 1000,
                      #n.cluster = n.chains,
                      #parameters.to.save = params
                    )

samp_occ <- coda.samples(sim_occ, variable.names = params, n.iter = 5000, thin = 1)


## Fill coefficient posterior for each parameter
sp1_prob_occ  <- grep("muv", dimnames(samp_occ[[1]])[[2]])
samp_occ.s1 <- as.data.frame(samp_occ[[1]][, params.ext]) # first chain
samp_occ.s2 <- as.data.frame(samp_occ[[2]][, params.ext]) # 2nd chain
samp_occ.s3 <- as.data.frame(samp_occ[[3]][, params.ext]) # 3rd chain
samp_occ.s  <- rbind(samp_occ.s1, samp_occ.s2, samp_occ.s3)
samp_occ.s  <- reshape2::melt(samp_occ.s)
 
params.ext  <- grep("muz", dimnames(samp_occ[[1]])[[2]])
samp_occ.s1 <- as.data.frame(samp_occ[[1]][, params.ext]) # first chain
samp_occ.s2 <- as.data.frame(samp_occ[[2]][, params.ext]) # 2nd chain
samp_occ.s3 <- as.data.frame(samp_occ[[3]][, params.ext]) # 3rd chain
samp_occ.s  <- rbind(samp_occ.s1, samp_occ.s2, samp_occ.s3)
samp_occ.s  <- reshape2::melt(samp_occ.s)
   
samp_occ.s  <- samp_occ.s %>% mutate(
  sample = rep(seq(1, 15000), each = 200 * 9)
, site   = rep(rep(seq(1:200), each = 15000), 9)
, year   = rep(seq(2, 10), each = 15000 * 200)
  )

samp_occ.s <- samp_occ.s %>% group_by(year) %>% summarize(
    lwr_nrw = quantile(value, 0.25)
  , lwr     = quantile(value, 0.025)
  , upr_nrw = quantile(value, 0.75)
  , upr     = quantile(value, 0.975)
  , med = quantile(value, 0.5)
  , param_set = i
)

samp_occ.s <- cbind(samp_occ.s, eff_size[i, ]) 

if (i == 1) {       
  samp_occ.s.f <- samp_occ.s
} else {
  samp_occ.s.f <- rbind(samp_occ.s.f, samp_occ.s)    
      
}
}
 
write.csv(samp_occ.s.f, file = "simulation_temp_comp.csv")

dat <- read.csv("simulation_temp_comp.csv")


dat1 <- dat %>% dplyr::select(-X) %>%
  pivot_longer(-c(year,param_set, true_beta.survAloneS1, true_beta.survBothS1, 
                  true_beta.colEmptyS1, true_beta.colOccS1 , true_beta.survAloneC1,
                  true_beta.survBothC1,true_beta.colEmptyC1,true_beta.colOccC1), names_to = "quantile", values_to = "value" )

dat2 <- dat1 %>% pivot_longer(-c(year, param_set, value, quantile), names_to = "param", values_to = "param_val")

## summarize by taking difference in mean over time

dat3 <- dat2 %>% 
  filter(quantile == "med") %>%
  filter(year == "2" | year == "10") %>%
  pivot_wider(everything(), names_from = year, values_from = value)
names(dat3) <- c("param_set", "quantile", "param", "param_val", "year_2", "year_10")  
  
dat4 <- dat3 %>%  
  group_by(param_set) %>% 
 mutate(delta_pr  = year_10 - year_2)

gg1 <- ggplot(dat4, aes(param_val,delta_pr)) + geom_point() + facet_wrap(~param) + theme_bw()

print(gg1)
   
