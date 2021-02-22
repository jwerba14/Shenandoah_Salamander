setwd("C:/Users/jower/OneDrive/Documents/PostDoc1/Shenandoah_Salamander/Data") ## i don't know couldn't get load to work so here we are continuously resetting wd..
load("formatted_shen_cin_dat.rda")
shen <- dat$shen 
cin <- dat$cin 

# Load temp/RMI data
load("temp_rmi.rda")

# 3. Bundle data & run Abiotic/Biotic model ----------------------------------------------------------

# Parameters to monitor for simulation need to track v and z as well
params <- c("alpha.pS", "alpha.rSC","alpha.rSc", "alpha.rC", "alpha.pC",
            "alpha.psiSC",       
            "alpha.psiSc",       
            "alpha.psiC",        
            "alpha.survAloneS",            "alpha.survBothS",
            "alpha.colEmptyS",            "alpha.colOccS",
            "beta.survAloneS1",            "beta.survBothS1",
            "beta.colEmptyS1",            "beta.colOccS1",
            "beta.survAloneS2",            "beta.survBothS2",
            "beta.colEmptyS2",            "beta.colOccS2",
            "alpha.survAloneC",            "alpha.survBothC",
            "alpha.colEmptyC",            "alpha.colOccC",
            "beta.survAloneC1",            "beta.survBothC1",
            "beta.colEmptyC1",            "beta.colOccC1",
            "beta.survAloneC2",            "beta.survBothC2",
            "beta.colEmptyC2",            "beta.colOccC2",
            "zzz.diff",                    "z",
             "v")

# Create temperature file 
TEMP <- dat$site.temp$Tmax_p
mean_temp <- mean(TEMP)
sd_temp <- sd(TEMP)
std_temp <- (TEMP - mean_temp ) / sd_temp

RMI <- dat$site.RMI$RMI_p
mean_rmi <- mean(RMI,na.rm = TRUE)
sd_rmi <- sd(RMI,na.rm = TRUE)
std_rmi <- (RMI - mean_rmi ) / sd_rmi

# Bundle data
win.data <- list(
  ## Parameter estimation
  N = dim(shen)[1],  ## 209 (# of sites) 
  J =  dim(shen)[2], # 14 (surveys in N site)
  Yr = dim(shen)[3], #10 (years)
  yS = shen,  ## full 0,1 matrix for p.shen sal
  yC = cin,   ## full p.cin 
  TEMP = std_temp, ##normalized temp of all years temp?- presumably so temp and RMI are on same scale?
  RMI = std_rmi ## normalized RMI
)


## prior simulation
shen_sim <- shen
shen_sim[ , , ] <- NA

cin_sim <- cin
cin_sim[ , , ] <- NA

win.data_sim <- list(
  ## Parameter estimation
  N = dim(shen)[1],  ## 209 (# of sites) 
  J =  dim(shen)[2], # 14 (surveys in N site)
  Yr = dim(shen)[3], #10 (years)
  yS = shen_sim,  ## full 0,1 matrix for p.shen sal
  yC = cin_sim,   ## full p.cin 
  TEMP = std_temp, ##normalized temp of all years temp?- presumably so temp and RMI are on same scale?
  RMI = std_rmi ## normalized RMI
)

# Look at structure
str(win.data)
str(win.data_sim)
# Estimate the occupancy of Pshen
# Take max value across surveys for each site and year combo
Sprez <- apply(win.data$yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
Sprez[Sprez == "-Inf"] <- NA

# Total number of sites sampled
tot.sites <- Sprez
tot.sites[tot.sites == 0] <- 1
colSums(Sprez, na.rm = TRUE)/colSums(tot.sites, na.rm = T) 

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data$yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
zinit[zinit == "-Inf"] <- NA

# Estimate the occupancy of Pshen
# Take max value across surveys for each site and year combo
Cprez <- apply(win.data$yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
Cprez[Cprez == "-Inf"] <- NA

# Total number of sites sampled
colSums(Cprez, na.rm = TRUE)/colSums(tot.sites, na.rm = T) 

# Take max value across surveys for each site and year combo
vinit <- apply(win.data$yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  


# Bundle initial values
inits <- function() {list(
  z = zinit,
  v = vinit
)}

# MCMC settings
ni <- 5000
nb <- 20
nt <- 1
nc <- 3
na <- 10000
mi <- 1000

# Run model
setwd("C:/Users/jower/OneDrive/Documents/PostDoc1/Shenandoah_Salamander/models")

library(jagsUI)

abiotic_biotic_sim <- autojags(data = win.data_sim, inits = NULL,  
                               parameters.to.save = params, 
                               model.file = "model_abiotic_biotic.txt", 
                               n.chains = nc, 
                               n.thin = nt, 
                               n.burnin = nb, 
                               n.adapt = na,
                               max.iter = mi,
                               iter.increment = ni,
                               parallel = TRUE)

str(abiotic_biotic_sim)

## output of p.shenendoah

p.shen.sim <- abiotic_biotic_sim$sims.list$z

## output of p.cinereus

p.cin.sim <- abiotic_biotic_sim$sims.list$v


##summarize by percent occupied by year?  and coef_plot of all params except z,v
## bc priors are centered at zero, no matter how wide they are for a bernoulli you will get 0.5 present - unbiased at zero,
## what wider priors allows is more play in the spreading the individual coefficients from one another
## these priors may artificially pull them all towards zero 
## they are therefore uninformative on the probability scale but are informative on the size of each individual coefficient
## so we may be estimating for example the effect of temperature to be much smaller than it really it

p.shen.occ <- apply(p.shen.sim,2:3,mean)  #col year, row site
plot(p.shen.occ)

p.cin.occ <- apply(p.shen.sim, 2:3, mean)

## lets see if wider priors gives the same results for occupancy, I changed priors to dnorm(0,0.04)
abiotic_biotic_sim_wider <- autojags(data = win.data_sim, inits = NULL,  
                               parameters.to.save = params, 
                               model.file = "model_abiotic_biotic_wider.txt", 
                               n.chains = nc, 
                               n.thin = nt, 
                               n.burnin = nb, 
                               n.adapt = na,
                               max.iter = mi,
                               iter.increment = ni,
                               parallel = TRUE)

## output of p.shenendoah

p.shen.sim_wider <- abiotic_biotic_sim_wider$sims.list$z

## output of p.cinereus

p.cin.sim_wider <- abiotic_biotic_sim_wider$sims.list$v


p.shen.occ_wider <- apply(p.shen.sim_wider,2:3,mean)  #col year, row site
plot(p.shen.occ_wider)

p.cin.occ_wider <- apply(p.shen.sim_wider, 2:3, mean)
plot(p.cin.occ_wider)


## lets see if narrower priors give the same results for occupancy dnorm(0,0.9)
abiotic_biotic_sim_narrow <- autojags(data = win.data_sim, inits = NULL,  
                                     parameters.to.save = params, 
                                     model.file = "model_abiotic_biotic_narrow.txt", 
                                     n.chains = nc, 
                                     n.thin = nt, 
                                     n.burnin = nb, 
                                     n.adapt = na,
                                     max.iter = mi,
                                     iter.increment = ni,
                                     parallel = TRUE)


## output of p.shenendoah

p.shen.sim_narrow <- abiotic_biotic_sim_narrow$sims.list$z

## output of p.cinereus

p.cin.sim_narrow <- abiotic_biotic_sim_narrow$sims.list$v


p.shen.occ_narrow <- apply(p.shen.sim_narrow,2:3,mean)  #col year, row site
plot(p.shen.occ_narrow)

p.cin.occ_narrow <- apply(p.shen.sim_narrow, 2:3, mean)
plot(p.cin.occ_narrow)


## plot coefficients from each -- I think to really explore this need to write simpler model and fit...
