##### The following R script was written by G. V. DiRenzo
### Please send questions to: grace.direnzo@gmail.com

# Objective: 
  # 1. To run 4 models (abiotic/biotic, abiotic, biotic, null [in that order]) and estimate parameters
  # 2. To create the master table with all parameter values for the 4 models





###### ###### BEWARE: There are 2 iterations of the PVA ###### ###### 

  # Iteration 1: We used the parameter estimates from each of the 4 models below to do the PVA - but this gave weird results. It showed that the Biotic model had a higher quasi-extinction probability than the Abiotic Biotic models
         ## JW:: but it is possible they don't compete- maybe like the rail example in Richmond et al 2010 they help each other in some way??? but I think it is right to fit to the full model... but does that mean the full model will always be better...

  # Iteration 2: We ONLY used the parameter estimates from the Abiotic Biotic model for each of the different scenarios. 
      # For example, in the null scenario (no competition, no climate change) - we used parameter estimates where Pshen was alone and the site was empty.
      # Evan showed you the results of this PVA
      # In this case, you only need to run the code to line 182

###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 




###############################
###############################
###############################

# 1. Set working directory 
# 2. load packages & data
# 3. Bundle data & run Abiotic/Biotic model
# 4. Bundle data & run Abiotic model 
# 5. Bundle data & run Biotic model
# 6. Bundle data & run Null model
# 7. Extract parameter values & make a table

###############################
###############################
###############################



# 1. Set working directory ------------------------------------------------

setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")
#setwd("F:/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")

# 2. load packages & data --------------------------------------------------------

library(jagsUI)
library(beepr)

# Load data
setwd("C:/Users/jower/OneDrive/Documents/PostDoc1/Shenandoah_Salamander/Data") ## i don't know couldn't get load to work so here we are continuously resetting wd..
load("formatted_shen_cin_dat.rda")
shen <- dat$shen 
cin <- dat$cin 

shen <- dat$shen[, , 1:5]
cin <- dat$cin[, , 1:5]
# Load temp/RMI data
load("temp_rmi.rda")

# 3. Bundle data & run Abiotic/Biotic model ----------------------------------------------------------

# Parameters to monitor
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
            "zzz.diff")

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

shen_sim <- array(1, dim = c(209,14,20))
shen_sim[ , , ] <- NA

cin_sim <- array(1, dim = c(209,14,20))
cin_sim[ , , ] <- NA
win.data_sim <- list(
  ## Parameter estimation
  N = dim(shen_sim)[1],  ## 209 (# of sites) 
  J =  dim(shen_sim)[2], # 14 (surveys in N site)
  Yr = dim(shen_sim)[2], #10 (years)
  yS = shen_sim,  ## full 0,1 matrix for p.shen sal
  yC = cin_sim,   ## full p.cin 
  TEMP = rep(std_temp,20), ##normalized temp of all years temp?- presumably so temp and RMI are on same scale?
  RMI = rep(std_rmi,20) ## normalized RMI
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
abiotic_biotic <- autojags(data = win.data, inits = inits,  
                parameters.to.save = params, 
                model.file = "model_abiotic_biotic.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                iter.increment = ni,
                parallel = TRUE)


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

## store 60 years of simulated data, then fit to 10 and see how it compares to 60

#abiotic_biotic <- jags(data = win.data, inits = inits,  
#                parameters.to.save = params, 
#                model.file = "./Models/model_abiotic_biotic.txt", 
#                n.chains = 3, 
#                n.thin = 1, 
#                n.burnin = 1, 
#                n.adapt = 1,
#                n.iter = 2,
#                parallel = TRUE)
#

beepr::beep(2)

save(abiotic_biotic, file = "./ModelOutput/abiotic_biotic_output.rda")
load()
# 4. Bundle data & run Abiotic model ----------------------------------------------------------

# Bundle data
win.data <- list(
  ## Parameter estimation
  N = dim(shen)[1],
  J = dim(shen)[2],
  Yr = dim(shen)[3],
  
  yS = shen,
  yC = cin,
  
  TEMP = std_temp,
  RMI = std_rmi,

  #informative priors
  apSm = abiotic_biotic$mean$alpha.psiSc,
  apSsd = abiotic_biotic$sd$alpha.psiSc,
  apCm = abiotic_biotic$mean$alpha.psiC,
  apCsd = abiotic_biotic$sd$alpha.psiC,
  asSm = abiotic_biotic$mean$alpha.survAloneS,
  asSsd = abiotic_biotic$sd$alpha.survAloneS,
  acSm = abiotic_biotic$mean$alpha.colEmptyS,
  acSsd = abiotic_biotic$sd$alpha.colEmptyS, 
  bsSm1 = abiotic_biotic$mean$beta.survAloneS1,
  bsSsd1 = abiotic_biotic$sd$beta.survAloneS1,
  bcSm1 = abiotic_biotic$mean$beta.colEmptyS1,
  bcSsd1 = abiotic_biotic$sd$beta.colEmptyS1,
  bsSm2 = abiotic_biotic$mean$beta.survAloneS2,
  bsSsd2 = abiotic_biotic$sd$beta.survAloneS2,
  bcSm2 = abiotic_biotic$mean$beta.colEmptyS2,
  bcSsd2 = abiotic_biotic$sd$beta.colEmptyS2,
  
  asCm = abiotic_biotic$mean$alpha.survAloneC,
  asCsd = abiotic_biotic$sd$alpha.survAloneC,
  acCm = abiotic_biotic$mean$alpha.colEmptyC,
  acCsd = abiotic_biotic$sd$alpha.colEmptyC, 
  bsCm1 = abiotic_biotic$mean$beta.survAloneC1,
  bsCsd1 = abiotic_biotic$sd$beta.survAloneC1,
  bcCm1 = abiotic_biotic$mean$beta.colEmptyC1,
  bcCsd1 = abiotic_biotic$sd$beta.colEmptyC1,
  bsCm2 = abiotic_biotic$mean$beta.survAloneC2,
  bsCsd2 = abiotic_biotic$sd$beta.survAloneC2,
  bcCm2 = abiotic_biotic$mean$beta.colEmptyC2,
  bcCsd2 = abiotic_biotic$sd$beta.colEmptyC2,
  
  pSm = abiotic_biotic$mean$alpha.pS,
  pSsd = abiotic_biotic$sd$alpha.pS,
  
  pCm = abiotic_biotic$mean$alpha.pC,
  pCsd = abiotic_biotic$sd$alpha.pC
)

# Parameters to monitor
params <- c("alpha.pS", "alpha.pC",
            
            "alpha.psiS",       
            "alpha.psiC", 
            
            "alpha.survS",           
            "alpha.colS",           
            "beta.survS1",
            "beta.colS1", 
            "beta.survS2",
            "beta.colS2", 
            
            "alpha.survC",
            "alpha.colC", 
            "beta.survC1", 
            "beta.colC1",  
            "beta.survC2", 
            "beta.colC2",
            
            "zzz.diff")

# Run model
abiotic <- autojags(data = win.data, 
                    inits = inits,  
                    parameters.to.save = params, 
                    model.file = "./Models/model_abiotic.txt", 
                    n.chains = nc, 
                    n.thin = nt, 
                    n.burnin = nb, 
                    n.adapt = na,
                    max.iter = mi,
                    parallel = TRUE)

#abiotic <- jags(data = win.data, inits = inits,  
#     parameters.to.save = params, 
#     model.file = "./Models/model_abiotic.txt", 
#     n.chains = 3, 
#     n.thin = 1, 
#     n.burnin = 1, 
#     n.adapt = 1,
#     n.iter = 2,
#     parallel = TRUE)

save(abiotic, file = "./ModelOutput/abiotic_output.rda")


# 5. Bundle data & run Biotic model ----------------------------------------------------------


# Parameters to monitor
params <- c("alpha.pS", "alpha.rSC", "alpha.rSc", "alpha.rC", "alpha.pC",
            "alpha.psiSC",       
            "alpha.psiSc",       
            "alpha.psiC",        
            "alpha.survAloneS",            "alpha.survBothS",
            "alpha.colEmptyS",            "alpha.colOccS",
            "alpha.survAloneC",            "alpha.survBothC",
            "alpha.colEmptyC",            "alpha.colOccC",
            "total.popPS", "total.popPC",
            "zzzfit", "zzzfit.new",
            "Pshen", "Pcin")

# Bundle data
win.data <- list(
  ## Parameter estimation
  N = dim(shen)[1],
  J = dim(shen)[2],
  Yr = dim(shen)[3],
  yS = shen,
  yC = cin
)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data$yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
zinit[zinit == "-Inf"] <- 1  

# Take max value across surveys for each site and year combo
vinit <- apply(win.data$yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1   

inits <- function() {list(
  z = zinit,
  v = vinit
)}


# Run model
biotic <- autojags(data = win.data, inits = inits,  
                parameters.to.save = params, 
                  model.file = "./Models/model_biotic.txt", 
                n.chains = nc, n.thin = nt, n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                parallel = TRUE)

#biotic <- jags(data = win.data, inits = inits,  
#     parameters.to.save = params, 
#     model.file = "./Models/model_biotic.txt", 
#     n.chains = 3, 
#     n.thin = 1, 
#     n.burnin = 1, 
#     n.adapt = 1,
#     n.iter = 2,
#     parallel = TRUE)

save(biotic, file = "./ModelOutput/biotic_output.rda")

# 6. Bundle data & run Null model ----------------------------------------------------------


# Parameters to monitor
params <- c("alpha.pS", "alpha.pC",
            "alpha.psiS",       
            "alpha.psiC",         
            "alpha.survS",      
            "alpha.colS",     
            "alpha.survC",      
            "alpha.colC",   
            "zzz.diff")

# Bundle data
win.data <- list(
  ## Parameter estimation
  N = dim(shen)[1],
  J = dim(shen)[2],
  Yr = dim(shen)[3],
  yS = shen,
  yC = cin,
  #informative priors
  apSm = biotic$mean$alpha.psiSc,
  apSsd = biotic$sd$alpha.psiSc,
  apCm= biotic$mean$alpha.psiC,
  apCsd = biotic$sd$alpha.psiC,
  
  asSm = biotic$mean$alpha.survAloneS,
  asSsd = biotic$sd$alpha.survAloneS,
  acSm = biotic$mean$alpha.colEmptyS,
  acSsd = biotic$sd$alpha.colEmptyS, 
  
  asCm = biotic$mean$alpha.survAloneC,
  asCsd = biotic$sd$alpha.survAloneC,
  acCm = biotic$mean$alpha.colEmptyC,
  acCsd = biotic$sd$alpha.colEmptyC, 
  
  pSm = biotic$mean$alpha.pS,
  pSsd = biotic$sd$alpha.pS,
  
  pCm = biotic$mean$alpha.pC,
  pCsd = biotic$sd$alpha.pC
)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data$yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
zinit[zinit == "-Inf"] <- 1  

# Take max value across surveys for each site and year combo
vinit <- apply(win.data$yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1   

inits <- function() {list(
  z = zinit,
  v = vinit
)}


# Run model
null <- autojags(data = win.data, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/model_null.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                parallel = TRUE)

#null <- jags(data = win.data, inits = inits,  
#     parameters.to.save = params, 
#     model.file = "./Models/model_null.txt", 
#     n.chains = 3, 
#     n.thin = 1, 
#     n.burnin = 1, 
#     n.adapt = 1,
#     n.iter = 2,
#     parallel = TRUE)

# Set working directory
save(null, file = "./ModelOutput/null_output.rda")


# 7. Extract parameter values & make a table ----------------------------------------------------------

# Four models: abiotic, abiotic_biotic, biotic, null

# Master parameter table
prms <- matrix(NA, 
              nrow = 45,    # 45 total parameter estimates
              ncol = 3 * 4) # 3 estimates per model (mean, 2.5, 97.5) * 4 models

# Add row names
rownames(prms) <-
  c("alpha.pS", "alpha.rSC","alpha.rSc", "alpha.rC", "alpha.pC",
    "alpha.psiS",
    "alpha.psiSC",       
    "alpha.psiSc",       
    "alpha.psiC", #9
    "alpha.survS", #10
    "alpha.survAloneS",            "alpha.survBothS",
    "alpha.colS", #13
    "alpha.colEmptyS",            "alpha.colOccS",
    "beta.survS1", "beta.survS2", # 16, 17
    "beta.colS1", "beta.colS2", #18,19
    "beta.survAloneS1",            "beta.survBothS1",
    "beta.colEmptyS1",            "beta.colOccS1",
    "beta.survAloneS2",            "beta.survBothS2",
    "beta.colEmptyS2",            "beta.colOccS2",
    "alpha.survC", #28
    "alpha.survAloneC",            "alpha.survBothC",
    "alpha.colC", #31
    "alpha.colEmptyC",            "alpha.colOccC",
    "beta.survC1", "beta.survC2",
    "beta.colC1", "beta.colC2",
    "beta.survAloneC1",            "beta.survBothC1",
    "beta.colEmptyC1",            "beta.colOccC1",
    "beta.survAloneC2",            "beta.survBothC2",
    "beta.colEmptyC2",            "beta.colOccC2")

# Add column names
colnames(prms) <- c("null.mean", "null.q2.5", "null.q97.5",
                   "biotic.mean", "biotic.q2.5", "biotic.q97.5",
                   "abiotic.mean", "abiotic.q2.5", "abiotic.q97.5",
                   "abiotic_biotic.mean", "abiotic_biotic.q2.5", "abiotic_biotic.q97.5")

# Fill in: Null model
# Pull out parameter names
null.names <- names(null$mean)

# Match the parameter names between the model and the master table
fill_rows <- which(rownames(prms)  %in% null.names)

prms[fill_rows, "null.mean"] <- unlist(null$mean[1:length(fill_rows)])
prms[fill_rows, "null.q2.5"] <- unlist(null$q2.5[1:length(fill_rows)])
prms[fill_rows, "null.q97.5"] <- unlist(null$q97.5[1:length(fill_rows)])


# Fill in: Biotic model
# Pull out parameter names
biotic.names <- names(biotic$mean)

# Match the parameter names between the model and the master table
fill_rows <- which(rownames(prms)  %in% biotic.names)

prms[fill_rows, "biotic.mean"]  <- unlist(biotic$mean[1:length(fill_rows)])
prms[fill_rows, "biotic.q2.5"]  <- unlist(biotic$q2.5[1:length(fill_rows)])
prms[fill_rows, "biotic.q97.5"] <- unlist(biotic$q97.5[1:length(fill_rows)])


# Fill in: Abiotic model

# Pull out parameter names
abiotic.names <- names(abiotic$mean)

# Match the parameter names between the model and the master table
fill_rows <- which(rownames(prms)  %in% abiotic.names)

prms[fill_rows, "abiotic.mean"]  <- unlist(abiotic$mean[1:length(fill_rows)])
prms[fill_rows, "abiotic.q2.5"]  <- unlist(abiotic$q2.5[1:length(fill_rows)])
prms[fill_rows, "abiotic.q97.5"] <- unlist(abiotic$q97.5[1:length(fill_rows)])



# Fill in: Biotic/Abiotic model
# Pull out parameter names
abiotic_biotic.names <- names(abiotic_biotic$mean)

# Match the parameter names between the model and the master table
fill_rows <- which(rownames(prms)  %in% abiotic_biotic.names)

prms[fill_rows, "abiotic_biotic.mean"]  <- unlist(abiotic_biotic$mean[1:length(fill_rows)])
prms[fill_rows, "abiotic_biotic.q2.5"]  <- unlist(abiotic_biotic$q2.5[1:length(fill_rows)])
prms[fill_rows, "abiotic_biotic.q97.5"] <- unlist(abiotic_biotic$q97.5[1:length(fill_rows)])

# Write the file
# write.csv(prms, "/Volumes/GVD/Yeti/ShenSal/Tables/master_parameter_values_table.csv")

# End script
