##### The following R script was written by G. V. DiRenzo
### Please send questions to: grace.direnzo@gmail.com

# Objective: 
# 1. To run population viability analysis for 18 scenarios
  # Null
  # Biotic
  # Biotic Abiotic
    # scenario 1, linear increase, + 2 RMI
    # scenario 1, exponential increase, + 2 RMI
    # scenario 1, linear increase, - 2 RMI
    # scenario 1, exponential increase, - 2 RMI
    # scenario 2, linear increase, + 2 RMI
    # scenario 2, exponential increase, + 2 RMI
    # scenario 2, linear increase, - 2 RMI
    # scenario 2, exponential increase, - 2 RMI
  # Abiotic
    # scenario 1, linear increase, + 2 RMI
    # scenario 1, exponential increase, + 2 RMI
    # scenario 1, linear increase, - 2 RMI
    # scenario 1, exponential increase, - 2 RMI
    # scenario 2, linear increase, + 2 RMI
    # scenario 2, exponential increase, + 2 RMI
    # scenario 2, linear increase, - 2 RMI
    # scenario 2, exponential increase, - 2 RMI
  # 2. To calculate the:
      # extinction probability, 
      # quasi-extinction probability, and 
      # minimum population size for all species each year  

###############################
###############################
###############################

# 1. Set working directory 
# 2. load packages
# 3. load data
# 4. bash script settings
# 5. Create array to save output
# 6. Projections
  # 6a. Biotic Abiotic
  # 6b. Abiotic
  # 6c. Biotic
  # 6d. Null
# 7. Calculate metrics
# 8. Calculate population size over time 
# 9. Save results

###############################
###############################
###############################


# 1. Set working directory ------------------------------------------------

#setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")
#setwd("F:/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")
setwd("/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/Dropbox/USGS/ShenandoahSalamander/")


# 2. load packages --------------------------------------------------------

libLocation <- c(.libPaths(),
"/home/gdirenzo/R/x86_64-redhat-linux-gnu-library/4.0",
"/opt/ohpc/pub/usgs/libs/gnu8/R/4.0.1/lib64/R/library")

library(tidyverse, lib.loc = libLocation)
library(reshape2, lib.loc = libLocation)
library(ulimit, lib.loc = libLocation)

library(tidyverse)
library(reshape2)
library(pryr)
#library(ulimit)


# 3. load data --------------------------------------------------------


# 4. bash script settings -------------------------------------------------

##### Set the Temp and RMI variables
args <- commandArgs(trailingOnly=TRUE)
cat("the arguments are:")
cat(args)

# temp
# args <- c("BioticAbiotic", "temp_mat_range_SCEN1", "RMI_mat_range_sub")

# Directory name
name1 <- as.character(args[1])

if(args[1] == "BioticAbiotic" | args[1] == "Abiotic"){
  
  # Load temp/RMI data
  load("./Data/temp_rmi.rda")
  
  # Set objects in the enviornment
  RMI_mat_range_add <- dat$RMI_mat_range_add
  RMI_mat_range_sub <- dat$RMI_mat_range_sub
  temp_mat_range_SCEN1 <- dat$temp_mat_range_SCEN1
  temp_mat_exp_range_SCEN1 <- dat$temp_mat_exp_range_SCEN1
  temp_mat_range_SCEN2 <- dat$temp_mat_range_SCEN2
  temp_mat_exp_range_SCEN2<- dat$temp_mat_exp_range_SCEN2
  
  # Temp name
  name2 <- as.character(args[2])
  # RMI name
  name3 <- as.character(args[3])

  # Temperature object
  var1 <- get(args[2])
  # RMI object
  var2 <- get(args[3])

## Standardize temp and RMI
  # temp
  TEMP2 <- as.matrix(var1)
  std_temp2 <- (TEMP2 - mean(TEMP2, na.rm = TRUE)) / sd(TEMP2, na.rm = TRUE)
  
  # rmi
  RMI2 <- as.matrix(var2)
  std_rmi2 <- (RMI2 - mean(RMI2, na.rm = TRUE)) / sd(RMI2, na.rm = TRUE)
}

# 5. Create array to save output ------------------------------------------

# Set number of MCMC samples from each model
if(args[1] == "BioticAbiotic"){
  MCMC.samps <- 1500
} else {MCMC.samps <- 300}

# Set values for dimensions
n.MCMC    <- MCMC.samps
n.site    <- 617
n.project <- 60
n.sim     <- 500

# ### short run
# n.MCMC    <- 3
# n.site    <- 617
# n.project <- 5
# n.sim     <- 2


# 6. Projections ----------------------------------------------------------


# 6a. Biotic Abiotic ------------------------------------------------------

if(length(which(name1 == "BioticAbiotic")) == 1){

# Abiotic biotic model
load("./ModelOutput/abiotic_biotic_output.rda")
  
# Set parameter values
alpha.psiSC      <- abiotic_biotic$sims$alpha.psiSC
alpha.psiSc      <- abiotic_biotic$sims$alpha.psiSc
alpha.psiC       <- abiotic_biotic$sims$alpha.psiC
alpha.survAloneS <- abiotic_biotic$sims$alpha.survAloneS
alpha.survBothS  <- abiotic_biotic$sims$alpha.survBothS
alpha.colEmptyS  <- abiotic_biotic$sims$alpha.colEmptyS
alpha.colOccS    <- abiotic_biotic$sims$alpha.colOccS
beta.survAloneS1 <- abiotic_biotic$sims$beta.survAloneS1
beta.survBothS1  <- abiotic_biotic$sims$beta.survBothS1
beta.colEmptyS1  <- abiotic_biotic$sims$beta.colEmptyS1
beta.colOccS1    <- abiotic_biotic$sims$beta.colOccS1
beta.survAloneS2 <- abiotic_biotic$sims$beta.survAloneS2
beta.survBothS2  <- abiotic_biotic$sims$beta.survBothS2
beta.colEmptyS2  <- abiotic_biotic$sims$beta.colEmptyS2
beta.colOccS2    <- abiotic_biotic$sims$beta.colOccS2
alpha.survAloneC <- abiotic_biotic$sims$alpha.survAloneC
alpha.survBothC  <- abiotic_biotic$sims$alpha.survBothC
alpha.colEmptyC  <- abiotic_biotic$sims$alpha.colEmptyC
alpha.colOccC    <- abiotic_biotic$sims$alpha.colOccC
beta.survAloneC1 <- abiotic_biotic$sims$beta.survAloneC1
beta.survBothC1  <- abiotic_biotic$sims$beta.survBothC1
beta.colEmptyC1  <- abiotic_biotic$sims$beta.colEmptyC1
beta.colOccC1    <- abiotic_biotic$sims$beta.colOccC1
beta.survAloneC2 <- abiotic_biotic$sims$beta.survAloneC2
beta.survBothC2  <- abiotic_biotic$sims$beta.survBothC2
beta.colEmptyC2  <- abiotic_biotic$sims$beta.colEmptyC2
beta.colOccC2    <- abiotic_biotic$sims$beta.colOccC2

# Create empty matricies to hold data
# 4D: site x year x MCMC x simulation
occS <- occC <- muS <- muC <- array(NA, dim = c(n.site, n.project, n.MCMC, n.sim))

# 3D: site x year x MCMC
survAloneS1 <- survBothS1 <- colEmptyS1 <- colOccS1 <- survAloneC1 <- survBothC1 <- colEmptyC1 <- colOccC1 <- array(NA, dim = c(n.site, n.project, n.MCMC))
  
# 2D: sites x MCMC
psiC1 <- psiSC1 <- psiSc1 <- array(NA, dim = c(n.site, n.MCMC))
psiS1 <- array(NA, dim = c(n.site, n.MCMC, n.sim))

# For loop to go over each MCMC iteration, site, and projection
# For each MCMC iteration
for(i in 1:n.MCMC){ 
     psiC1[,i] <-  plogis(alpha.psiC[i])
    psiSC1[,i] <- plogis(alpha.psiSC[i])
    psiSc1[,i] <- plogis(alpha.psiSc[i]) 
 
    # For each year in the projection 
    for(k in 2:n.project){ 
      
      # Set the colonization/extinction probabilities
      survAloneS1[ , k-1, i] <- plogis(alpha.survAloneS[i] + 
                                         beta.survAloneS1[i] * std_temp2[, k]+ 
                                         beta.survAloneS2[i] * std_rmi2[ ,k])
      survBothS1[, k-1, i]  <- plogis(alpha.survBothS[i] + 
                                          beta.survBothS1[i] * std_temp2[,k]+ 
                                          beta.survBothS2[i] * std_rmi2[,k])
      colEmptyS1[, k-1, i]  <- plogis(alpha.colEmptyS[i] + 
                                          beta.colEmptyS1[i] * std_temp2[,k]+ 
                                          beta.colEmptyS2[i] * std_rmi2[,k])
      colOccS1[, k-1, i]    <- plogis(alpha.colOccS[i] + 
                                        beta.colOccS1[i] * std_temp2[,k]+
                                        beta.colOccS2[i] * std_rmi2[,k])
      survAloneC1[, k-1, i] <- plogis(alpha.survAloneC[i] + 
                                        beta.survAloneC1[i] * std_temp2[,k]+ 
                                        beta.survAloneC2[i] * std_rmi2[,k])
      survBothC1[, k-1, i]  <- plogis(alpha.survBothC[i] + 
                                        beta.survBothC1[i] * std_temp2[,k]+ 
                                        beta.survBothC2[i] * std_rmi2[,k])
      colEmptyC1[, k-1, i]  <- plogis(alpha.colEmptyC[i] + 
                                        beta.colEmptyC1[i] * std_temp2[,k]+ 
                                        beta.colEmptyC2[i] * std_rmi2[,k])
      colOccC1[, k-1, i]    <- plogis(alpha.colOccC[i] + 
                                        beta.colOccC1[i] * std_temp2[,k]+ 
                                        beta.colOccC2[i] * std_rmi2[,k]) 
    } # close k
    
  # For each simulation  
  for(j in 1:n.sim){
    # Determine Pcin occupancy 1st year
    occC[ , 1, i, j] <- rbinom(n.site, 1, prob = psiC1[,i])
    
    # Based on Pcin occupancy, determine shenandoah probability of occupancy
    psiS1[ , i, j] <- psiSC1[ , i] * occC[ , 1, i, j] + psiSc1[ , i] * (1 - occC[,1, i, j])
    
    # Determine Pshen occupancy 1st year
    occS[, 1, i, j] <- rbinom(n.site, 1, prob = psiS1[ , i, j])	

    # For each year in the projection
    for(k in 2:n.project){ 
      
    # Determine probabilities 
    muS[,k-1,i, j] <- occS[,k-1,i, j] * survAloneS1[, k-1,i] * (1-occC[,k-1,i, j]) + 
                    occS[,k-1,i, j] * survBothS1[, k-1,i]      * occC[,k-1,i, j] + 
                    (1- occS[,k-1,i, j]) * colEmptyS1[, k-1,i] * (1-occC[,k-1,i, j]) +
                    (1- occS[,k-1,i, j]) * colOccS1[, k-1,i]   * occC[,k-1,i, j]
    
    muC[,k-1,i, j] <- (1-occS[,k-1,i, j]) * survAloneC1[, k-1,i] * occC[,k-1,i, j] + 
                    occS[,k-1,i, j]      * survBothC1[,k-1,i] * occC[,k-1,i, j] + 
                    (1- occS[,k-1,i, j]) * colEmptyC1[, k-1,i] * (1-occC[,k-1,i, j]) +
                    occS[,k-1,i, j]      * colOccC1[, k-1,i]   * (1-occC[,k-1,i, j])
    
    # Determine occupancy
    occS[,k,i,j] <- rbinom(n.site, 1, prob = muS[,k-1,i,j])	# True occupancy Shenandoah sal
    occC[,k,i,j] <- rbinom(n.site, 1, prob = muC[,k-1,i,j])	# True occupancy Cinereus sal
    
    } # k = year
  } # j = nsim
} # i = MCMC

# Save simulation results as a list
BioticAbiotic_pva <- list(occS = occS,
                          occC = occC)

# Save simulation results in a folder
#save(BioticAbiotic_pva, file = paste0("./PVA/BioticAbiotic/", name2, "_", name3, "/", name1, "_", name2, "_", name3,"_pva.rda"))  

}

# 6b. Abiotic ------------------------------------------------------


if(length(which(name1 == "Abiotic")) == 1){
  # Abiotic model
  load("./ModelOutput/abiotic_output.rda")
 
  # Set parameter values
  alpha.psiS  <- abiotic$sims$alpha.psiS
  alpha.psiC  <- abiotic$sims$alpha.psiC
  alpha.survS <- abiotic$sims$alpha.survS
  alpha.colS  <- abiotic$sims$alpha.colS
  beta.survS1 <- abiotic$sims$beta.survS1
  beta.colS1  <- abiotic$sims$beta.colS1
  beta.survS2 <- abiotic$sims$beta.survS2
  beta.colS2  <- abiotic$sims$beta.colS2
  alpha.survC <- abiotic$sims$alpha.survC
  alpha.colC  <- abiotic$sims$alpha.colC
  beta.survC1 <- abiotic$sims$beta.survC1
  beta.colC1  <- abiotic$sims$beta.colC1
  beta.survC2 <- abiotic$sims$beta.survC2
  beta.colC2  <- abiotic$sims$beta.colC2

  # Create empty matricies to hold data
  # 4D: site x year x MCMC x simulation
  occS <- occC <- muS <- muC <- array(NA, dim = c(n.site, n.project, n.MCMC, n.sim))
  
  # 3D: site x year x MCMC
  survS <- colS <- survC <- colC <- array(NA, dim = c(n.site, n.project, n.MCMC))
  
  # 2D: sites x MCMC
  psiC <- psiS <- array(NA, dim = c(n.site, n.MCMC))

  
  # For loop to go over each MCMC iteration, site, and projection
  # For each MCMC iteration
  for(i in 1:n.MCMC){ 
    # Determine Pcin occupancy 1st year
    psiC[,i] <- plogis(alpha.psiC[i])
    
    # Determine Pshen occupancy 1st year
    psiS[,i] <- plogis(alpha.psiS[i]) 
    
    # For each year in the projection 
    for(k in 2:n.project){ 
      survS[, k-1,i] <- plogis(alpha.survS[i] + 
                               beta.survS1[i] * std_temp2[,k]+ 
                               beta.survS2[i] * std_rmi2[,k])
      
      colS[, k-1,i]  <- plogis(alpha.colS[i] + 
                               beta.colS1[i] * std_temp2[,k]+ 
                               beta.colS2[i] * std_rmi2[,k])
      
      survC[, k-1,i] <- plogis(alpha.survC[i] + 
                               beta.survC1[i] * std_temp2[,k]+ 
                               beta.survC2[i] * std_rmi2[,k])
      
      colC[, k-1,i]  <- plogis(alpha.colC[i] + 
                               beta.colC1[i] * std_temp2[,k]+ 
                               beta.colC2[i] * std_rmi2[,k])
    } # k close
    
    for(j in 1:n.sim){
    
    # Determine occupancy 1st year  
    occC[,1, i, j] <- rbinom(n.site, 1, prob = psiC[,i])
    occS[,1, i, j] <- rbinom(n.site, 1, prob = psiS[,i])	
    
    # For each year in the projection
    for(k in 2:n.project){ 
      
      muS[,k-1,i,j] <- occS[,k-1,i,j] * survS[, k-1,i] + 
                    (1- occS[,k-1,i,j]) * colS[, k-1,i]
      
      muC[,k-1,i,j] <- survC[, k-1,i] * occC[,k-1,i,j] + 
                     colC[, k-1,i] * (1-occC[,k-1,i,j])
      
      # Determine occupancy for entire projection
      occS[,k,i,j] <- rbinom(n.site, 1, prob = muS[,k-1,i,j])		# True occupancy Shenandoah sal
      occC[,k,i,j] <- rbinom(n.site, 1, prob = muC[,k-1,i,j])		# True occupancy Cinereus sal
    } # k closed
  } # j closed
} # i closed

# Save simulation results as a list
Abiotic_pva <- list(occS = occS,
                    occC = occC)

# Save simulation results in a folder
# save(Abiotic_pva, file = paste0("./PVA/Abiotic/", name2, "_", name3, "/", name1, "_", name2, "_", name3,"_pva.rda"))  
}

# 6c. Biotic ------------------------------------------------------

if(length(which(name1 == "Biotic")) == 1){
  
  # Biotic model
  load("./ModelOutput/biotic_output.rda")
  
  # Set parameter values
  alpha.psiSC      <- biotic$sims$alpha.psiSC
  alpha.psiSc      <- biotic$sims$alpha.psiSc
  alpha.psiC       <- biotic$sims$alpha.psiC
  alpha.survAloneS <- biotic$sims$alpha.survAloneS
  alpha.survBothS  <- biotic$sims$alpha.survBothS
  alpha.colEmptyS  <- biotic$sims$alpha.colEmptyS
  alpha.colOccS    <- biotic$sims$alpha.colOccS
  alpha.survAloneC <- biotic$sims$alpha.survAloneC
  alpha.survBothC  <- biotic$sims$alpha.survBothC
  alpha.colEmptyC  <- biotic$sims$alpha.colEmptyC
  alpha.colOccC    <- biotic$sims$alpha.colOccC

  # Create empty matricies to hold data
  # 4D: site x year x MCMC x simulation
  occS <- occC <- muS <- muC <- array(NA, dim = c(n.site, n.project, n.MCMC, n.sim))
  
  # 3D: site x year x MCMC
  survAloneS1 <- survBothS1 <- colEmptyS1 <- colOccS1 <- survAloneC1 <- survBothC1 <- colEmptyC1 <- colOccC1 <- array(NA, dim = c(n.site, n.project, n.MCMC))
  
  # 2D: sites x MCMC
  psiC1 <- psiSC1 <- psiSc1 <- array(NA, dim = c(n.site, n.MCMC))
  psiS1 <- array(NA, dim = c(n.site, n.MCMC, n.sim))
  
  # For loop to go over each MCMC iteration, site, and projection
  # For each MCMC iteration
  for(i in 1:n.MCMC){ 
    psiC1[,i] <-  plogis(alpha.psiC[i])
    psiSC1[,i] <- plogis(alpha.psiSC[i])
    psiSc1[,i] <- plogis(alpha.psiSc[i]) 
    
    # For each year in the projection 
    for(k in 2:n.project){ 
      
      # Set the colonization/extinction probabilities
      survAloneS1[ , k-1, i] <- plogis(alpha.survAloneS[i])
      survBothS1[, k-1, i]  <- plogis(alpha.survBothS[i])
      colEmptyS1[, k-1, i]  <- plogis(alpha.colEmptyS[i])
      colOccS1[, k-1, i]    <- plogis(alpha.colOccS[i])
      survAloneC1[, k-1, i] <- plogis(alpha.survAloneC[i])
      survBothC1[, k-1, i]  <- plogis(alpha.survBothC[i])
      colEmptyC1[, k-1, i]  <- plogis(alpha.colEmptyC[i])
      colOccC1[, k-1, i]    <- plogis(alpha.colOccC[i]) 
    } # close k
    
    # For each simulation  
    for(j in 1:n.sim){
      # Determine Pcin occupancy 1st year
      occC[ , 1, i, j] <- rbinom(n.site, 1, prob = psiC1[,i])
      
      # Based on Pcin occupancy, determine shenandoah probability of occupancy
      psiS1[ , i, j] <- psiSC1[ , i] * occC[ , 1, i, j] + 
                        psiSc1[ , i] * (1 - occC[,1, i, j])
      
      # Determine Pshen occupancy 1st year
      occS[, 1, i, j] <- rbinom(n.site, 1, prob = psiS1[ , i, j])	
      
      # For each year in the projection
      for(k in 2:n.project){ 
        
        # Determine probabilities 
        muS[,k-1,i, j] <- occS[,k-1,i, j] * survAloneS1[, k-1,i] * (1-occC[,k-1,i, j]) + 
          occS[,k-1,i, j] * survBothS1[, k-1,i]      * occC[,k-1,i, j] + 
          (1- occS[,k-1,i, j]) * colEmptyS1[, k-1,i] * (1-occC[,k-1,i, j]) +
          (1- occS[,k-1,i, j]) * colOccS1[, k-1,i]   * occC[,k-1,i, j]
        
        muC[,k-1,i, j] <- (1-occS[,k-1,i, j]) * survAloneC1[, k-1,i] * occC[,k-1,i, j] + 
                      occS[,k-1,i, j]      * survBothC1[,k-1,i] * occC[,k-1,i, j] + 
                      (1- occS[,k-1,i, j]) * colEmptyC1[, k-1,i] * (1-occC[,k-1,i, j]) +
                      occS[,k-1,i, j]      * colOccC1[, k-1,i]   * (1-occC[,k-1,i, j])
        
        # Determine occupancy
        occS[,k,i,j] <- rbinom(n.site, 1, prob = muS[,k-1,i,j])	# True occupancy Shenandoah sal
        occC[,k,i,j] <- rbinom(n.site, 1, prob = muC[,k-1,i,j])	# True occupancy Cinereus sal
        
      } # k = year
    } # j = nsim
  } # i = MCMC
  
  # Save simulation results as a list
  Biotic_pva <- list(occS = occS,
                     occC = occC)
  
  # Save simulation results in a folder
#  save(Biotic_pva, file = paste0("./PVA/Biotic/", name1, "_", "_pva.rda"))
  
}

# 6d. Null ------------------------------------------------------


if(length(which(name1 == "Null")) == 1){
  
  # Null
  load("./ModelOutput/null_output.rda")
  
  # Set parameter values
  alpha.psiS  <- null$sims$alpha.psiS
  alpha.psiC  <- null$sims$alpha.psiC
  alpha.survS <- null$sims$alpha.survS
  alpha.colS  <- null$sims$alpha.colS
  alpha.survC <- null$sims$alpha.survC
  alpha.colC  <- null$sims$alpha.colC

  # Create empty matricies to hold data
  # 4D: site x year x MCMC x simulation
  occS <- occC <- muS <- muC <- array(NA, dim = c(n.site, n.project, n.MCMC, n.sim))
  
  # 3D: site x year x MCMC
  survS <- colS <- survC <- colC <- array(NA, dim = c(n.site, n.project, n.MCMC))
  
  # 2D: sites x MCMC
  psiC <- psiS <- array(NA, dim = c(n.site, n.MCMC))
  
  # For loop to go over each MCMC iteration, site, and projection
  # For each MCMC iteration
  for(i in 1:n.MCMC){ 
    # Determine Pcin occupancy 1st year
    psiC[,i] <- plogis(alpha.psiC[i])
    
    # Determine Pshen occupancy 1st year
    psiS[,i] <- plogis(alpha.psiS[i]) 
    
    # For each year in the projection 
    for(k in 2:n.project){ 
      survS[, k-1,i] <- plogis(alpha.survS[i])
      
      colS[, k-1,i]  <- plogis(alpha.colS[i])
      
      survC[, k-1,i] <- plogis(alpha.survC[i])
      
      colC[, k-1,i]  <- plogis(alpha.colC[i])
      
    } # k close
    
    for(j in 1:n.sim){
      
      # Determine occupancy 1st year  
      occC[,1, i, j] <- rbinom(n.site, 1, prob = psiC[,i])
      occS[,1, i, j] <- rbinom(n.site, 1, prob = psiS[,i])	
      
      # For each year in the projection
      for(k in 2:n.project){ 
        
        muS[,k-1,i,j] <- occS[,k-1,i,j] * survS[, k-1,i] + 
          (1- occS[,k-1,i,j]) * colS[, k-1,i]
        
        muC[,k-1,i,j] <- survC[, k-1,i] * occC[,k-1,i,j] + 
          colC[, k-1,i] * (1-occC[,k-1,i,j])
        
        # Determine occupancy for entire projection
        occS[,k,i,j] <- rbinom(n.site, 1, prob = muS[,k-1,i,j])		# True occupancy Shenandoah sal
        occC[,k,i,j] <- rbinom(n.site, 1, prob = muC[,k-1,i,j])		# True occupancy Cinereus sal
      } # k closed
    } # j closed
  } # i closed
  
  # Save simulation results as a list
  Null_pva <- list(occS = occS,
                   occC = occC)
  
  # Save simulation results in a folder
 # save(Null_pva, file = paste0("./PVA/Null/", name1, "_pva.rda")) 
}



# 7. Calculate metrics -----------------------------------

# Regardless of model run- store the output into a new object names "pva"
pva <- get(paste0(name1, "_pva"))

# Melt the data - turn it into long format
PVA <- melt(pva)

# Rename the columns
colnames(PVA) <- c("site", "year", "MCMC", "sim", "value", "species")

# Summarize the data
# Extinction probability = the proportion of simulations in which the metapopulation reached zero occupied patches
# Population size = number of occupied cells = summed occupancy across sites
# Quasi-extinction = the proportion of simulations that went below some threshold patch occupancy (e.g., < 5 patches occupied)

# Set threshold
quasi_threshold_02_per <- 617 * 0.02
quasi_threshold_05_per <- 617 * 0.05
quasi_threshold_10_per <- 617 * 0.10

# Calculate for 2% quasi-extinction
metrics_02 <- PVA %>%
  group_by(year, species, sim, MCMC) %>%
  summarise(# If this == 1, then all sites are occupied
            prop_occ = sum(value)/length(value),
            # If this == 1, then all sites are unoccupied
            prop_unocc = length(which(value == 0))/length(value),  
            # Total number of occupied sites
            pop = sum(value)   
            ) %>%
  group_by(year, species, MCMC) %>%
    summarise(# Extinction prob
              # The proportion of times that all sites were unoccupied
              ext_prob = length(which(prop_unocc == 0))/length(prop_unocc),
              # Quasi-extinction
              # The proportion of times that occupancy went below a threshold
              quasi = length(which(prop_occ < quasi_threshold_02_per))/length(prop_occ),
              # minimum population size
              min_pop = min(pop)    
  ) %>%
  group_by(year, species) %>%
      summarise(# Extinction prob
            # The proportion of times that all sites were unoccupied
            ext_mean = mean(ext_prob),
            ext_q2.5 = quantile(ext_prob, prob = c(0.025, 0.975)[1]),
            ext_q97.5 = quantile(ext_prob, prob = c(0.025, 0.975)[2]),
            
            # Quasi-extinction
            # The proportion of times that occupancy went below a threshold
            quasi_mean = mean(quasi),
            quasi_q2.5 = quantile(quasi, prob = c(0.025, 0.975)[1]),
            quasi_q97.5 = quantile(quasi, prob = c(0.025, 0.975)[2]),
            
            # minimum population size
            min_pop_mean = mean(min_pop),
            min_pop_q2.5 = quantile(min_pop, prob = c(0.025, 0.975)[1]),
            min_pop_q97.5 = quantile(min_pop, prob = c(0.025, 0.975)[2])
        )
  

# Calculate for 5% quasi-extinction
metrics_05 <- PVA %>%
  group_by(year, species, sim, MCMC) %>%
  summarise(# If this == 1, then all sites are occupied
    prop_occ = sum(value)/length(value),
    # If this == 1, then all sites are unoccupied
    prop_unocc = length(which(value == 0))/length(value),  
    # Total number of occupied sites
    pop = sum(value)   
  ) %>%
  group_by(year, species, MCMC) %>%
  summarise(# Extinction prob
    # The proportion of times that all sites were unoccupied
    ext_prob = length(which(prop_unocc == 0))/length(prop_unocc),
    # Quasi-extinction
    # The proportion of times that occupancy went below a threshold
    quasi = length(which(prop_occ < quasi_threshold_05_per))/length(prop_occ),
    # minimum population size
    min_pop = min(pop)    
  ) %>%
  group_by(year, species) %>%
  summarise(# Extinction prob
    # The proportion of times that all sites were unoccupied
    ext_mean = mean(ext_prob),
    ext_q2.5 = quantile(ext_prob, prob = c(0.025, 0.975)[1]),
    ext_q97.5 = quantile(ext_prob, prob = c(0.025, 0.975)[2]),
    
    # Quasi-extinction
    # The proportion of times that occupancy went below a threshold
    quasi_mean = mean(quasi),
    quasi_q2.5 = quantile(quasi, prob = c(0.025, 0.975)[1]),
    quasi_q97.5 = quantile(quasi, prob = c(0.025, 0.975)[2]),
    
    # minimum population size
    min_pop_mean = mean(min_pop),
    min_pop_q2.5 = quantile(min_pop, prob = c(0.025, 0.975)[1]),
    min_pop_q97.5 = quantile(min_pop, prob = c(0.025, 0.975)[2])
  )

# Calculate for 2% quasi-extinction
metrics_10 <- PVA %>%
  group_by(year, species, sim, MCMC) %>%
  summarise(# If this == 1, then all sites are occupied
    prop_occ = sum(value)/length(value),
    # If this == 1, then all sites are unoccupied
    prop_unocc = length(which(value == 0))/length(value),  
    # Total number of occupied sites
    pop = sum(value)   
  ) %>%
  group_by(year, species, MCMC) %>%
  summarise(# Extinction prob
    # The proportion of times that all sites were unoccupied
    ext_prob = length(which(prop_unocc == 0))/length(prop_unocc),
    # Quasi-extinction
    # The proportion of times that occupancy went below a threshold
    quasi = length(which(prop_occ < quasi_threshold_10_per))/length(prop_occ),
    # minimum population size
    min_pop = min(pop)    
  ) %>%
  group_by(year, species) %>%
  summarise(# Extinction prob
    # The proportion of times that all sites were unoccupied
    ext_mean = mean(ext_prob),
    ext_q2.5 = quantile(ext_prob, prob = c(0.025, 0.975)[1]),
    ext_q97.5 = quantile(ext_prob, prob = c(0.025, 0.975)[2]),
    
    # Quasi-extinction
    # The proportion of times that occupancy went below a threshold
    quasi_mean = mean(quasi),
    quasi_q2.5 = quantile(quasi, prob = c(0.025, 0.975)[1]),
    quasi_q97.5 = quantile(quasi, prob = c(0.025, 0.975)[2]),
    
    # minimum population size
    min_pop_mean = mean(min_pop),
    min_pop_q2.5 = quantile(min_pop, prob = c(0.025, 0.975)[1]),
    min_pop_q97.5 = quantile(min_pop, prob = c(0.025, 0.975)[2])
  )

# 8. Calculate population size over time ----------------------------------

# Calculate the mean (and 95% CI) population size over time

pop_size <- PVA %>%
  group_by(year, species, sim, MCMC) %>%
  summarise(
    # Total number of occupied sites- for each simulation and MCMC run
    pop = sum(value)   
  ) %>%
  group_by(year, species) %>%
  summarise(# Total occupied sites
    # minimum population size
    pop_mean = mean(pop),
    pop_q2.5 = quantile(pop, prob = c(0.025, 0.975)[1]),
    pop_q97.5 = quantile(pop, prob = c(0.025, 0.975)[2])
  )

# 9. Save results ---------------------------------------------------------


# Save the results

if(name1 == "Biotic" | name1 == "Null"){
  save(pop_size, file = paste0("./PVA/", name1, "/", name1, "_pop_size.rda")) 

  save(metrics_02, file = paste0("./PVA/", name1, "/", name1, "_metrics_02_per.rda")) 
  save(metrics_05, file = paste0("./PVA/", name1, "/", name1, "_metrics_05_per.rda")) 
  save(metrics_10, file = paste0("./PVA/", name1, "/", name1, "_metrics_10_per.rda")) 
}

if(name1 == "BioticAbiotic" | name1 == "Abiotic"){
  save(pop_size, file = paste0("./PVA/", name1 , "/", name2, "_", name3, "/", name1, "_", name2, "_", name3,"_pop_size.rda")) 

  save(metrics_02, file = paste0("./PVA/", name1 , "/", name2, "_", name3, "/", name1, "_", name2, "_", name3,"_metrics_02_per.rda")) 
  save(metrics_05, file = paste0("./PVA/", name1 , "/", name2, "_", name3, "/", name1, "_", name2, "_", name3,"_metrics_05_per.rda")) 
  save(metrics_10, file = paste0("./PVA/", name1 , "/", name2, "_", name3, "/", name1, "_", name2, "_", name3,"_metrics_10_per.rda")) 
}

# End script