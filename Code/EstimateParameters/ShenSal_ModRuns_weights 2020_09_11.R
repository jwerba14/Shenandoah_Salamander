#####################################
##### The following R script was written by G. V. DiRenzo
### Please send questions to: grace.direnzo@gmail.com
#####################################


# Objective: 
  # 1. To determine the model structure that best predicts the data
      # To do this- we will be using Bayesian model updating
          # This method requires us to:
            # 0. Start with all models having equal weights = 0.25
            # 1. Fit the model with n years of our data
            # 2. Determine how well the model predicts the following years (n+1) data
            # 3. Update the model weights according to how well the model predicts the n+1 data
            # 4. Refit the model with n+1 years of data
            # 5. Repeat steps 2 & 3 with n + 2 years of data
            # 6. Refit the model with n+2 years of data
            # 7. Keep repeating these steps until N-1 years of data





####### ******* BEWARE *******  #######
  # there is no 2015 data but I wrote this code assuming that data were collected every year from 2007 - 2016. 
  # Code needs to be adjusted 
####### ********************* #######







#####################################
####### Table of contents ###########
#####################################

# 1. Set working directory
# 2. load packages & data
# 3. Bundle data & run first 5 years of data
# 4. Set MCMC settings
# 5. Run the model with 5 years of data 
# 6. Use 6 years of data
# 7. Use 7 years of data 
# 8. Use 8 years of data
# 9. Use 9 years of data
# 10. Determine the model weights
# 11. Make a plot

#####################################
#####################################
#####################################



# 1. Set working directory ------------------------------------------------




setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")




# 2. load packages & data --------------------------------------------------------




library(jagsUI)
library(beepr)
library(ggplot2)


# Load data
load("../../Data/formatted_shen_cin_dat.rda")
shen <- dat$shen 
cin <- dat$cin 

# Load temp/RMI data
load("../../Data/temp_rmi.rda")



# You can run sections 3, 4, 5, 6, 7, 8, 9 or just download the model outputs

load("../../ModelOutputs/first_5yrs_out.rda")
load("../../ModelOutputs/first_6yrs_out.rda")
load("../../ModelOutputs/first_7yrs_out.rda")
load("../../ModelOutputs/first_8yrs_out.rda")   # NOT CONVERGED - short run
load("../../ModelOutput/first_9yrs_out.rda")   # NOT CONVERGED - short run

# If downloaded - you will need to create the objects in each section ( 3 - 9) but don't run the models




# 3. Bundle data & run first 5 years of data ----------------------------------------------------------


# Create temperature file 
TEMP <- dat$site.temp$Tmax_p
mean_temp <- mean(TEMP)
sd_temp <- sd(TEMP)
std_temp <- (TEMP - mean_temp ) / sd_temp

RMI <- dat$site.RMI$RMI_p
mean_rmi <- mean(RMI,na.rm = TRUE)
sd_rmi <- sd(RMI,na.rm = TRUE)
std_rmi <- (RMI - mean_rmi ) / sd_rmi


#### Use the first 5 years of data to fit the parameters 1st
# 2007 - 2011

shen5 <- shen[,,1:5]
cin5 <- cin[,,1:5]

# Bundle data
win.data5 <- list(
  ## Parameter estimation
  N =  dim(shen5)[1],
  J =  dim(shen5)[2],
  Yr = dim(shen5)[3],
  
  AB.yS = shen5,
  A.yS  = shen5,
  B.yS  = shen5,
  N.yS  = shen5,
  
  AB.yC = cin5,
  A.yC  = cin5,
  B.yC  = cin5,
  N.yC  = cin5,
  
  TEMP = std_temp,
  RMI = std_rmi
)

# Look at structure
str(win.data5)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data5$AB.yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 0
zinit[zinit == "-Inf"] <- 0

# Take max value across surveys for each site and year combo
vinit <- apply(win.data5$AB.yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  


# Bundle initial values
inits <- function() {list(
  AB.z = zinit,
  A.z = zinit,
  B.z = zinit,
  N.z = zinit,
  
  AB.v = vinit,
  A.v = vinit,
  B.v = vinit,
  N.v = vinit,
  
  AB.alpha.pS= runif(1, -3, 3), 
  AB.alpha.rSC= runif(1, -3, 3),
  AB.alpha.rSc= runif(1, -3, 3), 
  AB.alpha.rC= runif(1, -3, 3), 
  AB.alpha.pC= runif(1, -3, 3),
  AB.alpha.psiSC= runif(1, -3, 3),       
  AB.alpha.psiSc= runif(1, -3, 3),       
  AB.alpha.psiC= runif(1, -3, 3),        
  AB.alpha.survAloneS= runif(1, -3, 3),            
  AB.alpha.survBothS= runif(1, -3, 3),
  AB.alpha.colEmptyS= runif(1, -3, 3),            
  AB.alpha.colOccS= runif(1, -3, 3),
  AB.beta.survAloneS1= runif(1, -3, 3),            
  AB.beta.survBothS1= runif(1, -3, 3),
  AB.beta.colEmptyS1= runif(1, -3, 3),            
  AB.beta.colOccS1= runif(1, -3, 3),
  AB.beta.survAloneS2= runif(1, -3, 3),            
  AB.beta.survBothS2= runif(1, -3, 3),
  AB.beta.colEmptyS2= runif(1, -3, 3),            
  AB.beta.colOccS2= runif(1, -3, 3),
  AB.alpha.survAloneC= runif(1, -3, 3),            
  AB.alpha.survBothC= runif(1, -3, 3),
  AB.alpha.colEmptyC= runif(1, -3, 3),            
  AB.alpha.colOccC= runif(1, -3, 3),
  AB.beta.survAloneC1= runif(1, -3, 3),            
  AB.beta.survBothC1= runif(1, -3, 3),
  AB.beta.colEmptyC1= runif(1, -3, 3),            
  AB.beta.colOccC1= runif(1, -3, 3),
  AB.beta.survAloneC2= runif(1, -3, 3),            
  AB.beta.survBothC2= runif(1, -3, 3),
  AB.beta.colEmptyC2= runif(1, -3, 3),            
  AB.beta.colOccC2= runif(1, -3, 3),
  
  # Abiotic
  A.alpha.pS = runif(1, -3, 3), 
  A.alpha.pC = runif(1, -3, 3),
  
  A.alpha.psiS= runif(1, -3, 3),       
  A.alpha.psiC= runif(1, -3, 3), 
  
  A.alpha.survS= runif(1, -3, 3),           
  A.alpha.colS= runif(1, -3, 3),           
  A.beta.survS1= runif(1, -3, 3),
  A.beta.colS1= runif(1, -3, 3), 
  A.beta.survS2= runif(1, -3, 3),
  A.beta.colS2= runif(1, -3, 3), 
  
  A.alpha.survC= runif(1, -3, 3),
  A.alpha.colC= runif(1, -3, 3), 
  A.beta.survC1= runif(1, -3, 3), 
  A.beta.colC1= runif(1, -3, 3),  
  A.beta.survC2= runif(1, -3, 3), 
  A.beta.colC2= runif(1, -3, 3),
  
  # Biotic
  B.alpha.pS= runif(1, -3, 3), 
  B.alpha.rSC= runif(1, -3, 3), 
  B.alpha.rSc= runif(1, -3, 3), 
  B.alpha.rC= runif(1, -3, 3), 
  B.alpha.pC= runif(1, -3, 3),
  B.alpha.psiSC= runif(1, -3, 3),       
  B.alpha.psiSc= runif(1, -3, 3),       
  B.alpha.psiC= runif(1, -3, 3),        
  B.alpha.survAloneS= runif(1, -3, 3),            
  B.alpha.survBothS= runif(1, -3, 3),
  B.alpha.colEmptyS= runif(1, -3, 3),            
  B.alpha.colOccS= runif(1, -3, 3),
  B.alpha.survAloneC= runif(1, -3, 3),           
  B.alpha.survBothC= runif(1, -3, 3),
  B.alpha.colEmptyC= runif(1, -3, 3),            
  B.alpha.colOccC= runif(1, -3, 3),
  
  # Null
  N.alpha.pS = runif(1, -3, 3), 
  N.alpha.pC= runif(1, -3, 3),
  N.alpha.psiS= runif(1, -3, 3),       
  N.alpha.psiC= runif(1, -3, 3),         
  N.alpha.survS= runif(1, -3, 3),      
  N.alpha.colS= runif(1, -3, 3),     
  N.alpha.survC= runif(1, -3, 3),      
  N.alpha.colC= runif(1, -3, 3)
  
)}


# Parameters to monitor
params <- c(# Abiotic biotic
  "AB.alpha.pS", "AB.alpha.rSC","AB.alpha.rSc", "AB.alpha.rC", "AB.alpha.pC",
  "AB.alpha.psiSC",       
  "AB.alpha.psiSc",       
  "AB.alpha.psiC",        
  "AB.alpha.survAloneS",            "AB.alpha.survBothS",
  "AB.alpha.colEmptyS",            "AB.alpha.colOccS",
  "AB.beta.survAloneS1",            "AB.beta.survBothS1",
  "AB.beta.colEmptyS1",            "AB.beta.colOccS1",
  "AB.beta.survAloneS2",            "AB.beta.survBothS2",
  "AB.beta.colEmptyS2",            "AB.beta.colOccS2",
  "AB.alpha.survAloneC",            "AB.alpha.survBothC",
  "AB.alpha.colEmptyC",            "AB.alpha.colOccC",
  "AB.beta.survAloneC1",            "AB.beta.survBothC1",
  "AB.beta.colEmptyC1",            "AB.beta.colOccC1",
  "AB.beta.survAloneC2",            "AB.beta.survBothC2",
  "AB.beta.colEmptyC2",            "AB.beta.colOccC2",
  
  # Abiotic
  "A.alpha.pS", "A.alpha.pC",
  
  "A.alpha.psiS",       
  "A.alpha.psiC", 
  
  "A.alpha.survS",           
  "A.alpha.colS",           
  "A.beta.survS1",
  "A.beta.colS1", 
  "A.beta.survS2",
  "A.beta.colS2", 
  
  "A.alpha.survC",
  "A.alpha.colC", 
  "A.beta.survC1", 
  "A.beta.colC1",  
  "A.beta.survC2", 
  "A.beta.colC2",
  
  # Biotic
  "B.alpha.pS", "B.alpha.rSC", "B.alpha.rSc", "B.alpha.rC", "B.alpha.pC",
  "B.alpha.psiSC",       
  "B.alpha.psiSc",       
  "B.alpha.psiC",        
  "B.alpha.survAloneS",            "B.alpha.survBothS",
  "B.alpha.colEmptyS",            "B.alpha.colOccS",
  "B.alpha.survAloneC",            "B.alpha.survBothC",
  "B.alpha.colEmptyC",            "B.alpha.colOccC",
  
  # Null
  "N.alpha.pS", "N.alpha.pC",
  "N.alpha.psiS",       
  "N.alpha.psiC",         
  "N.alpha.survS",      
  "N.alpha.colS",     
  "N.alpha.survC",      
  "N.alpha.colC")




# 4. Set MCMC settings ----------------------------------------------------------




# MCMC settings
ni <- 5000
nb <- 2000
nt <- 10
nc <- 3
na <- 10000
mi <- 1e6




# 5. Run the model with 5 years of data ----------------------------------------------------------


## Run model
out5 <- autojags(data = win.data5, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/all_models.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                iter.increment = ni,
                parallel = TRUE)

#out5 <- jags(data = win.data5, inits = inits,  
#                parameters.to.save = params, 
#                model.file = "./Models/all_models.txt", 
#                n.chains = 3, 
#                n.thin = 1, 
#                n.burnin = 1, 
#                n.adapt = 1,
#                n.iter = 2,
#                parallel = TRUE)
#

beepr::beep(2)


# Save the output
save(out5, file = "./ModelOutput/first_5yrs_out.rda")




# 6. Use 6 years of data ----------------------------------------------------------


#### Use the first 6 years of data to fit the parameters
# 2007 - 2012

shen6 <- shen[,,1:6]
cin6 <- cin[,,1:6]

# Bundle data
win.data6 <- list(
  ## Parameter estimation
  N =  dim(shen6)[1],
  J =  dim(shen6)[2],
  Yr = dim(shen6)[3],
  
  AB.yS = shen6,
  A.yS  = shen6,
  B.yS  = shen6,
  N.yS  = shen6,
  
  AB.yC = cin6,
  A.yC  = cin6,
  B.yC  = cin6,
  N.yC  = cin6,
  
  TEMP = std_temp,
  RMI = std_rmi
)

# Look at structure
str(win.data6)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data6$AB.yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 0
zinit[zinit == "-Inf"] <- 0

# Take max value across surveys for each site and year combo
vinit <- apply(win.data6$AB.yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  

## Run model
out6 <- autojags(data = win.data6, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/all_models.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                iter.increment = ni,
                parallel = TRUE)

#out6 <- jags(data = win.data6, inits = inits,  
#             parameters.to.save = params, 
#             model.file = "./Models/all_models.txt", 
#             n.chains = 3, 
#             n.thin = 1, 
#             n.burnin = 1, 
#             n.adapt = 1,
#             n.iter = 2,
#             parallel = TRUE)
#

beepr::beep(2)


# Save the output
save(out6, file = "./ModelOutput/first_6yrs_out.rda")



#load ("../../ModelOutputs/first_6yrs_out.rda")


# 7. Use 7 years of data ----------------------------------------------------------


#### Use the first 7 years of data to fit the parameters
# 2007 - 2013

shen7 <- shen[,,1:7]
cin7 <- cin[,,1:7]

# Bundle data
win.data7 <- list(
  ## Parameter estimation
  N =  dim(shen7)[1],
  J =  dim(shen7)[2],
  Yr = dim(shen7)[3],
  
  AB.yS = shen7,
  A.yS  = shen7,
  B.yS  = shen7,
  N.yS  = shen7,
  
  AB.yC = cin7,
  A.yC  = cin7,
  B.yC  = cin7,
  N.yC  = cin7,
  
  TEMP = std_temp,
  RMI = std_rmi
)

# Look at structure
str(win.data7)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data7$AB.yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 0
zinit[zinit == "-Inf"] <- 0

# Take max value across surveys for each site and year combo
vinit <- apply(win.data7$AB.yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  

# Run model
out7 <- autojags(data = win.data7, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/all_models.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                iter.increment = ni,
                parallel = TRUE)
<<<<<<< HEAD

#out7 <- jags(data = win.data7, inits = inits,  
=======

#out7 <- jags(data = win.data7, inits = inits,  
#             parameters.to.save = params, 
#             model.file = "./Models/all_models.txt", 
#             n.chains = 3, 
#             n.thin = 1, 
#             n.burnin = 1, 
#             n.adapt = 1,
#             n.iter = 2,
#             parallel = TRUE)
#

beepr::beep(2)


# Save the output
save(out7, file = "./ModelOutput/first_7yrs_out.rda")






# 8. Use 8 years of data ----------------------------------------------------------


#### Use the first 8 years of data to fit the parameters
# 2007 - 2014

shen8 <- shen[,,1:8]
cin8 <- cin[,,1:8]

# Bundle data
win.data8 <- list(
  ## Parameter estimation
  N =  dim(shen8)[1],
  J =  dim(shen8)[2],
  Yr = dim(shen8)[3],
  
  AB.yS = shen8,
  A.yS  = shen8,
  B.yS  = shen8,
  N.yS  = shen8,
  
  AB.yC = cin8,
  A.yC  = cin8,
  B.yC  = cin8,
  N.yC  = cin8,
  
  TEMP = std_temp,
  RMI = std_rmi
)

# Look at structure
str(win.data8)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data8$AB.yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 0
zinit[zinit == "-Inf"] <- 0

# Take max value across surveys for each site and year combo
vinit <- apply(win.data8$AB.yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  

## Run model
out8 <- autojags(data = win.data8, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/all_models.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                iter.increment = ni,
                parallel = TRUE)

#out8 <- jags(data = win.data8, inits = inits,  

#             parameters.to.save = params, 
#             model.file = "./Models/all_models.txt", 
#             n.chains = 3, 
#             n.thin = 1, 
#             n.burnin = 1, 
#             n.adapt = 1,
#             n.iter = 2,
#             parallel = TRUE)
#

beepr::beep(2)


# Save the output

save(out7, file = "./ModelOutput/first_7yrs_out.rda")






# 8. Use 8 years of data ----------------------------------------------------------


#### Use the first 8 years of data to fit the parameters
# 2007 - 2014

shen8 <- shen[,,1:8]
cin8 <- cin[,,1:8]

# Bundle data
win.data8 <- list(
  ## Parameter estimation
  N =  dim(shen8)[1],
  J =  dim(shen8)[2],
  Yr = dim(shen8)[3],
  
  AB.yS = shen8,
  A.yS  = shen8,
  B.yS  = shen8,
  N.yS  = shen8,
  
  AB.yC = cin8,
  A.yC  = cin8,
  B.yC  = cin8,
  N.yC  = cin8,
  
  TEMP = std_temp,
  RMI = std_rmi
)

# Look at structure
str(win.data8)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data8$AB.yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 0
zinit[zinit == "-Inf"] <- 0

# Take max value across surveys for each site and year combo
vinit <- apply(win.data8$AB.yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  

# MCMC settings
ni <- 1500
nb <- 500
nt <- 1
nc <- 3
na <- 10000
mi <- 1e6



## Run model
mod8 <- jags.model(file = "../../models/all_models.txt",  data = win.data8,
                   n.chains = 3, inits = inits,
                   n.adapt = 1000)

out8 <- jags(model.file = "../../models/all_models.txt", data = win.data8,
             inits = inits, parameters.to.save = params, n.iter = 5000, n.chains = 3 )

                plot(#progress.bar = "text")
)
#out8 <- jags(data = win.data8, inits = inits,  
#             parameters.to.save = params, 
#             model.file = "./Models/all_models.txt", 
#             n.chains = 3, 
#             n.thin = 1, 
#             n.burnin = 1, 
#             n.adapt = 1,
#             n.iter = 2,
#             parallel = TRUE)
#

beepr::beep(2)


# Save the output

save(out8, file = "./ModelOutput/first_8yrs_out.rda")





# 9. Use 9 years of data ----------------------------------------------------------


#### Use the first 9 years of data to fit the parameters
# 2007 - 2015 - NO 2015 data collected !!!!

shen9 <- shen[,,1:9]
cin9 <- cin[,,1:9]

# Bundle data
win.data9 <- list(
  ## Parameter estimation
  N =  dim(shen9)[1],
  J =  dim(shen9)[2],
  Yr = dim(shen9)[3],
  
  AB.yS = shen9,
  A.yS  = shen9,
  B.yS  = shen9,
  N.yS  = shen9,
  
  AB.yC = cin9,
  A.yC  = cin9,
  B.yC  = cin9,
  N.yC  = cin9,
  
  TEMP = std_temp,
  RMI = std_rmi
)

# Look at structure
str(win.data9)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data9$AB.yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 0
zinit[zinit == "-Inf"] <- 0

# Take max value across surveys for each site and year combo
vinit <- apply(win.data9$AB.yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  

## Run model
out9 <- autojags(data = win.data9, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/all_models.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                iter.increment = ni,
                parallel = TRUE)

#out9 <- jags(data = win.data9, inits = inits,  
#             parameters.to.save = params, 
#             model.file = "./Models/all_models.txt", 
#             n.chains = 3, 
#             n.thin = 1, 
#             n.burnin = 1, 
#             n.adapt = 1,
#             n.iter = 2,
#             parallel = TRUE)


beepr::beep(2)


# Save the output
save(out9, file = "./ModelOutput/first_9yrs_out.rda")




# 10. Determine the model weights ----------------------------------------------------------




# Empty array to hold model weights
  # This will hold the weights for each year of data not fit in the model yet = 5
  # Start with equal weights = 0.25
  # First model fit with 5 years of data: 2007 - 2011
    # Then, predictions will be made for 2012, 2013, 2014, 2015, 2016 with the respective models
# This will hold the model weights
AB.w <- A.w <- B.w <- N.w <- array(NA, dim = c(win.data5$N, win.data5$J, 6))

# Need another object to hold the 
AB.WP <- A.WP <- B.WP <- N.WP <- array(NA, dim = c(win.data5$N, win.data5$J, 5))

# Equal initial model weights
  # Then it will be updated for each year of predictions 2012 - 2016
AB.w[,,1] <- A.w[,,1] <- B.w[,,1] <- N.w[,,1] <- 1/4


# Need to calculate the probability of detecting Pshen at a site for each model

# To do this, we need to simulate Pshen & Pcin presence/absence under each model
  # There are 4 functions to do this
    # Abiotic Biotic = ./Code/Functions/AB_fun.R
    # Abioti = ./Code/Functions/A_fun.R
    # Biotic = ./Code/Functions/B_fun.R
    # Null = ./Code/Functions/N_fun.R
  # The output of these functions is the probability of detecting Pshen at each site, survey, year combination


source("../Code/Functions/AB_fun.R")    

AB.pred.6 <- AB.fun(n.year = 6,
                   out = out5,
                   win.data = win.data5)
AB.pred.7 <- AB.fun(n.year = 7,
                   out = out6,
                   win.data = win.data6)
AB.pred.8 <- AB.fun(n.year = 8,
                   out = out7,
                   win.data = win.data7)
AB.pred.9 <- AB.fun(n.year = 9,
                   out = out8,
                   win.data = win.data8)
AB.pred.10 <- AB.fun(n.year = 10,
                   out = out9,
                   win.data = win.data9)

source("../Code/Functions/A_fun.R")    

A.pred.6 <- A.fun(n.year = 6,
                   out = out5,
                   win.data = win.data5)
A.pred.7 <- A.fun(n.year = 7,
                   out = out6,
                   win.data = win.data6)
A.pred.8 <- A.fun(n.year = 8,
                   out = out7,
                   win.data = win.data7)
A.pred.9 <- A.fun(n.year = 9,
                   out = out8,
                   win.data = win.data8)
A.pred.10 <- A.fun(n.year = 10,
                    out = out9,
                    win.data = win.data9)


source("../Functions/B_fun.R")    

B.pred.6 <- B.fun(n.year = 6,
                 out = out5,
                 win.data = win.data5)
B.pred.7 <- B.fun(n.year = 7,
                   out = out6,
                   win.data = win.data6)
B.pred.8 <- B.fun(n.year = 8,
                   out = out7,
                   win.data = win.data7)
B.pred.9 <- B.fun(n.year = 9,
                   out = out8,
                   win.data = win.data8)
B.pred.10 <- B.fun(n.year = 10,
                    out = out9,
                    win.data = win.data9)


source("../Functions/N_fun.R")    

N.pred.6 <- N.fun(n.year = 6,
                 out = out5,
                 win.data = win.data5)
N.pred.7 <- N.fun(n.year = 7,
                   out = out6,
                   win.data = win.data6)
N.pred.8 <- N.fun(n.year = 8,
                   out = out7,
                   win.data = win.data7)
N.pred.9 <- N.fun(n.year = 9,
                   out = out8,
                   win.data = win.data8)
N.pred.10 <- N.fun(n.year = 10,
                    out = out9,
                    win.data = win.data9)

# At this point, you have the probability of detecting Pshen at each site, survey, year from 1 to n + 1 years of data under each model   

# Calculate the likelihood of observing your data with the probabilities predicted by each model
AB.mod.prob.6 <- pbinom(shen[,,6], 1, prob = AB.pred.6$AB.eff.pS[,,6])
A.mod.prob.6  <- pbinom(shen[,,6], 1, prob =  A.pred.6$A.eff.pS[,,6])
B.mod.prob.6  <- pbinom(shen[,,6], 1, prob =  B.pred.6$B.eff.pS[,,6])
N.mod.prob.6  <- pbinom(shen[,,6], 1, prob =  N.pred.6$N.eff.pS[,,6])

AB.mod.prob.7 <- pbinom(shen[,,7], 1, prob = AB.pred.7$AB.eff.pS[,,7])
A.mod.prob.7  <- pbinom(shen[,,7], 1, prob =  A.pred.7$A.eff.pS[,,7])
B.mod.prob.7  <- pbinom(shen[,,7], 1, prob =  B.pred.7$B.eff.pS[,,7])
N.mod.prob.7  <- pbinom(shen[,,7], 1, prob =  N.pred.7$N.eff.pS[,,7])

AB.mod.prob.8 <- pbinom(shen[,,8], 1, prob = AB.pred.8$AB.eff.pS[,,8])
A.mod.prob.8  <- pbinom(shen[,,8], 1, prob =  A.pred.8$A.eff.pS[,,8])
B.mod.prob.8  <- pbinom(shen[,,8], 1, prob =  B.pred.8$B.eff.pS[,,8])
N.mod.prob.8  <- pbinom(shen[,,8], 1, prob =  N.pred.8$N.eff.pS[,,8])

AB.mod.prob.9 <- pbinom(shen[,,9], 1, prob = AB.pred.9$AB.eff.pS[,,9])
A.mod.prob.9  <- pbinom(shen[,,9], 1, prob =  A.pred.9$A.eff.pS[,,9])
B.mod.prob.9  <- pbinom(shen[,,9], 1, prob =  B.pred.9$B.eff.pS[,,9])
N.mod.prob.9  <- pbinom(shen[,,9], 1, prob =  N.pred.9$N.eff.pS[,,9])

AB.mod.prob.10 <- pbinom(shen[,,10], 1, prob = AB.pred.10$AB.eff.pS[,,10])
A.mod.prob.10  <- pbinom(shen[,,10], 1, prob =  A.pred.10$A.eff.pS[,,10])
B.mod.prob.10  <- pbinom(shen[,,10], 1, prob =  B.pred.10$B.eff.pS[,,10])
N.mod.prob.10  <- pbinom(shen[,,10], 1, prob =  N.pred.10$N.eff.pS[,,10])


# Update the model weights by multiplying the previous years weight by likelihood

# 2012 predictions - update model weights
AB.WP[,,1] <- AB.w[,,1] * AB.mod.prob.6
A.WP[,,1] <-   A.w[,,1] * A.mod.prob.6
B.WP[,,1] <-   B.w[,,1] * B.mod.prob.6
N.WP[,,1] <-   N.w[,,1] * N.mod.prob.6


# Updating the model weights via Bayes' Theorem ******
AB.w[,,2]  <- AB.WP[,,1] / (AB.WP[,,1] + A.WP[,,1] + B.WP[,,1]+ N.WP[,,1])
A.w[,,2]   <- A.WP[,,1]  / (AB.WP[,,1] + A.WP[,,1] + B.WP[,,1]+ N.WP[,,1])
B.w[,,2]   <- B.WP[,,1]  / (AB.WP[,,1] + A.WP[,,1] + B.WP[,,1]+ N.WP[,,1])
N.w[,,2]   <- N.WP[,,1]  / (AB.WP[,,1] + A.WP[,,1] + B.WP[,,1]+ N.WP[,,1])


# 2013 predictions - update model weights
AB.WP[,,2] <- AB.w[,,2] * AB.mod.prob.7
A.WP[,,2] <-   A.w[,,2] * A.mod.prob.7
B.WP[,,2] <-   B.w[,,2] * B.mod.prob.7
N.WP[,,2] <-   N.w[,,2] * N.mod.prob.7


# Updating the model weights via Bayes' Theorem ******
AB.w[,,3]  <- AB.WP[,,2] / (AB.WP[,,2] + A.WP[,,2] + B.WP[,,2]+ N.WP[,,2])
 A.w[,,3]   <- A.WP[,,2] / (AB.WP[,,2] + A.WP[,,2] + B.WP[,,2]+ N.WP[,,2])
 B.w[,,3]   <- B.WP[,,2] / (AB.WP[,,2] + A.WP[,,2] + B.WP[,,2]+ N.WP[,,2])
 N.w[,,3]   <- N.WP[,,2] / (AB.WP[,,2] + A.WP[,,2] + B.WP[,,2]+ N.WP[,,2])



# 2014 predictions - update model weights
AB.WP[,,3] <- AB.w[,,3] * AB.mod.prob.8
A.WP[,,3] <-   A.w[,,3] * A.mod.prob.8
B.WP[,,3] <-   B.w[,,3] * B.mod.prob.8
N.WP[,,3] <-   N.w[,,3] * N.mod.prob.8


# Updating the model weights via Bayes' Theorem ******
AB.w[,,4]  <- AB.WP[,,3] / (AB.WP[,,3] + A.WP[,,3] + B.WP[,,3]+ N.WP[,,3])
 A.w[,,4]   <- A.WP[,,3] / (AB.WP[,,3] + A.WP[,,3] + B.WP[,,3]+ N.WP[,,3])
 B.w[,,4]   <- B.WP[,,3] / (AB.WP[,,3] + A.WP[,,3] + B.WP[,,3]+ N.WP[,,3])
 N.w[,,4]   <- N.WP[,,3] / (AB.WP[,,3] + A.WP[,,3] + B.WP[,,3]+ N.WP[,,3])


# NO SURVEYS in 2015
#AB.WP[,,4] <- AB.w[,,4] * AB.mod.prob.9
#A.WP[,,4] <-   A.w[,,4] * A.mod.prob.9
#B.WP[,,4] <-   B.w[,,4] * B.mod.prob.9
#N.WP[,,4] <-   N.w[,,4] * N.mod.prob.9

# 2015 predictions - update model weights
AB.WP[,,5] <- AB.w[,,4] * AB.mod.prob.10
A.WP[,,5] <-   A.w[,,4] * A.mod.prob.10
B.WP[,,5] <-   B.w[,,4] * B.mod.prob.10
N.WP[,,5] <-   N.w[,,4] * N.mod.prob.10


# Updating the model weights via Bayes' Theorem ******
AB.w[,,6]  <- AB.WP[,,5] / (AB.WP[,,5] + A.WP[,,5] + B.WP[,,5]+ N.WP[,,5])
 A.w[,,6]   <- A.WP[,,5] / (AB.WP[,,5] + A.WP[,,5] + B.WP[,,5]+ N.WP[,,5])
 B.w[,,6]   <- B.WP[,,5] / (AB.WP[,,5] + A.WP[,,5] + B.WP[,,5]+ N.WP[,,5])
 N.w[,,6]   <- N.WP[,,5] / (AB.WP[,,5] + A.WP[,,5] + B.WP[,,5]+ N.WP[,,5])




# Calculate the mean weight each year
AB.w.mean <- apply(AB.w, c(3), mean, na.rm = TRUE)
A.w.mean <- apply(A.w, c(3), mean, na.rm = TRUE)
B.w.mean <- apply(B.w, c(3), mean, na.rm = TRUE)
N.w.mean <- apply(N.w, c(3), mean, na.rm = TRUE)


# Put the data together to make a plot
weights <- data.frame(year = rep(2011:2016, times = 4),
                      weights = c(AB.w.mean, 
                                  A.w.mean,
                                  B.w.mean,
                                  N.w.mean),
                      Model = c(rep("Abiotic \nBiotic", times = length(AB.w.mean)),
                                rep("Abiotic", times = length(A.w.mean)),
                                rep("Biotic", times = length(B.w.mean)),
                                rep("Null", times = length(N.w.mean))))




# 11. Make a plot ---------------------------------------------------------



# Make a plot
ggplot(data = weights, aes(x = year, y = weights, col = Model))+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  scale_color_manual(values = c("black", "goldenrod3", "skyblue3", "deeppink3")) +
  ylab("Model weights")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "white"))



# End script
