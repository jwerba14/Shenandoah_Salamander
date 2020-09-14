##### The following R script was written by G. V. DiRenzo
### Please send questions to: grace.direnzo@gmail.com

# Objective: 
  # 1. To run 4 models (abiotic/biotic, abiotic, biotic, null) with model weighting and estimate parameters
  # 2. To create the master table with all parameter values

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
load("./Data/formatted_shen_cin_dat.rda")
shen <- dat$shen 
cin <- dat$cin 

# Load temp/RMI data
load("./Data/temp_rmi.rda")




# 3. Bundle data & run Abiotic/Biotic model ----------------------------------------------------------



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





# 4. Set MCMC settings ----------------------------------------------------------




# MCMC settings
ni <- 5000
nb <- 2000
nt <- 10
nc <- 3
na <- 10000
mi <- 1e6



# 5. Run the model ----------------------------------------------------------


## Run model
#out5 <- autojags(data = win.data, inits = inits,  
#                parameters.to.save = params, 
#                model.file = "./Models/model_weights.txt", 
#                n.chains = nc, 
#                n.thin = nt, 
#                n.burnin = nb, 
#                n.adapt = na,
#                max.iter = mi,
#                iter.increment = ni,
#                parallel = TRUE)
#
out5 <- jags(data = win.data5, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/all_models.txt", 
                n.chains = 3, 
                n.thin = 1, 
                n.burnin = 1, 
                n.adapt = 1,
                n.iter = 2,
                parallel = TRUE)


beepr::beep(2)


# Save the output
save(out5, file = "./ModelOutput/first_5yrs_out.rda")



# 6. Determine the model weights ----------------------------------------------------------


# Empty array to hold model weights
AB.w <- A.w <- B.w <- N.w <- array(NA, dim = c(win.data$N, win.data$J, 5))

# Equal initial model weights
AB.w[,,1] <- A.w[,,1] <- B.w[,,1] <- N.w[,,1] <- 1/4


# Calculate the probability of observing Pshen at a site for each model

# First need to simulate Pshen & Pcin presence/absence

# ------------------------------------------------------------------------
#----------------------------- Abiotic biotic-----------------------------

n.site <- nrow(shen)
n.year <- 6

# Create empty matricies to hold data
# 2D: site x year
AB.occS <- AB.occC <- AB.muS <- AB.muC <- array(NA, dim = c(n.site, n.year))

# 2D: site x year 
AB.survAloneS1 <- AB.survBothS1 <- AB.colEmptyS1 <- AB.colOccS1 <- AB.survAloneC1 <- AB.survBothC1 <- AB.colEmptyC1 <- AB.colOccC1 <- array(NA, dim = c(n.site, n.year))


# Initial occupancy in 2007
  AB.psiC1 <-  plogis(out5$mean$AB.alpha.psiC)
  AB.psiSC1 <- plogis(out5$mean$AB.alpha.psiSC)
  AB.psiSc1 <- plogis(out5$mean$AB.alpha.psiSc) 
  
    
# Set the colonization/extinction probabilities
    AB.survAloneS1 <- plogis(out5$mean$AB.alpha.survAloneS + 
                          out5$mean$AB.beta.survAloneS1 * win.data$TEMP + 
                          out5$mean$AB.beta.survAloneS2 * win.data$RMI)
    AB.survBothS1 <- plogis(out5$mean$AB.alpha.survBothS + 
                         out5$mean$AB.beta.survBothS1 * win.data$TEMP + 
                         out5$mean$AB.beta.survBothS2 * win.data$RMI)
    AB.colEmptyS1 <- plogis(out5$mean$AB.alpha.colEmptyS + 
                         out5$mean$AB.beta.colEmptyS1 * win.data$TEMP + 
                         out5$mean$AB.beta.colEmptyS2 * win.data$RMI)
    AB.colOccS1   <- plogis(out5$mean$AB.alpha.colOccS + 
                         out5$mean$AB.beta.colOccS1 * win.data$TEMP +
                         out5$mean$AB.beta.colOccS2 * win.data$RMI)
    AB.survAloneC1 <- plogis(out5$mean$AB.alpha.survAloneC + 
                          out5$mean$AB.beta.survAloneC1 * win.data$TEMP + 
                          out5$mean$AB.beta.survAloneC2 * win.data$RMI)
    AB.survBothC1 <- plogis(out5$mean$AB.alpha.survBothC + 
                         out5$mean$AB.beta.survBothC1 * win.data$TEMP + 
                         out5$mean$AB.beta.survBothC2 * win.data$RMI)
    AB.colEmptyC1  <- plogis(out5$mean$AB.alpha.colEmptyC + 
                          out5$mean$AB.beta.colEmptyC1 * win.data$TEMP + 
                          out5$mean$AB.beta.colEmptyC2 * win.data$RMI)
    AB.colOccC1   <- plogis(out5$mean$AB.alpha.colOccC + 
                         out5$mean$AB.beta.colOccC1 * win.data$TEMP + 
                         out5$mean$AB.beta.colOccC2 * win.data$RMI) 
    
    AB.pC <- plogis(out5$mean$AB.alpha.pC)
    AB.rC <- plogis(out5$mean$AB.alpha.rC)
    
    AB.pS  <- plogis(out5$mean$AB.alpha.pS)
    AB.rSC <- plogis(out5$mean$AB.alpha.rSC)
    AB.rSc <- plogis(out5$mean$AB.alpha.rSc)
    
    
# Determine Pcin occupancy in 2007
    AB.occC[,1] <- rbinom(n.site, 1, prob = AB.psiC1)
    

# Based on Pcin occupancy, determine shenandoah probability of occupancy
    AB.psiS1 <- AB.psiSC1 * AB.occC + 
      AB.psiSc1 * (1 - AB.occC)
    
    # Determine Pshen occupancy 1st year
    AB.occS[ , 1] <- rbinom(n.site, 1, prob = AB.psiS1)	
    
# For each year in the projection
for(k in 2:n.year){ 
      
      # Determine probabilities 
  AB.muS[,k-1] <- AB.occS[,k-1] * AB.AB.survAloneS1 * (1-AB.occC[,k-1]) + 
    AB.occS[,k-1] * AB.survBothS1      * AB.occC[,k-1] + 
                    (1- AB.occS[,k-1]) * AB.colEmptyS1 * (1-AB.occC[,k-1]) +
                    (1- AB.occS[,k-1]) * AB.colOccS1   * AB.occC[,k-1]
      
  AB.muC[,k-1] <- (1-AB.occS[,k-1]) * AB.survAloneC1 * AB.occC[,k-1] + 
    AB.occS[,k-1]      * AB.survBothC1 * AB.occC[,k-1] + 
                   (1- AB.occS[,k-1]) * AB.colEmptyC1 * (1-AB.occC[,k-1]) +
    AB.occS[,k-1]      * AB.colOccC1   * (1-AB.occC[,k-1])
      
      # Determine occupancy
      AB.occS[,k] <- rbinom(n.site, 1, prob = AB.muS[,k-1])	# True occupancy Shenandoah sal
      AB.occC[,k] <- rbinom(n.site, 1, prob = AB.muC[,k-1])	# True occupancy Cinereus sal

} # k = year

    AB.eff.pS <- array(NA, dim = c(win.data5$n.site))
    
for(k in 1:Yr) {  # Loop over years
    for(j in 1:J) { # Loop over replicates
      
      # Conditional detection probabilities
      AB.eff.pS[,j,k] <- AB.occS[i,k] * AB.pS[i,j,k] * (1 - AB.occC[i,k]) +
                          AB.occS[i,k] * AB.rSC[i,j,k] * AB.occC[i,k] * win.data5$AB.yC[i,j,k] +
                          AB.occS[i,k] * AB.rSc[i,j,k] * AB.occC[i,k] * (1-win.data5$AB.yC[i,j,k])
      
      # Conditional detection probabilities
      AB.eff.pC[i,j,k] <- (1 - AB.z[i,k]) * AB.pC[i,j,k] * AB.v[i,k] +
        AB.z[i,k] * AB.rC[i,j,k] * AB.v[i,k]
      
      
      # Observation model for the actual observations
      AB.yS[,j,k] ~ dbern(AB.eff.pS[i,j,k])
      
      # Observation model for the actual observations
      AB.yC[i,j,k] ~ dbern(AB.eff.pC[i,j,k])
      

      
      

      
    }
  }
}
    
    
    


# Calculate the likelihood of observing your data
AB.mod.prob <- pbinom(shen[,,6], 1, prob = plogis(out$mean$AB.alpha.pS))
A.mod.prob  <- pbinom(shen[,,6], 1, prob = plogis(out$mean$A.alpha.pS))
B.mod.prob  <- pbinom(shen[,,6], 1, prob = plogis(out$mean$B.alpha.pS))
N.mod.prob  <- pbinom(shen[,,6], 1, prob = plogis(out$mean$N.alpha.pS))


# Update the model weights
AB.w[,,2] <- AB.w[,,1] * AB.mod.prob
A.w[,,2] <-   A.w[,,1] * A.mod.prob
B.w[,,2] <-   B.w[,,1] * B.mod.prob
N.w[,,2] <-   N.w[,,1] * N.mod.prob



# 7. Extract parameter values & make a table ----------------------------------------------------------


#### Refit the model 6 years of data

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

# Fit the same model
out6 <- jags(data = win.data, inits = inits,  
             parameters.to.save = params, 
             model.file = "./Models/all_models.txt", 
             n.chains = 3, 
             n.thin = 1, 
             n.burnin = 1, 
             n.adapt = 1,
             n.iter = 2,
             parallel = TRUE)


beepr::beep(2)


# Save the output
save(out6, file = "./ModelOutput/first_5yrs_out.rda")





# End script
