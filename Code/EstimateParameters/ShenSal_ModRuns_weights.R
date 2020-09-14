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
            "N.alpha.colC",
            
            # Model weights
            "AB.w.mean",
            "A.w.mean",
            "B.w.mean",
            "N.w.mean",
            
            # Log-likelihoods
            "AB.LL",
            "A.LL",
            "B.LL",
            "N.LL")

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
  N = dim(shen)[1],
  J =  dim(shen)[2],
  Yr = dim(shen)[3],
  
  AB.yS = shen,
  A.yS = shen,
  B.yS = shen,
  N.yS = shen,
  
  AB.yC = cin,
  A.yC = cin,
  B.yC = cin,
  N.yC = cin,
  
  TEMP = std_temp,
  RMI = std_rmi
)

# Look at structure
str(win.data)

# Initial values
# Take max value across surveys for each site and year combo
zinit <- apply(win.data$AB.yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 0
zinit[zinit == "-Inf"] <- 0

# Take max value across surveys for each site and year combo
vinit <- apply(win.data$AB.yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  

# Initialize model weights
AB.w <- A.w <- B.w <- N.w <- array(0.25, dim = c(win.data$N, win.data$Yr))

# Everything except 1st column is NA
AB.w[,-1] <- A.w[,-1] <- B.w[,-1] <- N.w[,-1] <- NA

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
  
  AB.w = AB.w,
  A.w = A.w,
  B.w = B.w,
  N.w = N.w,
  
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

# MCMC settings
ni <- 5000
nb <- 2000
nt <- 10
nc <- 3
na <- 10000
mi <- 1e6

# Run model
out <- autojags(data = win.data, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/model_weights.txt", 
                n.chains = nc, 
                n.thin = nt, 
                n.burnin = nb, 
                n.adapt = na,
                max.iter = mi,
                iter.increment = ni,
                parallel = TRUE)

out <- jags(data = win.data, inits = inits,  
                parameters.to.save = params, 
                model.file = "./Models/model_weights.txt", 
                n.chains = 3, 
                n.thin = 1, 
                n.burnin = 1, 
                n.adapt = 1,
                n.iter = 2,
                parallel = TRUE)


beepr::beep(2)

# Save the output
save(out, file = "./ModelOutput/mod_weights_output.rda")






# 7. Extract parameter values & make a table ----------------------------------------------------------



# Master parameter table
prms <- matrix(NA, 
              nrow = length(params), # Total number of parameters
              ncol = 4)              # Parameter name, mean, lower, upper

# Add column names
colnames(prms) <- c("param.name","mean", "q2.5", "q97.5")

# Pull out parameter names
prms[,"param.name"] <- names(out$mean)

# Add in the mean, lower, and upper
prms[, "mean"]  <- unlist(out$mean)
prms[, "q2.5"]  <- unlist(out$q2.5)
prms[, "q97.5"] <- unlist(out$q97.5)

# Write the file
write.csv(prms, "/Volumes/GVD/Yeti/ShenSal/Tables/Model_weights_parameter_values.csv")

# End script
