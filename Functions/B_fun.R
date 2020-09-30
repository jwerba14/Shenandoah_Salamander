

# This is the function to run the Biotic model predictions

B.fun <- function(n.year, # Number of years = # of years the model + 1 year of prediction
                   out,    # JAGS model output
                   win.data # Data fed into the model
                   ){
  
# Number of sites
n.site <- nrow(shen)

# Number of years = # of years the model + 1 year of prediction
n.year <- n.year

# Create empty matricies to hold data
# 2D: site x year
B.occS <- B.occC <- B.muS <- B.muC <- array(NA, dim = c(n.site, n.year))

# 2D: site x year 
B.survAloneS1 <- B.survBothS1 <- B.colEmptyS1 <- B.colOccS1 <- B.survAloneC1 <- B.survBothC1 <- B.colEmptyC1 <- B.colOccC1 <- array(NA, dim = c(n.site, n.year))

# 3D: site x survey x year
B.eff.pS <- B.eff.pC <- B.yS <- B.yC <- array(NA, dim = c(n.site, win.data$J, n.year))

# Initial occupancy in 2007
B.psiC1 <-  plogis(out$mean$B.alpha.psiC)
B.psiSC1 <- plogis(out$mean$B.alpha.psiSC)
B.psiSc1 <- plogis(out$mean$B.alpha.psiSc) 


# Set the colonization/extinction probabilities
B.survAloneS1 <- plogis(out$mean$B.alpha.survAloneS)
B.survBothS1 <- plogis(out$mean$B.alpha.survBothS)
B.colEmptyS1 <- plogis(out$mean$B.alpha.colEmptyS)
B.colOccS1   <- plogis(out$mean$B.alpha.colOccS)
B.survAloneC1 <- plogis(out$mean$B.alpha.survAloneC)
B.survBothC1 <- plogis(out$mean$B.alpha.survBothC)
B.colEmptyC1  <- plogis(out$mean$B.alpha.colEmptyC)
B.colOccC1   <- plogis(out$mean$B.alpha.colOccC) 

B.pC <- plogis(out$mean$B.alpha.pC)
B.rC <- plogis(out$mean$B.alpha.rC)

B.pS  <- plogis(out$mean$B.alpha.pS)
B.rSC <- plogis(out$mean$B.alpha.rSC)
B.rSc <- plogis(out$mean$B.alpha.rSc)


# Determine Pcin occupancy in 2007
B.occC[,1] <- rbinom(n.site, 1, prob = B.psiC1)


# Based on Pcin occupancy, determine shenandoah probability of occupancy in 2007
B.psiS1 <- B.psiSC1 * B.occC + 
           B.psiSc1 * (1 - B.occC)

B.occS[ , 1] <- rbinom(n.site, 1, prob = B.psiS1)	

# For each year from 2 to 5
for(k in 2:n.year){ 
  
  # Determine probBilities 
  B.muS[,k-1] <- B.occS[,k-1] * B.survAloneS1 * (1-B.occC[,k-1]) + 
                  B.occS[,k-1] * B.survBothS1      * B.occC[,k-1] + 
                  (1- B.occS[,k-1]) * B.colEmptyS1 * (1-B.occC[,k-1]) +
                  (1- B.occS[,k-1]) * B.colOccS1   * B.occC[,k-1]
  
  B.muC[,k-1] <- (1-B.occS[,k-1]) * B.survAloneC1 * B.occC[,k-1] + 
                   B.occS[,k-1]      * B.survBothC1 * B.occC[,k-1] + 
                   (1- B.occS[,k-1]) * B.colEmptyC1 * (1-B.occC[,k-1]) +
                   B.occS[,k-1]      * B.colOccC1   * (1-B.occC[,k-1])
  
  # Determine occupancy
  B.occS[,k] <- rbinom(n.site, 1, prob = B.muS[,k-1])	# True occupancy Shenandoah sal
  B.occC[,k] <- rbinom(n.site, 1, prob = B.muC[,k-1])	# True occupancy Cinereus sal
  
} # k = year

for(k in 1:n.year) {  # Loop over years
  for(j in 1:win.data$J) { # Loop over replicates
    
    # Conditional detection probBilities
    B.eff.pS[,j,k] <- B.occS[,k] * B.pS * (1 - B.occC[,k]) +
                       B.occS[,k] * B.rSC * B.occC[,k] * cin[,j,k] +
                       B.occS[,k] * B.rSc * B.occC[,k] * (1-cin[,j,k])
                     
    # Conditional detection probBilities
    B.eff.pC[,j,k] <- (1 - B.occS[,k]) * B.pC * B.occC[,k] +
                          B.occS[,k] * B.rC * B.occC[,k]
    
    # Model predictions for each year
    for(i in 1:win.data$N){
      if(is.na(B.eff.pS[i,j,k]) == FALSE){
        # Observation model for the actual observations
        B.yS[i,j,k] <- rbinom(1, 1, prob = B.eff.pS[i,j,k])
        
        # Observation model for the actual observations
        B.yC[i,j,k] <- rbinom(1, 1, prob = B.eff.pC[i,j,k])
      }
    }
  }
}

return(list(B.eff.pS = B.eff.pS,
             B.eff.pC = B.eff.pC))
}