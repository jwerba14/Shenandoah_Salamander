

# This is the function to run the Abiotic Biotic model predictions

AB.fun <- function(n.year, # Number of years = # of years the model + 1 year of prediction
                   out,    # JAGS model output
                   win.data # Data fed into the model
                   ){
  
# Number of sites
n.site <- nrow(shen)

# Number of years = # of years the model + 1 year of prediction
n.year <- n.year

# Create empty matricies to hold data
# 2D: site x year
AB.occS <- AB.occC <- AB.muS <- AB.muC <- array(NA, dim = c(n.site, n.year))

# 2D: site x year 
AB.survAloneS1 <- AB.survBothS1 <- AB.colEmptyS1 <- AB.colOccS1 <- AB.survAloneC1 <- AB.survBothC1 <- AB.colEmptyC1 <- AB.colOccC1 <- array(NA, dim = c(n.site, n.year))

# 3D: site x survey x year
AB.eff.pS <- AB.eff.pC <- AB.yS <- AB.yC <- array(NA, dim = c(n.site, win.data$J, n.year))

# Initial occupancy in 2007
AB.psiC1 <-  plogis(out$mean$AB.alpha.psiC)
AB.psiSC1 <- plogis(out$mean$AB.alpha.psiSC)
AB.psiSc1 <- plogis(out$mean$AB.alpha.psiSc) 


# Set the colonization/extinction probabilities
AB.survAloneS1 <- plogis(out$mean$AB.alpha.survAloneS + 
                         out$mean$AB.beta.survAloneS1 * win.data$TEMP + 
                         out$mean$AB.beta.survAloneS2 * win.data$RMI)
AB.survBothS1 <- plogis(out$mean$AB.alpha.survBothS + 
                        out$mean$AB.beta.survBothS1 * win.data$TEMP + 
                        out$mean$AB.beta.survBothS2 * win.data$RMI)
AB.colEmptyS1 <- plogis(out$mean$AB.alpha.colEmptyS + 
                        out$mean$AB.beta.colEmptyS1 * win.data$TEMP + 
                        out$mean$AB.beta.colEmptyS2 * win.data$RMI)
AB.colOccS1   <- plogis(out$mean$AB.alpha.colOccS + 
                        out$mean$AB.beta.colOccS1 * win.data$TEMP +
                        out$mean$AB.beta.colOccS2 * win.data$RMI)
AB.survAloneC1 <- plogis(out$mean$AB.alpha.survAloneC + 
                         out$mean$AB.beta.survAloneC1 * win.data$TEMP + 
                         out$mean$AB.beta.survAloneC2 * win.data$RMI)
AB.survBothC1 <- plogis(out$mean$AB.alpha.survBothC + 
                        out$mean$AB.beta.survBothC1 * win.data$TEMP + 
                        out$mean$AB.beta.survBothC2 * win.data$RMI)
AB.colEmptyC1  <- plogis(out$mean$AB.alpha.colEmptyC + 
                         out$mean$AB.beta.colEmptyC1 * win.data$TEMP + 
                         out$mean$AB.beta.colEmptyC2 * win.data$RMI)
AB.colOccC1   <- plogis(out$mean$AB.alpha.colOccC + 
                        out$mean$AB.beta.colOccC1 * win.data$TEMP + 
                        out$mean$AB.beta.colOccC2 * win.data$RMI) 

AB.pC <- plogis(out$mean$AB.alpha.pC)
AB.rC <- plogis(out$mean$AB.alpha.rC)

AB.pS  <- plogis(out$mean$AB.alpha.pS)
AB.rSC <- plogis(out$mean$AB.alpha.rSC)
AB.rSc <- plogis(out$mean$AB.alpha.rSc)


# Determine Pcin occupancy in 2007
AB.occC[,1] <- rbinom(n.site, 1, prob = AB.psiC1)


# Based on Pcin occupancy, determine shenandoah probability of occupancy in 2007
AB.psiS1 <- AB.psiSC1 * AB.occC + 
            AB.psiSc1 * (1 - AB.occC)

AB.occS[ , 1] <- rbinom(n.site, 1, prob = AB.psiS1)	

# For each year from 2 to 5
for(k in 2:n.year){ 
  
  # Determine probabilities 
  AB.muS[,k-1] <- AB.occS[,k-1] * AB.survAloneS1 * (1-AB.occC[,k-1]) + 
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

for(k in 1:n.year) {  # Loop over years
  for(j in 1:win.data$J) { # Loop over replicates
    
    # Conditional detection probabilities
    AB.eff.pS[,j,k] <- AB.occS[,k] * AB.pS * (1 - AB.occC[,k]) +
                       AB.occS[,k] * AB.rSC * AB.occC[,k] * cin[,j,k] +
                       AB.occS[,k] * AB.rSc * AB.occC[,k] * (1-cin[,j,k])
                     
    # Conditional detection probabilities
    AB.eff.pC[,j,k] <- (1 - AB.occS[,k]) * AB.pC * AB.occC[,k] +
                          AB.occS[,k] * AB.rC * AB.occC[,k]
    
    # Model predictions for each year
    for(i in 1:win.data$N){
      if(is.na(AB.eff.pS[i,j,k]) == FALSE){
        # Observation model for the actual observations
        AB.yS[i,j,k] <- rbinom(1, 1, prob = AB.eff.pS[i,j,k])
        
        # Observation model for the actual observations
        AB.yC[i,j,k] <- rbinom(1, 1, prob = AB.eff.pC[i,j,k])
      }
    }
  }
}

return(list(AB.eff.pS = AB.eff.pS,
             AB.eff.pC = AB.eff.pC))
}