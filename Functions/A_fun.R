

# This is the function to run the Abiotic model predictions

A.fun <- function(n.year, # Number of years = # of years the model + 1 year of prediction
                   out,    # JAGS model output
                   win.data # Data fed into the model
                   ){
  
# Number of sites
n.site <- nrow(shen)

# Number of years = # of years the model + 1 year of prediction
n.year <- n.year

# Create empty matricies to hold data
# 2D: site x year
A.occS <- A.occC <- A.muS <- A.muC <- array(NA, dim = c(n.site, n.year))

# 2D: site x year 
A.survS1 <- A.colS1 <- A.survC1 <- A.colC1 <- array(NA, dim = c(n.site, n.year))

# 3D: site x survey x year
A.eff.pS <- A.eff.pC <- A.yS <- A.yC <- array(NA, dim = c(n.site, win.data$J, n.year))

# Initial occupancy in 2007
A.psiC1 <-  plogis(out$mean$A.alpha.psiC)
A.psiS1 <- plogis(out$mean$A.alpha.psiS)


# Set the colonization/extinction probabilities
A.survS1 <- plogis(out$mean$A.alpha.survS + 
                   out$mean$A.beta.survS1 * win.data$TEMP + 
                   out$mean$A.beta.survS2 * win.data$RMI)
A.colS1 <- plogis(out$mean$A.alpha.colS + 
                  out$mean$A.beta.colS1 * win.data$TEMP + 
                  out$mean$A.beta.colS2 * win.data$RMI)
A.survC1 <- plogis(out$mean$A.alpha.survC + 
                    out$mean$A.beta.survC1 * win.data$TEMP + 
                    out$mean$A.beta.survC2 * win.data$RMI)
A.colC1  <- plogis(out$mean$A.alpha.colC + 
                    out$mean$A.beta.colC1 * win.data$TEMP + 
                    out$mean$A.beta.colC2 * win.data$RMI)

A.pC <- plogis(out$mean$A.alpha.pC)
A.pS  <- plogis(out$mean$A.alpha.pS)


# Determine Pcin occupancy in 2007
A.occC[,1] <- rbinom(n.site, 1, prob = A.psiC1)


# Determine Pshen occupancy in 2007
A.occS[ , 1] <- rbinom(n.site, 1, prob = A.psiS1)	

# For each year from 2 to 5
for(k in 2:n.year){ 
  
  # Determine probabilities 
  A.muS[,k-1] <- A.occS[,k-1] * A.survS1  + 
                  (1- A.occS[,k-1]) * A.colS1 

  A.muC[,k-1] <- A.survC1 * A.occC[,k-1] + 
                  A.colC1 * (1-A.occC[,k-1])

  # Determine occupancy
  A.occS[,k] <- rbinom(n.site, 1, prob = A.muS[,k-1])	# True occupancy Shenandoah sal
  A.occC[,k] <- rbinom(n.site, 1, prob = A.muC[,k-1])	# True occupancy Cinereus sal
  
} # k = year

for(k in 1:n.year) {  # Loop over years
  for(j in 1:win.data$J) { # Loop over replicates
    
    # Conditional detection probabilities
    A.eff.pS[,j,k] <- A.occS[,k] * A.pS
                     
    # Conditional detection probabilities
    A.eff.pC[,j,k] <- A.pC * A.occC[,k] 
    
    # Model predictions for each year
        # Observation model for the actual observations
        A.yS[,j,k] <- rbinom(n.site, 1, prob = A.eff.pS[,j,k])
        
        # Observation model for the actual observations
        A.yC[,j,k] <- rbinom(n.site, 1, prob = A.eff.pC[,j,k])

  }
}

return(list(A.eff.pS = A.eff.pS,
             A.eff.pC = A.eff.pC))
}