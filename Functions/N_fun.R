

# This is the function to run the Null model predictions

N.fun <- function(n.year, # Number of years = # of years the model + 1 year of prediction
                   out,    # JAGS model output
                   win.data # Data fed into the model
                   ){
  
# Number of sites
n.site <- nrow(shen)

# Number of years = # of years the model + 1 year of prediction
n.year <- n.year

# Create empty matricies to hold data
# 2D: site x year
N.occS <- N.occC <- N.muS <- N.muC <- array(NA, dim = c(n.site, n.year))

# 2D: site x year 
N.survS1 <- N.colS1 <- N.survC1 <- N.colC1 <- array(NA, dim = c(n.site, n.year))

# 3D: site x survey x year
N.eff.pS <- N.eff.pC <- N.yS <- N.yC <- array(NA, dim = c(n.site, win.data$J, n.year))

# Initial occupancy in 2007
N.psiC1 <-  plogis(out$mean$N.alpha.psiC)
N.psiS1 <- plogis(out$mean$N.alpha.psiS)


# Set the colonization/extinction probabilities
N.survS1 <- plogis(out$mean$N.alpha.survS)
N.colS1 <- plogis(out$mean$N.alpha.colS)
N.survC1 <- plogis(out$mean$N.alpha.survC)
N.colC1  <- plogis(out$mean$N.alpha.colC)

N.pC <- plogis(out$mean$N.alpha.pC)
N.pS  <- plogis(out$mean$N.alpha.pS)


# Determine Pcin occupancy in 2007
N.occC[,1] <- rbinom(n.site, 1, prob = N.psiC1)


# Determine Pshen occupancy in 2007
N.occS[ , 1] <- rbinom(n.site, 1, prob = N.psiS1)	

# For each year from 2 to 5
for(k in 2:n.year){ 
  
  # Determine probabilities 
  N.muS[,k-1] <- N.occS[,k-1] * N.survS1  + 
                  (1- N.occS[,k-1]) * N.colS1 

  N.muC[,k-1] <- N.survC1 * N.occC[,k-1] + 
                  N.colC1 * (1-N.occC[,k-1])

  # Determine occupancy
  N.occS[,k] <- rbinom(n.site, 1, prob = N.muS[,k-1])	# True occupancy Shenandoah sal
  N.occC[,k] <- rbinom(n.site, 1, prob = N.muC[,k-1])	# True occupancy Cinereus sal
  
} # k = year

for(k in 1:n.year) {  # Loop over years
  for(j in 1:win.data$J) { # Loop over replicates
    
    # Conditional detection probabilities
    N.eff.pS[,j,k] <- N.occS[,k] * N.pS
                     
    # Conditional detection probabilities
    N.eff.pC[,j,k] <- N.pC * N.occC[,k] 
    
    # Model predictions for each year
        # Observation model for the actual observations
        N.yS[,j,k] <- rbinom(n.site, 1, prob = N.eff.pS[,j,k])
        
        # Observation model for the actual observations
        N.yC[,j,k] <- rbinom(n.site, 1, prob = N.eff.pC[,j,k])
  }
}

return(list(N.eff.pS = N.eff.pS,
             N.eff.pC = N.eff.pC))
}