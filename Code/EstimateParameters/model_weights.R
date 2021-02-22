library(jagsUI)
library(beepr)
library(ggplot2)
library(tidyverse)

# Load data
load("../../Data/formatted_shen_cin_dat.rda")
sites <- read.csv("../../Data/site_by_year_temp.csv")

shen_full <- dat$shen 
cin_full <- dat$cin 

# Load temp/RMI data
#temp <- read.csv("../../Data/site_by_year_temp.csv")
load("../../Data/temp_rmi.rda")

# Create temperature file 

std_temp <- sites %>% 
  mutate(mean_temp = mean(T_final), sd_temp = sd(T_final)) %>%
  group_by(Longitude, Latitude, year) %>% 
  mutate(std_temp = (T_final-mean_temp)/ sd_temp)


std_temp1 <- std_temp %>%
  dplyr::select(Longitude, Latitude, year, T_final) %>%
  pivot_wider(everything(), names_from = year, values_from = T_final) %>%
  dplyr::select(-c(Longitude, Latitude))

std_temp2 <- as.matrix(std_temp1[, -c(1:2)])
## RMI
RMI <- dat$site.RMI$RMI_p
mean_rmi <- mean(RMI,na.rm = TRUE)
sd_rmi <- sd(RMI,na.rm = TRUE)
std_rmi <- (RMI - mean_rmi ) / sd_rmi



##mcmc settings 
n.chains = 3 
n.iter = 100000
n.thin = 100
n.adapt = 10000
n.burnin = 10000


# Need another object to hold the weights 
AB.WP <- A.WP <- B.WP <- N.WP <- data.frame(weights = 0) 

# Parameters to monitor
paramsAB <- c(# Abiotic biotic
  "alpha.pS", "alpha.rSC","alpha.rSc", "alpha.rC", "alpha.pC",
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
  "beta.colEmptyC2",            "beta.colOccC2"
  ,"v", "z" 
  )
  
  # Abiotic
paramsA <-  c("alpha.pS", "alpha.pC",
  
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
  "beta.colC2", "v", "z")
  
  # Biotic
paramsB <- c("alpha.pS", "alpha.rSC", "alpha.rSc", "alpha.rC", "alpha.pC",
  "alpha.psiSC",       
  "alpha.psiSc",       
  "alpha.psiC",        
  "alpha.survAloneS",            "alpha.survBothS",
  "alpha.colEmptyS",            "alpha.colOccS",
  "alpha.survAloneC",            "alpha.survBothC",
  "alpha.colEmptyC",            "alpha.colOccC", "v", "z")
  
  # Null
paramsN <- c("alpha.pS", "alpha.pC",
  "alpha.psiS",       
  "alpha.psiC",         
  "alpha.survS",      
  "alpha.colS",     
  "alpha.survC",      
  "alpha.colC", 
  "v", "z")




p <- 6

AB.params.pred <- array(data = 0, dim = c(length(paramsAB) - 2, ((n.iter-n.burnin)/10)*3, 5))
dimnames(AB.params.pred)[[1]] <- paramsAB[1:(length(paramsAB) - 2)]
A.params.pred <- array(data = 0, dim = c(length(paramsA) - 2, ((n.iter-n.burnin)/10)*3, 5))
dimnames(A.params.pred)[[1]] <- paramsA[1:(length(paramsA) - 2)]
B.params.pred <- array(data = 0, dim = c(length(paramsB) - 2, ((n.iter-n.burnin)/10)*3, 5))
dimnames(B.params.pred)[[1]] <- paramsB[1:(length(paramsB) - 2)]
N.params.pred <- array(data = 0, dim = c(length(paramsN) - 2, ((n.iter-n.burnin)/10)*3, 5))
dimnames(N.params.pred)[[1]] <- paramsN[1:(length(paramsN) - 2)]

AB.predvals <- array(data = 0, dim = c(((n.iter-n.burnin)/10)*3, 209, 5, 2))
dimnames(AB.predvals)[[4]] <- c("z", "v")
A.predvals <- array(data = 0, dim = c(((n.iter-n.burnin)/10)*3, 209, 5, 2))
dimnames(A.predvals)[[4]] <- c("z", "v")
B.predvals <- array(data = 0, dim = c(((n.iter-n.burnin)/10)*3, 209, 5, 2))
dimnames(B.predvals)[[4]] <- c("z", "v")
N.predvals <- array(data = 0, dim = c(((n.iter-n.burnin)/10)*3, 209, 5, 2))  ## 4500 is n.iter(20000)- n.burnin(5000)/10 * n.chains(3)
dimnames(N.predvals)[[4]] <- c("z", "v")

for (i in 1:5) {

## fulldata
shen_full <- shen_full
cin_full <- cin_full
    
    
#subsample data
shen <- shen_full[,,1:p]

cin <- cin_full[,,1:p]

## put NAs in final year to make predictions
shen[, ,p] <- NA
cin[,,p] <- NA

# Bundle data
win.data <- list(
  ## Parameter estimation
  N =  dim(shen)[1],
  J =  dim(shen)[2],
  Yr = dim(shen)[3],
  yS = shen,
  yC = cin,
  TEMP = std_temp2,
  RMI = std_rmi
)


# Initial values
Sprez <- apply(win.data$yS, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
Sprez[Sprez == "-Inf"] <- NA

# Total number of sites sampled
tot.sites <- Sprez
#tot.sites[tot.sites == 0] <- 1
#colSums(Sprez, na.rm = TRUE)/colSums(tot.sites, na.rm = T) 

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
#colSums(Cprez, na.rm = TRUE)/colSums(tot.sites, na.rm = T) 

# Take max value across surveys for each site and year combo
vinit <- apply(win.data$yC, c(1, 3), max, na.rm = TRUE) 
# Replace in "-Inf" with 1
vinit[vinit == "-Inf"] <- 1  


# Bundle initial values
inits <- function() {list(
  z = zinit,
 v = vinit
)}


## fit model and simulate next year
AB.pred <- jagsUI::jags(model.file = "../../models/model_abiotic_biotic.txt", 
                    data = win.data, 
                    parameters.to.save = paramsAB,
                   n.chains = 3,
                   inits = inits, 
                   n.iter = n.iter, 
                   n.thin = n.thin,
                   n.adapt = n.adapt,
                  n.burnin = n.burnin,
                 parallel = TRUE)

#samplesAB <- coda.samples(AB.pred, variable.names = paramsAB, n.iter = 5000, thin = 1)
A.pred <- jagsUI::jags(model.file = "../../models/model_abiotic.txt", 
               data = win.data, 
               parameters.to.save = paramsA,
               n.chains = n.chains, 
               inits = inits,
               n.iter = n.iter,
               n.thin = n.thin,
               n.adapt = n.adapt,
               n.burnin = n.burnin,
               parallel = TRUE)

B.pred <- jagsUI::jags(model.file = "../../models/model_biotic.txt",  
               data = win.data, 
               parameters.to.save = paramsB,
               n.chains = 3,
               inits = inits, 
               n.iter = n.iter, 
               n.thin = n.thin,
               n.adapt = n.adapt,
               n.burnin = n.burnin,
               parallel = TRUE)

N.pred <- jagsUI::jags(model.file = "../../models/model_null.txt",  
               data = win.data, 
               parameters.to.save = paramsN,
               n.chains = 3,
               inits = inits, 
               n.iter = n.iter, 
               n.thin = n.thin,
               n.adapt = n.adapt,
               n.burnin = n.burnin,
               parallel = TRUE)

## store rhats and neff for parameters 
if (i ==1){
 conv_check <- data.frame(
   parameter = c(names(unlist(AB.pred$Rhat[1:32])), 
                 names(unlist(A.pred$Rhat[1:16])),
                 names(unlist(B.pred$Rhat[1:16])),
                 names(unlist(N.pred$Rhat[1:8]))),
   rhat = c(unlist(AB.pred$Rhat[1:32]),unlist(A.pred$Rhat[1:16]),unlist(B.pred$Rhat[1:16]),unlist(N.pred$Rhat[1:8])  ),
   neff =c(unlist(AB.pred$n.eff[1:32]), unlist(A.pred$n.eff[1:16]), unlist(B.pred$n.eff[1:16]), unlist(N.pred$n.eff[1:8])),
   model = c(rep("AB",length((unlist(AB.pred$Rhat[1:32])))), 
             rep("A",length(unlist(A.pred$Rhat[1:16]))),
                 rep("B",length(unlist(B.pred$Rhat[1:16]))), 
                     rep("N", length(unlist(N.pred$n.eff[1:8])))),
   iteration =1
 ) 
  
} else {
  conv_check2 <- data.frame(
    parameter = c(names(unlist(AB.pred$Rhat[1:32])), 
                  names(unlist(A.pred$Rhat[1:16])),
                  names(unlist(B.pred$Rhat[1:16])),
                  names(unlist(N.pred$Rhat[1:8]))),
    rhat = c(unlist(AB.pred$Rhat[1:32]),unlist(A.pred$Rhat[1:16]),unlist(B.pred$Rhat[1:16]),unlist(N.pred$Rhat[1:8])  ),
    neff =c(unlist(AB.pred$n.eff[1:32]), unlist(A.pred$n.eff[1:16]), unlist(B.pred$n.eff[1:16]), unlist(N.pred$n.eff[1:8])),
    model = c(rep("AB",length((unlist(AB.pred$Rhat[1:32])))), 
              rep("A",length(unlist(A.pred$Rhat[1:16]))),
              rep("B",length(unlist(B.pred$Rhat[1:16]))), 
              rep("N", length(unlist(N.pred$n.eff[1:8])))),
    iteration = i 
  ) 
  
  conv_check <- rbind(conv_check, conv_check2)  
}

#########

## Fill coefficient posterior for each parameter
for (j in 1:(length(AB.pred$sims.list) - 3)) {
 AB.params.pred[j, , i] <-  AB.pred$sims.list[[match(dimnames(AB.params.pred[, , i])[[1]][j], names(AB.pred$sims.list))]]
}
## Fill predicted values for year i for salamander species "v"
AB.predvals[, , i, "v"] <- AB.pred$sims.list[which(names(AB.pred$sims.list) == "v")][[1]][, , p]
AB.predvals[, , i, "z"] <- AB.pred$sims.list[which(names(AB.pred$sims.list) == "z")][[1]][, , p]


#########
for (k in 1:(length(A.pred$sims.list) - 3)) {
  A.params.pred[k, , i] <-  A.pred$sims.list[[match(dimnames(A.params.pred[, , i])[[1]][k], names(A.pred$sims.list))]]
}
## Fill predicted values for year i for salamander species "v"
A.predvals[, , i, "v"] <- A.pred$sims.list[which(names(A.pred$sims.list) == "v")][[1]][, , p]
A.predvals[, , i, "z"] <- A.pred$sims.list[which(names(A.pred$sims.list) == "z")][[1]][, , p]

######
for (m in 1:(length(B.pred$sims.list) - 3)) {
  B.params.pred[m, , i] <-  B.pred$sims.list[[match(dimnames(B.params.pred[, , i])[[1]][m], names(B.pred$sims.list))]]
}
## Fill predicted values for year i for salamander species "v"
B.predvals[, , i, "v"] <- B.pred$sims.list[which(names(B.pred$sims.list) == "v")][[1]][, , p]
B.predvals[, , i, "z"] <- B.pred$sims.list[which(names(B.pred$sims.list) == "z")][[1]][, , p]

######
for (q in 1:(length(N.pred$sims.list) - 3)) {
  N.params.pred[q, , i] <-  N.pred$sims.list[[match(dimnames(N.params.pred[, , i])[[1]][q], names(N.pred$sims.list))]]
}
## Fill predicted values for year i for salamander species "v"
N.predvals[, , i, "v"] <- N.pred$sims.list[which(names(N.pred$sims.list) == "v")][[1]][, , p]
N.predvals[, , i, "z"] <- N.pred$sims.list[which(names(N.pred$sims.list) == "z")][[1]][, , p]


## calculate likelihood of simulated data to actual data

# Calculate the loglikelihood of observing your data with the probabilities predicted by each model
AB.mod.prob <- sum(log(pbinom(shen_full[,,p], 1, prob = AB.pred$mean$z[,p])),log(
                       pbinom(cin_full[,,p], 1, prob = AB.pred$mean$v[,p])), na.rm = T)  ## true date first,1, predicted data


A.mod.prob  <- sum(log(pbinom(shen_full[,,p], 1, prob = A.pred$mean$z[,p])),log(
  pbinom(cin_full[,,p], 1, prob = A.pred$mean$v[,p])), na.rm = T)

B.mod.prob  <- sum(log(pbinom(shen_full[,,p], 1, prob = B.pred$mean$z[,p])),log(
  pbinom(cin_full[,,p], 1, prob = B.pred$mean$v[,p])), na.rm = T)

N.mod.prob  <- sum(log(pbinom(shen_full[,,p], 1, prob = N.pred$mean$z[,p])),log(
  pbinom(cin_full[,,p], 1, prob = N.pred$mean$v[,p])), na.rm = T)




# 10. Determine the model weights ----------------------------------------------------------




# Empty array to hold model weights
# This will hold the weights for each year of data not fit in the model yet = 5
# Start with equal weights = 0.25
# First model fit with 5 years of data: 2007 - 2011
# Then, predictions will be made for 2012, 2013, 2014, 2015, 2016 with the respective models
# This will hold the model weights




if (i == 1){
# Equal initial model weights
# Then it will be updated for each year of predictions 2012 - 2016
  AB.WP[1] <- A.WP[1] <- B.WP[1] <- N.WP[1] <- 1/4

} else {

# Update the model weights by multiplying the previous years weight by likelihood
## year 4 (2015) doesn't have data
  
if (i == 5){
  AB.WP[i,] <-  (AB.WP[i-2,] * AB.mod.prob) / ((AB.WP[i-2,] * AB.mod.prob) + (A.WP[i-2,] * A.mod.prob) + (B.WP[i-2,] * B.mod.prob)+ (N.WP[i-2,] * N.mod.prob) )
  A.WP[i,] <-   (A.WP[i-2,] * A.mod.prob)  / ((AB.WP[i-2,] * AB.mod.prob) + (A.WP[i-2,] * A.mod.prob) + (B.WP[i-2,] * B.mod.prob)+ (N.WP[i-2,] * N.mod.prob) )
  B.WP[i,] <-   (B.WP[i-2,] * B.mod.prob)  / ((AB.WP[i-2,] * AB.mod.prob) + (A.WP[i-2,] * A.mod.prob) + (B.WP[i-2,] * B.mod.prob)+ (N.WP[i-2,] * N.mod.prob) )
  N.WP[i,] <-   (N.WP[i-2,] * N.mod.prob)  / ((AB.WP[i-2,] * AB.mod.prob) + (A.WP[i-2,] * A.mod.prob) + (B.WP[i-2,] * B.mod.prob)+ (N.WP[i-2,] * N.mod.prob) )
  
  
  
} else {
  
  
  
AB.WP[i,] <-  (AB.WP[i-1,] * AB.mod.prob) / ((AB.WP[i-1,] * AB.mod.prob) + (A.WP[i-1,] * A.mod.prob) + (B.WP[i-1,] * B.mod.prob)+ (N.WP[i-1,] * N.mod.prob) )
A.WP[i,] <-   (A.WP[i-1,] * A.mod.prob)  / ((AB.WP[i-1,] * AB.mod.prob) + (A.WP[i-1,] * A.mod.prob) + (B.WP[i-1,] * B.mod.prob)+ (N.WP[i-1,] * N.mod.prob) )
B.WP[i,] <-   (B.WP[i-1,] * B.mod.prob)  / ((AB.WP[i-1,] * AB.mod.prob) + (A.WP[i-1,] * A.mod.prob) + (B.WP[i-1,] * B.mod.prob)+ (N.WP[i-1,] * N.mod.prob) )
N.WP[i,] <-   (N.WP[i-1,] * N.mod.prob)  / ((AB.WP[i-1,] * AB.mod.prob) + (A.WP[i-1,] * A.mod.prob) + (B.WP[i-1,] * B.mod.prob)+ (N.WP[i-1,] * N.mod.prob) )
  }
}
p <- p +1


}

wt <- as.vector(rbind(AB.WP,A.WP,B.WP,N.WP))

##

#dataframe of weights

weights <- data.frame( model = rep(c("AB", "A","B","N"), each = 5),
                       weights = wt,
                       year = rep(c("2012","2013","2014","2015","2016"), 4))
                


weights <- weights %>% filter(year != "2015")

# 11. Make a plot ---------------------------------------------------------



# Make a plot
ggplot(data = weights, aes(x = year, y = weights, col = model))+
  geom_jitter(size = 4)+
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

 




saveRDS(list("AB.params.pred" = AB.params.pred,
             "A.params.pred" = A.params.pred,
             "B.params.pred"= B.params.pred,
             "N.params.pred" = N.params.pred,
             "Convergence_check" = conv_check,
             "AB.predvals" = AB.predvals,
             "A.predvals" = A.predvals,
              "B.predvals" =B.predvals,
              "N.predvals" = N.predvals,
               "weights" = weights),
        file = "weights_output.RDS")
