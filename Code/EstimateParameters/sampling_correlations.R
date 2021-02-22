#model exploration

AB.pred <- jags.model(file = "../../models/model_abiotic_biotic.txt", 
                      data = win.data, 
                      #parameters.to.save = paramsAB,
                      n.chains = 3,
                      inits = inits, 
                      #n.iter = 10000, 
                      #n.thin = 10,
                      n.adapt = 10000)

samplesAB <- coda.samples(AB.pred, variable.names = paramsAB, n.iter = 5000, thin = 1)
CorrAB <- autocor(samplesAB)

CC1 <- as.data.frame(CorrAB[1])
CC1 <- CC1[complete.cases(CC1), ]
tt <- which(CC1 > 0.7) 

CC2 <- as.data.frame(CorrAB[2])
CC2 <- CC2[complete.cases(CC2), ]
tt2 <- which(CC2 > 0.7) 


CC3 <- as.data.frame(CorrAB[3])
CC3 <- CC2[complete.cases(CC3), ]
tt3 <- which(CC3 > 0.7) 

autocorr.plot(samplesAB)

plot(samplesAB)

corr.matt <- matrix(data = 0, nrow = length(paramsAB), ncol = length(paramsAB))
row.names(corr.matt) <- paramsAB
colnames(corr.matt) <- paramsAB


for(i in 1:nrow(corr.matt)) {
 for(j in 1:ncol(corr.matt)) {
    
  firstval <- c(
    samplesAB[[1]][, which(dimnames(samplesAB[[1]])[[2]] == 
                             dimnames(corr.matt)[[1]][i])]
  , samplesAB[[2]][, which(dimnames(samplesAB[[2]])[[2]] == 
                             dimnames(corr.matt)[[1]][i])]
  , samplesAB[[3]][, which(dimnames(samplesAB[[3]])[[2]] == 
                             dimnames(corr.matt)[[1]][i])]
    )
  
  secondval <- c(
    samplesAB[[1]][, which(dimnames(samplesAB[[1]])[[2]] == 
                             dimnames(corr.matt)[[2]][j])]
    , samplesAB[[2]][, which(dimnames(samplesAB[[2]])[[2]] == 
                               dimnames(corr.matt)[[2]][j])]
    , samplesAB[[3]][, which(dimnames(samplesAB[[3]])[[2]] == 
                               dimnames(corr.matt)[[2]][j])]
  )
  
  corr.matt[i, j] <- cor(firstval, secondval)
  
  
  print(i); print(j)
  
 }
}

corr.matt.gg <- reshape2::melt(corr.matt)
names(corr.matt.gg) <- c("X", "Y", "val")

corr.matt.gg <- corr.matt.gg[!duplicated(corr.matt.gg[, 3]), ]
corr.matt.gg <- corr.matt.gg %>% filter(X != Y)

max(corr.matt.gg$val)

head(corr.matt.gg[order(corr.matt.gg$val, decreasing = T), ], 30)

ggplot(corr.matt.gg, aes(X, Y, z = val)) + 
  geom_tile() +
  


###
A.pred <- jags.model(file = "../../models/model_abiotic.txt", 
                      data = win.data, 
                      #parameters.to.save = paramsAB,
                      n.chains = 3,
                      inits = inits, 
                      #n.iter = 10000, 
                      #n.thin = 10,
                      n.adapt = 10000)

samplesAB <- coda.samples(A.pred, variable.names = paramsA, n.iter = 5000, thin = 1)
CorrAB <- cor(samplesAB)

CC1 <- as.data.frame(CorrAB[1])
CC1 <- CC1[complete.cases(CC1), ]
tt <- which(CC1 > 0.7) 

CC2 <- as.data.frame(CorrAB[2])
CC2 <- CC2[complete.cases(CC2), ]
tt2 <- which(CC2 > 0.7) 


CC3 <- as.data.frame(CorrAB[3])
CC3 <- CC2[complete.cases(CC3), ]
tt3 <- which(CC3 > 0.7) 

autocorr.plot(samplesAB)

plot(samplesAB)

##


