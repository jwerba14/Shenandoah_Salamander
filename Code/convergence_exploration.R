library(tidyverse)
out <- readRDS( file = "weights_output.RDS")

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
              "beta.colC2")

# Biotic
paramsB <- c("alpha.pS", "alpha.rSC", "alpha.rSc", "alpha.rC", "alpha.pC",
             "alpha.psiSC",       
             "alpha.psiSc",       
             "alpha.psiC",        
             "alpha.survAloneS",            "alpha.survBothS",
             "alpha.colEmptyS",            "alpha.colOccS",
             "alpha.survAloneC",            "alpha.survBothC",
             "alpha.colEmptyC",            "alpha.colOccC")

# Null
paramsN <- c("alpha.pS", "alpha.pC",
             "alpha.psiS",       
             "alpha.psiC",         
             "alpha.survS",      
             "alpha.colS",     
             "alpha.survC",      
             "alpha.colC" 
             )


## explore convergence
cc1 <- out$Convergence_check
cc <- cc1 %>% filter(iteration != 4)

(cc[which(cc$rhat > 1.1), ])
(cc[which(cc$neff < 1000), ])
(cc[which(cc$neff < 800), ])
years <- c("2012", "2013","2014","2015","2016")


#### parameter correlations


corr.mattAB <- array(data = NA, dim=c(length(paramsAB),length(paramsAB),5) )
row.names(corr.mattAB) <- paramsAB
colnames(corr.mattAB) <- paramsAB

corr.mattA <-array(data = NA, dim=c(length(paramsA),length(paramsA),5))
row.names(corr.mattA) <- paramsA
colnames(corr.mattA) <- paramsA

corr.mattB <- array(data = NA, dim=c(length(paramsB),length(paramsB),5))
row.names(corr.mattB) <- paramsB
colnames(corr.mattB) <- paramsB

corr.mattN <- array(data = NA, dim=c(length(paramsN),length(paramsN),5))
row.names(corr.mattN) <- paramsN
colnames(corr.mattN) <- paramsN

## Abiotic-Biotic

AB.params.pred <- out$AB.params.pred
A.params.pred <- out$A.params.pred
B.params.pred <- out$B.params.pred
N.params.pred <- out$N.params.pred

for(k in 1:5){  
for(i in 1:nrow(corr.mattAB)) {
  for(j in 1:ncol(corr.mattAB)) {
    
    firstval <- c(
      AB.params.pred[ ,which(dimnames(AB.params.pred)[[1]] == 
                               dimnames(corr.mattAB)[[1]][[i]]), k]
    )
    
    secondval <- c(
      AB.params.pred[ ,which(dimnames(AB.params.pred)[[1]] == 
                               dimnames(corr.mattAB)[[1]][[j]]),k]
    )
    
    corr.mattAB[i, j,k] <- c(cor(firstval, secondval))
    
    
    print(i); print(j); print(k)
    
  }
}
}

corr.matt.gg <- reshape2::melt(corr.mattAB)
names(corr.matt.gg) <- c("X", "Y", "val")

corr.matt.gg <- corr.matt.gg[!duplicated(corr.matt.gg[, 3]), ]
corr.matt.gg <- corr.matt.gg %>% filter(X != Y)

max(corr.matt.gg$val)

tail(corr.matt.gg[order(corr.matt.gg$val, decreasing = T), ], 30)

ggplot(corr.matt.gg, aes(X, Y, z = val)) + 
  geom_tile() 
heatmap(corr.mattAB[,,1])  
heatmap(corr.mattAB[,,2])   
heatmap(corr.mattAB[,,3]) 
heatmap(corr.mattAB[,,4])
heatmap(corr.mattAB[,,5])


  

## Abiotic



for(k in 1:5){  
  for(i in 1:nrow(corr.mattA)) {
    for(j in 1:ncol(corr.mattA)) {
      
      firstval <- c(
        A.params.pred[ ,which(dimnames(A.params.pred)[[1]] == 
                                 dimnames(corr.mattA)[[1]][[i]]), k]
      )
      
      secondval <- c(
        A.params.pred[ ,which(dimnames(A.params.pred)[[1]] == 
                                 dimnames(corr.mattA)[[1]][[j]]),k]
      )
      
      corr.mattA[i, j,k] <- c(cor(firstval, secondval))
      
      
      print(i); print(j); print(k)
      
    }
  }
}

corr.matt.gg <- reshape2::melt(corr.mattAB)
names(corr.matt.gg) <- c("X", "Y", "val")

corr.matt.gg <- corr.matt.gg[!duplicated(corr.matt.gg[, 3]), ]
corr.matt.gg <- corr.matt.gg %>% filter(X != Y)

max(corr.matt.gg$val)

tail(corr.matt.gg[order(corr.matt.gg$val, decreasing = T), ], 30)

ggplot(corr.matt.gg, aes(X, Y, z = val)) + 
  geom_tile() 
heatmap(corr.mattA[,,1])  
heatmap(corr.mattA[,,2])   
heatmap(corr.mattA[,,3]) 
heatmap(corr.mattA[,,4])
heatmap(corr.mattA[,,5])


## Biotic
for(k in 1:5){  
  for(i in 1:nrow(corr.mattB)) {
    for(j in 1:ncol(corr.mattB)) {
      
      firstval <- c(
        B.params.pred[ ,which(dimnames(B.params.pred)[[1]] == 
                                dimnames(corr.mattB)[[1]][[i]]), k]
      )
      
      secondval <- c(
        B.params.pred[ ,which(dimnames(B.params.pred)[[1]] == 
                                dimnames(corr.mattB)[[1]][[j]]),k]
      )
      
      corr.mattA[i, j,k] <- c(cor(firstval, secondval))
      
      
      print(i); print(j); print(k)
      
    }
  }
}

corr.matt.gg <- reshape2::melt(corr.mattAB)
names(corr.matt.gg) <- c("X", "Y", "val")

corr.matt.gg <- corr.matt.gg[!duplicated(corr.matt.gg[, 3]), ]
corr.matt.gg <- corr.matt.gg %>% filter(X != Y)

max(corr.matt.gg$val)

tail(corr.matt.gg[order(corr.matt.gg$val, decreasing = T), ], 30)

ggplot(corr.matt.gg, aes(X, Y, z = val)) + 
  geom_tile() 
heatmap(corr.mattB[,,1])  
heatmap(corr.mattB[,,2])   
heatmap(corr.mattB[,,3]) 
heatmap(corr.mattB[,,4])
heatmap(corr.mattB[,,5])

##autocorrelation
corrAB <- data.frame(
  params = 
)
for (i in 1:length((AB.params.pred[,1,1]))){

  corrAB[i] <- acf(AB.params.pred[i,1,1])

}

prior <- plogis(rnorm(4500,0, 0.6))
hist(prior, col = "red") + hist(AB.params.pred[1,,1], col = "gray")


## prior distribution vs posterior distribution
ab1 <- t(as.data.frame(AB.params.pred[ ,,1]))
colnames(ab1) <- dimnames(AB.params.pred)[[1]]
ab2 <- data.frame(ab1)
ab2 <- ab2 %>% pivot_longer(everything(), names_to = "param", values_to = "value")

priorab <- ab2
priorab[,2] <- (rnorm(864000,0,sqrt(1/0.368)))

ab_plot <- ggplot() + 
  geom_histogram(data = priorab, aes( x=value, y=(..count../sum(..count..))), color = "red", alpha = 0.2, bins = 100) +
  geom_histogram(data = ab2, aes( x=value, y=..count../sum(..count..)), bins = 100) +
  facet_wrap(~param)

# ggplot(data = priorab, aes(value)) + geom_density() + facet_wrap(~param)

print(ab_plot)

ggplot(data = priorab) + geom_histogram(aes( x=value, y=..count../sum(..count..)), color = "red", alpha = 0.2, bins = 100)

priorab <- ab2
priorab[,2] <- (rnorm(864000,0,sqrt(1/0.368)))

ab_plot <- ggplot() + 
  geom_histogram(data = priorab, aes( x=value), color = "red", alpha = 0.2, bins = 100) +
  geom_histogram(data = ab2, aes( x=value), bins = 100) +
  facet_wrap(~param)

print(ab_plot)

### abiotic

a1 <- t(as.data.frame(A.params.pred[ ,,1]))
colnames(a1) <- dimnames(A.params.pred)[[1]]
a2 <- data.frame(a1)
a2 <- a2 %>% pivot_longer(everything(), names_to = "param", values_to = "value")

priora <- a2
priora[,2] <- (rnorm(432000,0,sqrt(1/0.368)))

a_plot <- ggplot() + 
  geom_histogram(data = priora, aes( x=value), color = "red", alpha = 0.2, bins = 100) +
  geom_histogram(data = a2, aes( x=value), bins = 100) +
  facet_wrap(~param)


print(a_plot)

## biotic

b1 <- t(as.data.frame(B.params.pred[ ,,1]))
colnames(b1) <- dimnames(B.params.pred)[[1]]
b2 <- data.frame(b1)
b2 <- b2 %>% pivot_longer(everything(), names_to = "param", values_to = "value")

priorb <- b2
priorb[,2] <- (rnorm(432000,0,sqrt(1/0.368)))

b_plot <- ggplot() + 
  geom_histogram(data = priorb, aes( x=value), color = "red", alpha = 0.2, bins = 100) +
  geom_histogram(data = b2, aes( x=value), bins = 100) +
  facet_wrap(~param)


print(b_plot)

## neutral
n1 <- t(as.data.frame(N.params.pred[ ,,1]))
colnames(n1) <- dimnames(N.params.pred)[[1]]
n2 <- data.frame(n1)
n2 <- n2 %>% pivot_longer(everything(), names_to = "param", values_to = "value")

priorn <- n2
priorn[,2] <- (rnorm(216000,0,sqrt(1/0.368)))

n_plot <- ggplot() + 
  geom_histogram(data = priorn, aes( x=value), color = "red", alpha = 0.2, bins = 100) +
  geom_histogram(data = n2, aes( x=value), bins = 100) +
  facet_wrap(~param)


print(n_plot)

##coef plots



##c-statistic

