library(boot)
library(purrr)
##write a bernoulli function and see how the outcome is affected by different linear inputs

simp_test <- function(slope1, slope2,x,y){
  prob <- slope1*x + slope2*y
  rbernoulli(100, boot::inv.logit(prob))
}


simp_test2 <- function(slope1, slope2,x,y){
  prob <- slope1*x * slope2*y
  rbernoulli(100, boot::inv.logit(prob))
}


x <- 2
y <- 3

slope1 = rnorm(200, mean = 0, sd = 2)
slope2 = 20
#plot(inv.logit(seq(-10,10, by = 0.1)))
out <-data.frame(
  prb = numeric(length = length(slope1)),
  slope1 = slope1)

for(i in 1:length(slope1)){
  
  out$prb[i] <- sum(simp_test(out$slope1[i], slope2, x,y))/100
  
}

out <-data.frame(
  prb = numeric(length = length(slope1)),
  slope1 = slope1)

for(i in 1:length(slope1)){
  
  out$prb[i] <- sum(simp_test2(out$slope1[i], slope2, x,y))/100
  
}


ggplot(out, aes(slope1, prb)) + geom_point()

## try with 2 slopes together
slope2 <- rnorm(200, mean = 0, sd =2)

out <-data.frame(
  prb = numeric(length = length(slope1)),
  slope1 = slope1,
  slope2 = slope2)


for(i in 1:length(slope1)){
  
  out$prb[i] <- sum(simp_test(out$slope1[i], out$slope2[i], x,y))/100
  
}

out2 <- out %>% pivot_longer(-prb, names_to = "param", values_to= "value")

ggplot(out2, aes(value, prb)) + geom_point() + facet_wrap(~param) 

## what if x and y are different
x<- -2
slope1 = rnorm(200, mean = 0, sd = 2)
slope2 = 0
#plot(inv.logit(seq(-10,10, by = 0.1)))
out <-data.frame(
  prb = numeric(length = length(slope1)),
  slope1 = slope1)

for(i in 1:length(slope1)){
  
  out$prb[i] <- sum(simp_test(out$slope1[i], slope2, x,y))/100
  
}

ggplot(out, aes(slope1, prb)) + geom_point()

###
y <- 2
slope2 <- rnorm(200, mean = 0, sd =2)

out <-data.frame(
  prb = numeric(length = length(slope1)),
  slope1 = slope1,
  slope2 = slope2)


for(i in 1:length(slope1)){
  
  out$prb[i] <- sum(simp_test(out$slope1[i], out$slope2[i], x,y))/100
  
}

out2 <- out %>% pivot_longer(-prb, names_to = "param", values_to= "value")

ggplot(out2, aes(value, prb)) + geom_point() + facet_wrap(~param) 

###
eff_size <- sobolDesign(lower = c(
  int_col = -5,
  slope1 = -5,
  int_surv = -5,
  slope2 = -5),
  upper = c(int_col = 5,
            slope1 = 5,
            int_surv = 5,
            slope2 = 5),
 nseq = 200 
  
)

occ <- data.frame(
  int_col = numeric(length = 200),
  slope1 = numeric(length = 200),
  int_surv = numeric(length = 200),
  slope2 = numeric(length = 200),
  prob_col = numeric(length = 200),
  prob_surv = numeric(length = 200),
  occ = numeric(length = 200)
)

## hold all but 1 constant first
for (i in 1:nrow(eff_size)){

occ$prob_col[i] <- eff_size$int_col[i] + eff_size$slope1[1]*x
occ$prob_surv[i] <- eff_size$int_surv[1] + eff_size$slope2[1]*x

## stable
occ$slope1 <-eff_size$slope1[1]
occ$slope2 <-eff_size$slope2[1]
occ$int_surv <-eff_size$int_surv[1]

## changing    
occ$int_col[i] <-  eff_size$int_col[i]
occ$occ[i] <- inv.logit(prob_col + prob_surv)

}

ggplot(occ, aes(int_col, occ)) + geom_point()
ggplot(occ, aes(int_col, prob_col)) + geom_point() 
ggplot(occ, aes(int_col, prob_surv)) + geom_point()
ggplot(occ, aes(prob_surv, occ)) + geom_point()
ggplot(occ, aes(prob_col, occ)) + geom_point()



for (i in 1:nrow(eff_size)){
  
  occ$prob_col[i] <-  eff_size$int_col[1] + eff_size$slope1[i]*x
  occ$prob_surv[i] <- eff_size$int_surv[1] + eff_size$slope2[200]*x
  
  
  occ$slope1[i] <-eff_size$slope1[i]
  occ$slope2 <-eff_size$slope2[1]
  occ$int_surv <-eff_size$int_surv[1]
  
      
  occ$int_col <-  eff_size$int_col[1]
  occ$occ[i] <- inv.logit(prob_col + prob_surv)
  
  
}

ggplot(occ, aes(slope1, occ)) + geom_point()
ggplot(occ, aes(slope1, prob_col)) + geom_point() 
ggplot(occ, aes(slope1, prob_surv)) + geom_point()
ggplot(occ, aes(prob_surv, occ)) + geom_point()
ggplot(occ, aes(prob_col, occ)) + geom_point()

