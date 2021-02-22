library(tidyverse)
library(jagsUI)
# Load data
load("../../Data/formatted_shen_cin_dat.rda")
sites <- read.csv("../../Data/site_by_year_temp.csv")

## temperature 
std_temp <- sites %>% 
  mutate(mean_temp = mean(T_final), sd_temp = sd(T_final)) %>%
  group_by(Longitude, Latitude, year) %>% 
  mutate(std_temp = (T_final-mean_temp)/ sd_temp)


std_temp1 <- std_temp %>%
  dplyr::select(Longitude, Latitude, year, T_final) %>%
  pivot_wider(everything(), names_from = year, values_from = T_final) %>%
  dplyr::select(-c(Longitude, Latitude))

std_temp2 <- as.matrix(std_temp1[, -c(1:2)])



shen_full <- dat$shen 
cin_full <- dat$cin 

# Load temp/RMI data
#temp <- read.csv("../../Data/site_by_year_temp.csv")
load("../../Data/temp_rmi.rda")


## RMI
RMI <- dat$site.RMI$RMI_p
mean_rmi <- mean(RMI,na.rm = TRUE)
sd_rmi <- sd(RMI,na.rm = TRUE)
std_rmi <- (RMI - mean_rmi ) / sd_rmi




### sensitivity to parameters-- without any competiton
library(pomp)
eff_size <- sobolDesign(lower =c(true_alpha.psiS = 0,
                                 true_alpha.psiC= 0, 
                                 true_alpha.survS = 0,
                                 true_alpha.colS = 0,
                                 true_beta.survS1= 0,
                                 true_beta.colS1= 0, 
                                 true_beta.survS2= 0,
                                 true_beta.colS2= 0,
                                 true_alpha.survC= 0,
                                 true_alpha.colC= 0,
                                 true_beta.survC1 = 0,
                                 true_beta.colC1= 0,
                                 true_beta.survC2= 0,
                                 true_beta.colC2= 0,
                                 true_alpha.pS = 0, 
                                 true_alpha.pC= 0), 
                        upper =c(true_alpha.psiS = 1,
                                 true_alpha.psiC= 1, 
                                 true_alpha.survS = 1,
                                 true_alpha.colS = 1,
                                 true_beta.survS1= 1,
                                 true_beta.colS1= 1, 
                                 true_beta.survS2= 1,
                                 true_beta.colS2= 1,
                                 true_alpha.survC= 1,
                                 true_alpha.colC= 1,
                                 true_beta.survC1 = 1,
                                 true_beta.colC1= 1,
                                 true_beta.survC2= 1,
                                 true_beta.colC2= 1,
                                 true_alpha.pS = 1,
                                 true_alpha.pC = 1),
                        nseq = 500
)


## dataframe for occupancy prob
occupancy_prob <- data.frame(
  occ_v = numeric(length = nrow(eff_size)),
  occ_z = numeric(length = nrow(eff_size)),
  col_prob_v = numeric(length = nrow(eff_size)),   ## v and z should be identical in this simulation
  surv_prob_v = numeric(length = nrow(eff_size)),
  col_prob_z = numeric(length = nrow(eff_size)), 
  surv_prob_z = numeric(length = nrow(eff_size)),
  alpha.psiS= numeric(length = nrow(eff_size)),
  alpha.psiC= numeric(length = nrow(eff_size)),
  alpha.survS = numeric(length = nrow(eff_size)),
  alpha.colS = numeric(length = nrow(eff_size)),
  beta.survS1= numeric(length = nrow(eff_size)),
  beta.colS1= numeric(length = nrow(eff_size)),
  beta.survS2= numeric(length = nrow(eff_size)),
  beta.colS2= numeric(length = nrow(eff_size)),
  alpha.survC = numeric(length = nrow(eff_size)),
  alpha.colC = numeric(length = nrow(eff_size)),
  beta.survC1 = numeric(length = nrow(eff_size)),
  beta.colC1 = numeric(length = nrow(eff_size)),
  beta.survC2 = numeric(length = nrow(eff_size)),
  beta.colC2 = numeric(length = nrow(eff_size))
)

## input for occupancy
sp1_occ <- array(data = NA, dim= c(200,5,10)) 
sp1_occ[, ,] <- as.numeric(sp1_occ[, , ])
sp2_occ <- sp1_occ

## at least for now ignore detection probabilities-- probabally can be excluded from sensitivity analysis

params <- c("muv","muz","survS", "colS", "survC", "colC",
            
            "alpha.psiS", 
             "alpha.psiC ",
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
              "beta.colC2" 
)

for (i in 1:nrow(eff_size)){
  
    
    #bundle data
    dat_sim <-  list(
      N = dim(sp1_occ)[1],
      TEMP = std_temp2,
      Yr = dim(sp1_occ)[3],
      RMI = std_rmi,
      #J = dim(sp1_occ)[2],
      
      true_alpha.psiS = eff_size$true_alpha.psiS[i],
      true_alpha.psiC= eff_size$true_alpha.psiC[i], 
      true_alpha.survS = eff_size$true_alpha.survS[i],
      true_alpha.colS = eff_size$true_alpha.colS[i],
      true_beta.survS1= eff_size$true_beta.survS1[i],
      true_beta_colS1= eff_size$true_beta.colS1[i], 
      true_beta.survS2= eff_size$true_beta.survS2[i],
      true_beta.colS2= eff_size$ true_beta.colS2[i],
      true_alpha.survC= eff_size$true_alpha.survC[i],
      true_alpha.colC= eff_size$true_alpha.colC[i],
      true_beta.survC1 = eff_size$true_beta.survC1[i],
      true_beta.colC1= eff_size$true_beta.colC1[i],
      true_beta.survC2= eff_size$true_beta.survC2[i],
      true_beta.colC2= eff_size$true_beta.colC2[i]#,
      #true_alpha.pS = eff_size$true_alpha.pS[i], 
      #true_alpha.pC= eff_size$true_alpha.pC[i]
      )
      
  
    
    
    
    
    
    SS.pred <- jags(model.file = "../../models/no_comp_sensitivity.txt", 
                    data = dat_sim, 
                    parameters.to.save = params,
                    n.chains = 3, 
                    #inits = 1000,
                    n.iter = 2000,
                    n.thin = 1,
                    #n.adapt = n.adapt,
                    #n.burnin = n.burnin,
                    parallel = TRUE)
 
    
    
  ## what outputs matter?? colonization probability after 10 yrs?   
    
    occupancy_prob$occ_v[i] <- median(SS.pred$q50$muv[,dim(SS.pred$q50$muv)[2]]) 
    occupancy_prob$occ_z [i] <- median(SS.pred$q50$muz[,dim(SS.pred$q50$muz)[2]])
    occupancy_prob$col_prob_v[i] <- median(SS.pred$q50$survC[,dim(SS.pred$q50$survC)[2]])   ## v and z should be identical in this simulation
    occupancy_prob$surv_prob_v[i] <- median(SS.pred$q50$colC[,dim(SS.pred$q50$colC)[2]]) 
    occupancy_prob$col_prob_z[i] <- median(SS.pred$q50$survS[,dim(SS.pred$q50$survS)[2]]) 
    occupancy_prob$surv_prob_z[i] <- median(SS.pred$q50$colS[,dim(SS.pred$q50$colS)[2]]) 
    occupancy_prob$alpha.psiS[i] <- SS.pred$q50$alpha.psiS
    occupancy_prob$alpha.psiC[i]<- SS.pred$q50$colS
    occupancy_prob$alpha.survS[i] <- SS.pred$q50$alpha.survS
    occupancy_prob$alpha.colS[i] <- SS.pred$q50$alpha.colS 
    occupancy_prob$beta.survS1[i]<- SS.pred$q50$beta.survS1
    occupancy_prob$beta.colS1[i]<- SS.pred$q50$colS
    occupancy_prob$beta.survS2[i]<- SS.pred$q50$beta.survS2
    occupancy_prob$beta.colS2[i]<- SS.pred$q50$beta.colS2 
    occupancy_prob$alpha.survC[i]<- SS.pred$q50$alpha.survC
    occupancy_prob$alpha.colC[i]<- SS.pred$q50$alpha.colC 
    occupancy_prob$beta.survC1[i] <- SS.pred$q50$beta.survC1
    occupancy_prob$beta.colC1[i]<- SS.pred$q50$beta.colC1 
    occupancy_prob$beta.survC2[i]<- SS.pred$q50$beta.survC2
    occupancy_prob$beta.colC2[i]<- SS.pred$q50$beta.colC2

    print(i)
  }
  
  


write.csv(occupancy_prob, file = "no_comp_sensitivity.csv")
  
occ <- read.csv("no_comp_sensitivity.csv")
occ <- occ %>% dplyr::select(-X)

occ2 <- occ %>% 
  pivot_longer(-c(occ_v,occ_z, col_prob_v, col_prob_z, surv_prob_v, surv_prob_z),
               values_to = "value", names_to = "param")

colonization_v <- ggplot(occ2, aes(value, col_prob_v)) + geom_point() + facet_wrap(~param)
print(colonization_v)


colonization_z <- ggplot(occ2, aes(value, col_prob_z)) + geom_point() + facet_wrap(~param)
print(colonization_z)

survival_v <- ggplot(occ2, aes(value, surv_prob_v)) + geom_point() + facet_wrap(~param)
print(survival_v)


survival_z <- ggplot(occ2, aes(value, surv_prob_z)) + geom_point() + facet_wrap(~param)
print(survival_z)

true_occ_v <- ggplot(occ2, aes(value, occ_v)) + geom_point() + facet_wrap(~param)
print(true_occ_v)

true_occ_z <- ggplot(occ2, aes(value, occ_z)) + geom_point() + facet_wrap(~param)
print(true_occ_z)


ggplot(occ2, aes(occ_v, col_prob_v)) +geom_point()
ggplot(occ2, aes(occ_z, col_prob_z)) +geom_point()

ggplot(occ2, aes(occ_v, surv_prob_v))+ geom_point()
ggplot(occ2, aes(occ_z, surv_prob_z))+ geom_point()



### with competition
eff_size_comp <- sobolDesign(lower =c(true_alpha.psiSC =0 ,
                                 true_alpha.psiSc=0, 
                                 true_alpha.psiC=0, 
                                 
                                 # Intercept
                               true_alpha.survAloneS=0, 
                               true_alpha.survBothS=0,
                               true_alpha.colEmptyS=0, 
                               true_alpha.colOccS=0,
                                 
                                 # TEMP slope
                               true_beta.survAloneS1=0,
                               true_beta.survBothS1=0, 
                               true_beta.colEmptyS1=0, 
                               true_beta.colOccS1=0, 
                                 
                                 # RMI slope
                                true_beta.survAloneS2=0,
                                true_beta.survBothS2=0, 
                                true_beta.colEmptyS2=0, 
                                true_beta.colOccS2=0, 
                                 
                                 # Intercept
                                 true_alpha.survAloneC=0, 
                                 true_alpha.survBothC=0,
                                 true_alpha.colEmptyC=0, 
                                 true_alpha.colOccC=0, 
                                 
                                 # TEMP slope
                                true_beta.survAloneC1=0,
                                true_beta.survBothC1=0, 
                                true_beta.colEmptyC1=0, 
                                true_beta.colOccC1=0, 
                                 
                                 # RMI slope
                                true_beta.survAloneC2=0, 
                                true_beta.survBothC2=0, 
                                true_beta.colEmptyC2=0, 
                                true_beta.colOccC2=0) , 
                        upper =c(true_alpha.psiSC =1 ,
                                 true_alpha.psiSc=1, 
                                 true_alpha.psiC=1, 
                                 
                                 # Intercept
                                 true_alpha.survAloneS=1, 
                                 true_alpha.survBothS=1,
                                 true_alpha.colEmptyS=1, 
                                 true_alpha.colOccS=1,
                                 
                                 # TEMP slope
                                 true_beta.survAloneS1=1,
                                 true_beta.survBothS1=1, 
                                 true_beta.colEmptyS1=1, 
                                 true_beta.colOccS1=1, 
                                 
                                 # RMI slope
                                 true_beta.survAloneS2=1,
                                 true_beta.survBothS2=1, 
                                 true_beta.colEmptyS2=1, 
                                 true_beta.colOccS2=1, 
                                 
                                 # Intercept
                                 true_alpha.survAloneC=1, 
                                 true_alpha.survBothC=1,
                                 true_alpha.colEmptyC=1, 
                                 true_alpha.colOccC=1, 
                                 
                                 # TEMP slope
                                 true_beta.survAloneC1=1,
                                 true_beta.survBothC1=1, 
                                 true_beta.colEmptyC1=1, 
                                 true_beta.colOccC1=1, 
                                 
                                 # RMI slope
                                 true_beta.survAloneC2=1, 
                                 true_beta.survBothC2=1, 
                                 true_beta.colEmptyC2=1, 
                                 true_beta.colOccC2=1),
                        nseq =700
)


## dataframe for occupancy prob
occupancy_prob_comp <- data.frame(
  occ_v = numeric(length = nrow(eff_size_comp)),
  occ_z =numeric(length = nrow(eff_size_comp)),
  col_prob_v = numeric(length = nrow(eff_size_comp)),   ## v and z should be identical in this simulation
  surv_prob_v = numeric(length = nrow(eff_size_comp)),
  col_prob_z = numeric(length = nrow(eff_size_comp)), 
  surv_prob_z = numeric(length = nrow(eff_size_comp)),
  true_alpha.psiSC = numeric(length = nrow(eff_size_comp)) ,
  true_alpha.psiSc= numeric(length = nrow(eff_size_comp)), 
  true_alpha.psiC= numeric(length = nrow(eff_size_comp)), 
  true_alpha.survAloneS=numeric(length = nrow(eff_size_comp)), 
  true_alpha.survBothS=numeric(length = nrow(eff_size_comp)),
  true_alpha.colEmptyS=numeric(length = nrow(eff_size_comp)), 
  true_alpha.colOccS=numeric(length = nrow(eff_size_comp)),
  true_beta.survAloneS1=numeric(length = nrow(eff_size_comp)),
  true_beta.survBothS1=numeric(length = nrow(eff_size_comp)), 
  true_beta.colEmptyS1=numeric(length = nrow(eff_size_comp)), 
  true_beta.colOccS1=numeric(length = nrow(eff_size_comp)), 
  true_beta.survAloneS2=numeric(length = nrow(eff_size_comp)),
  true_beta.survBothS2=numeric(length = nrow(eff_size_comp)), 
  true_beta.colEmptyS2=numeric(length = nrow(eff_size_comp)), 
  true_beta.colOccS2=numeric(length = nrow(eff_size_comp)), 
  true_alpha.survAloneC=numeric(length = nrow(eff_size_comp)), 
  true_alpha.survBothC=numeric(length = nrow(eff_size_comp)),
  true_alpha.colEmptyC=numeric(length = nrow(eff_size_comp)), 
  true_alpha.colOccC=numeric(length = nrow(eff_size_comp)), 
  true_beta.survAloneC1=numeric(length = nrow(eff_size_comp)),
  true_beta.survBothC1=numeric(length = nrow(eff_size_comp)), 
  true_beta.colEmptyC1=numeric(length = nrow(eff_size_comp)), 
  true_beta.colOccC1=numeric(length = nrow(eff_size_comp)), 
  true_beta.survAloneC2=numeric(length = nrow(eff_size_comp)), 
  true_beta.survBothC2=numeric(length = nrow(eff_size_comp)), 
  true_beta.colEmptyC2=numeric(length = nrow(eff_size_comp)), 
  true_beta.colOccC2=numeric(length = nrow(eff_size_comp)),
 colB_prob_v=numeric(length = nrow(eff_size_comp)),   ## v and z should be identical in this simulation
  colA_prob_v=numeric(length = nrow(eff_size_comp)),
  survB_prob_v=numeric(length = nrow(eff_size_comp)),
  survA_prob_v=numeric(length = nrow(eff_size_comp)),
  colB_prob_z =numeric(length = nrow(eff_size_comp)),  ## v and z should be identical in this simulation
 colA_prob_z=numeric(length = nrow(eff_size_comp)),
  survB_prob_z=numeric(length = nrow(eff_size_comp)),
  survA_prob_z=numeric(length = nrow(eff_size_comp)))
  
  


## input for occupancy
sp1_occ <- array(data = NA, dim= c(200,5,10)) 
sp1_occ[, ,] <- as.numeric(sp1_occ[, , ])
sp2_occ <- sp1_occ

## at least for now ignore detection probabilities-- probabally can be excluded from sensitivity analysis

params_comp <- c("muv","muz","survAloneS", "colS", "survC", "colC","survBothS", "colEmptyS","colOccS",
                 "survAloneC", "survBothC","colEmptyC", "colOccC",
                 
                 "alpha.psiSC",
                 "alpha.psiSc",
                 "alpha.psiC",
                "alpha.survAloneS",
                 "alpha.survBothS" ,
                 "alpha.colEmptyS" ,
                 "alpha.colOccS",
                 "beta.survAloneS1",
                 "beta.survBothS1",
                 "beta.colEmptyS1" ,
                 "beta.colOccS1",
                 "beta.survAloneS2",
                 "beta.survBothS2",
                 "beta.colEmptyS2", 
                 "beta.colOccS2" ,
                 "alpha.survAloneC", 
                 "alpha.survBothC" ,
                 "alpha.colEmptyC",
                 "alpha.colOccC" ,
                 "beta.survAloneC1", 
                 "beta.survBothC1",
                 "beta.colEmptyC1" ,
                 "beta.colOccC1",
                 "beta.survAloneC2",
                 "beta.survBothC2",
                 "beta.colEmptyC2" ,
                 "beta.colOccC2"
)

for (i in 1:nrow(eff_size_comp)){
  
  
  #bundle data
  dat_sim <-  list(
    N = dim(sp1_occ)[1],
    TEMP = std_temp2,
    Yr = dim(sp1_occ)[3],
    RMI = std_rmi,
    #J = dim(sp1_occ)[2]
 
    true_alpha.psiSC = eff_size_comp$true_alpha.psiSC[i] ,
    true_alpha.psiSc= eff_size_comp$true_alpha.psiSc[i], 
    true_alpha.psiC= eff_size_comp$true_alpha.psiC[i], 
    true_alpha.survAloneS=eff_size_comp$true_alpha.survAloneS[i], 
    true_alpha.survBothS=eff_size_comp$true_alpha.survBothS[i],
    true_alpha.colEmptyS=eff_size_comp$true_alpha.colEmptyS[i], 
    true_alpha.colOccS=eff_size_comp$true_alpha.colOccS[i],
    true_beta.survAloneS1= eff_size_comp$ true_beta.survAloneS1[i],
    true_beta.survBothS1= eff_size_comp$true_beta.survBothS1[i], 
    true_beta.colEmptyS1=eff_size_comp$true_beta.colEmptyS1[i], 
    true_beta.colOccS1=eff_size_comp$true_beta.colOccS1[i], 
    true_beta.survAloneS2=eff_size_comp$true_beta.survAloneS2[i],
    true_beta.survBothS2=eff_size_comp$true_beta.survBothS2[i], 
    true_beta.colEmptyS2=eff_size_comp$true_beta.colEmptyS2[i], 
    true_beta.colOccS2=eff_size_comp$true_beta.colOccS2[i], 
    true_alpha.survAloneC=eff_size_comp$true_alpha.survAloneC[i], 
    true_alpha.survBothC=eff_size_comp$ true_alpha.survBothC[i],
    true_alpha.colEmptyC=eff_size_comp$ true_alpha.colEmptyC[i], 
    true_alpha.colOccC=eff_size_comp$true_alpha.colOccC[i], 
    true_beta.survAloneC1=eff_size_comp$true_beta.survAloneC1[i],
    true_beta.survBothC1=eff_size_comp$true_beta.survBothC1[i], 
    true_beta.colEmptyC1=eff_size_comp$ true_beta.colEmptyC1[i], 
    true_beta.colOccC1=eff_size_comp$true_beta.colOccC1[i], 
    true_beta.survAloneC2=eff_size_comp$true_beta.survAloneC2[i], 
    true_beta.survBothC2=eff_size_comp$true_beta.survBothC2[i], 
    true_beta.colEmptyC2=eff_size_comp$true_beta.colEmptyC2[i], 
    true_beta.colOccC2=eff_size_comp$true_beta.colOccC2[i]
  )
  
  
  
  
  
  
  
  SS.pred <- jags(model.file = "../../models/two_species_comp_sensitivity.txt", 
                  data = dat_sim, 
                  parameters.to.save = params_comp,
                  n.chains = 3, 
                  #inits = 1000,
                  n.iter = 2000,
                  n.thin = 1,
                  #n.adapt = n.adapt,
                  #n.burnin = n.burnin,
                  parallel = TRUE)
  
  
  
  ## what outputs matter?? colonization probability after 10 yrs?   
  
  occupancy_prob_comp$occ_v[i] <- median(SS.pred$q50$muv[,dim(SS.pred$q50$muv)[2]]) 
  occupancy_prob_comp$occ_z [i] <- median(SS.pred$q50$muz[,dim(SS.pred$q50$muz)[2]])
  
  occupancy_prob_comp$colB_prob_v[i] <- median(SS.pred$q50$colOccC[,dim(SS.pred$q50$survC)[2]])   ## v and z should be identical in this simulation
  occupancy_prob_comp$colA_prob_v[i] <- median(SS.pred$q50$colEmptyC[,dim(SS.pred$q50$survC)[2]])
  occupancy_prob_comp$survB_prob_v[i] <- median(SS.pred$q50$survBothC[,dim(SS.pred$q50$colC)[2]])
  occupancy_prob_comp$survA_prob_v[i] <- median(SS.pred$q50$survAloneC[,dim(SS.pred$q50$colC)[2]])
  
  occupancy_prob_comp$colB_prob_z[i] <- median(SS.pred$q50$colOccS[,dim(SS.pred$q50$survC)[2]])   ## v and z should be identical in this simulation
  occupancy_prob_comp$colA_prob_z[i] <- median(SS.pred$q50$colEmptyS[,dim(SS.pred$q50$survC)[2]])
  occupancy_prob_comp$survB_prob_z[i] <- median(SS.pred$q50$survBothS[,dim(SS.pred$q50$colC)[2]])
  occupancy_prob_comp$survA_prob_z[i] <- median(SS.pred$q50$survAloneS[,dim(SS.pred$q50$colC)[2]])
  
  
  
  
  
  occupancy_prob_comp$alpha.psiSC[i]  <- SS.pred$q50$alpha.psiSC
  occupancy_prob_comp$alpha.psiSc[i]<- SS.pred$q50$alpha.psiSc
  occupancy_prob_comp$alpha.psiC[i]<- SS.pred$q50$alpha.psiC
  occupancy_prob_comp$alpha.survAloneS[i]<- SS.pred$q50$alpha.survAloneS
  occupancy_prob_comp$alpha.survBothS[i]<- SS.pred$q50$alpha.survBothS
  occupancy_prob_comp$alpha.colEmptyS[i]<- SS.pred$q50$alpha.colEmptyS
  occupancy_prob_comp$alpha.colOccS[i]<- SS.pred$q50$alpha.colOccS
  occupancy_prob_comp$beta.survAloneS1[i]<- SS.pred$q50$beta.survAloneS1
  occupancy_prob_comp$beta.survBothS1[i]<- SS.pred$q50$beta.survBothS1
  occupancy_prob_comp$beta.colEmptyS1[i]<- SS.pred$q50$beta.colEmptyS1
  occupancy_prob_comp$beta.colOccS1[i]<- SS.pred$q50$beta.colOccS1
  occupancy_prob_comp$beta.survAloneS2[i]<- SS.pred$q50$beta.survAloneS2
  occupancy_prob_comp$beta.survBothS2[i]<- SS.pred$q50$beta.survBothS2
  occupancy_prob_comp$beta.colEmptyS2[i]<- SS.pred$q50$beta.colEmptyS2 
  occupancy_prob_comp$beta.colOccS2[i]<- SS.pred$q50$beta.colOccS2
  occupancy_prob_comp$alpha.survAloneC[i] <- SS.pred$q50$alpha.survAloneC
  occupancy_prob_comp$alpha.survBothC[i]<- SS.pred$q50$alpha.survBothC
  occupancy_prob_comp$alpha.colEmptyC[i]<- SS.pred$q50$alpha.colEmptyC
  occupancy_prob_comp$alpha.colOccC[i]<- SS.pred$q50$alpha.colOccC
  occupancy_prob_comp$beta.survAloneC1[i] <- SS.pred$q50$beta.survAloneC1
  occupancy_prob_comp$beta.survBothC1[i]<- SS.pred$q50$beta.survBothC1
  occupancy_prob_comp$beta.colEmptyC1[i]<- SS.pred$q50$beta.colEmptyC1
  occupancy_prob_comp$beta.colOccC1[i]<- SS.pred$q50$beta.colOccC1
  occupancy_prob_comp$beta.survAloneC2[i]<- SS.pred$q50$beta.survAloneC2
  occupancy_prob_comp$beta.survBothC2[i]<- SS.pred$q50$beta.survBothC2
  occupancy_prob_comp$beta.colEmptyC2[i]<- SS.pred$q50$beta.colEmptyC2
  occupancy_prob_comp$beta.colOccC2[i]<- SS.pred$q50$beta.colOccC2
  
  print(i)
}


write.csv(occupancy_prob_comp, file = "comp_sensitivity.csv")

occ <- read.csv("comp_sensitivity.csv")
occ <- occ %>% dplyr::select(-X)

occ2 <- occ %>% 
  pivot_longer(-c(occ_v,occ_z, col_prob_v, col_prob_z, surv_prob_v, surv_prob_z),
               values_to = "value", names_to = "param")

colonization_v <- ggplot(occ2, aes(value, col_prob_v)) + geom_point() + facet_wrap(~param)
print(colonization_v)


colonization_z <- ggplot(occ2, aes(value, col_prob_z)) + geom_point() + facet_wrap(~param)
print(colonization_z)

survival_v <- ggplot(occ2, aes(value, surv_prob_v)) + geom_point() + facet_wrap(~param)
print(survival_v)


survival_z <- ggplot(occ2, aes(value, surv_prob_z)) + geom_point() + facet_wrap(~param)
print(survival_z)

true_occ_v <- ggplot(occ2, aes(value, occ_v)) + geom_point() + facet_wrap(~param)
print(true_occ_v)

true_occ_z <- ggplot(occ2, aes(value, occ_z)) + geom_point() + facet_wrap(~param)
print(true_occ_z)


ggplot(occ2, aes(occ_v, col_prob_v)) +geom_point()
ggplot(occ2, aes(occ_z, col_prob_z)) +geom_point()

ggplot(occ2, aes(occ_v, surv_prob_v))+ geom_point()
ggplot(occ2, aes(occ_z, surv_prob_z))+ geom_point()

