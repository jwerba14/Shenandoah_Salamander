


setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")
load("./Data/formatted_shen_cin_dat.rda")


str(dat$shen)

z_pshen <- apply(dat$shen, c(1, 3), max, na.rm = TRUE)
z_pshen[z_pshen == "-Inf"] <- -9999


z_cin <- apply(dat$cin, c(1, 3), max, na.rm = TRUE)
z_cin[z_cin == "-Inf"] <- -9999


# Determine the proportion of sites where:
  # Pshen was present at time t and persisted at time t + 1
    # and Pcin was present
    # and Pcin was absent
  # Pshen was not present at time t and colonized at time t + 1
    # and Pcin was present
    # and Pcin was absent

Survival.Pcin.absent <- numeric(ncol(z_pshen))
Survival.Pcin.present <- numeric(ncol(z_pshen))
Colonize.Pcin.absent <- numeric(ncol(z_pshen))
Colonize.Pcin.present <- numeric(ncol(z_pshen))
Both <- numeric(ncol(z_pshen))
Alone <- numeric(ncol(z_pshen))

for(i in 1:nrow(z_pshen)){
  for(j in 2:ncol(z_pshen)){
    

    if(z_pshen[i,j] > -9999 & z_pshen[i,j-1] > -9999){
      
      # Pshen is present
      # Pcin is present
      if(z_pshen[i,j] == 1 & z_cin[i,j] == 1){
        Both[j] <- Both[j] + 1
      }
      # Pshen is present
      # Pcin is present
      if(z_pshen[i,j] == 1 & z_cin[i,j] == 0){
        Alone[j] <- Alone[j] + 1
      }
      
      # Pshen was present at time t
      # Pshen was present at time t + 1
      # Pcin was present at time t
      if(z_pshen[i,j-1] == 1 & z_pshen[i,j] == 1 & z_cin[i,j-1] == 1){
        Survival.Pcin.present[j] <- Survival.Pcin.present[j] + 1
    }
    
    # Pshen was present at time t
    # Pshen was present at time t + 1
    # Pcin was absent at time t
    if(z_pshen[i,j-1] == 1 & z_pshen[i,j] == 1 & z_cin[i,j-1] == 0){
      Survival.Pcin.absent[j] <- Survival.Pcin.absent[j] + 1
  }
  
  # Pshen was absent at time t
  # Pshen was present at time t + 1
  # Pcin was absent at time t
  if(z_pshen[i,j-1] == 0 & z_pshen[i,j] == 1 & z_cin[i,j-1] == 0){
   Colonize.Pcin.absent[j] <- Colonize.Pcin.absent[j] + 1
}

  # Pshen was absent at time t
  # Pshen was present at time t + 1
  # Pcin was absent at time t
  if(z_pshen[i,j-1] == 0 & z_pshen[i,j] == 1 & z_cin[i,j-1] == 1){
    Colonize.Pcin.present[j] <- Colonize.Pcin.present[j] + 1
  }
  
    }
  } 
}


# Now determine the number of sites where Pshen occurred that year

sites <- z_pshen
sites[sites == -9999] <- NA
occupied <- apply(sites, 2, sum, na.rm = TRUE)

z_pshen[which(z_pshen[,ncol(z_pshen)] == 1),]


sites[sites == 0] <- 1


total.sites <- apply(sites, 2, sum, na.rm = TRUE)

SPP <- data.frame(prop = c(t(t(Survival.Pcin.present/total.sites)),
                          t(t(Survival.Pcin.absent  /total.sites)),
                          t(t(Colonize.Pcin.present /total.sites)),
                          t(t(Colonize.Pcin.absent  /total.sites)),
                          t(t(Both/total.sites)),
                          t(t(Alone/total.sites))
                          )
                  )
SPP$year <-  rep(2007:2016, times = 6)
SPP$condition <- rep(c("Pcin PRESENT @ t",
                       "Pcin ABSENT @ t",
                       "Pcin PRESENT @ t",
                       "Pcin ABSENT @ t",
                       "Pcin PRESENT @ t",
                       "Pcin ABSENT @ t"), each = length(2007:2016))
SPP$Process <- rep(c("Survival", "Survival", 
                     "Colonize", "Colonize",
                     "Occupancy", "Occupancy"), each = length(2007:2016))

SPP$Process <- factor(SPP$Process, levels = c("Colonize", "Survival", "Occupancy"))

SPP <- SPP[!is.na(SPP$prop),]


# Graph does not include sites that were occupied but with no information about previous year
# 2010 and 2015 were not surveyed
# So you should remove 2011 and 2016
SPP <- SPP[SPP$year != 2011,]
SPP <- SPP[SPP$year != 2016,]
# Remove the first year of data collection
SPP <- SPP[SPP$year != 2007,]


library(ggplot2)
ggplot()+
  geom_point(data = SPP, aes(x = year, y = prop, col = condition), size = 2)+
  geom_line(data = SPP, aes(x = year, y = prop, col = condition), lwd = 1.5)+
  facet_wrap(~Process)+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  theme_bw(17)+
  xlab("Year")+
  ylab("Proportion of sites")

ggsave("/Volumes/GVD/Yeti/ShenSal/Figures/Raw_survival_colonization.pdf", 
       height = 8,
       width = 12)
