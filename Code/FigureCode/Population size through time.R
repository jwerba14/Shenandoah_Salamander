# This code was written by: G. V. DiRenzo
# If you have any questions, please send them to: grace.direnzo@gmail.com

# Code Objective:
  # To create tables & figures for PVA simualtions

##################################
###### Table of Contents #########
##################################

# 1. Load libraries
# 2. Set working directory
# 3. Set arguments 
# 4. Load in data
# 5. Format data
# 6. Summarise data
# 7. Save the summary

##################################
##################################
##################################

# 1. Load libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot)


# 2. Set working directory ---------------------------------------------------------------

# set working directory
setwd("/Volumes/GVD/lustre 4/")


# 3. load data --------------------------------------------------------

## Class
#  BioticAbiotic
#  Abiotic
#  Biotic
#  Null
#
## TEMP
#  temp_mat_range_SCEN1
#  temp_mat_range_SCEN2
#  
## RMI
#  RMI_mat_range_sub
#  RMI_mat_range_add
  
# Load the data

# ------------------------ BIOTIC ABIOTIC
##### Linear
load("./PVA_summary/BioticAbiotic/temp_mat_range_SCEN2_RMI_mat_range_sub/BioticAbiotic_temp_mat_range_SCEN2_RMI_mat_range_sub_pop_size_summary.rda")
BioticAbiotic_LIN_Scen1_sub <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 1",
                                          RMI = "Sub")

load("./PVA_summary/BioticAbiotic/temp_mat_range_SCEN1_RMI_mat_range_add/BioticAbiotic_temp_mat_range_SCEN1_RMI_mat_range_add_pop_size_summary.rda")
BioticAbiotic_LIN_Scen1_add <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 1",
                                          RMI = "Add")

load("./PVA_summary/BioticAbiotic/temp_mat_range_SCEN2_RMI_mat_range_sub/BioticAbiotic_temp_mat_range_SCEN2_RMI_mat_range_sub_pop_size_summary.rda")
BioticAbiotic_LIN_Scen2_sub <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 2",
                                          RMI = "Sub")

load("./PVA_summary/BioticAbiotic/temp_mat_range_SCEN2_RMI_mat_range_add/BioticAbiotic_temp_mat_range_SCEN2_RMI_mat_range_add_pop_size_summary.rda")
BioticAbiotic_LIN_Scen2_add <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 2",
                                          RMI = "Add")

#### Exponential

load("./PVA_summary/BioticAbiotic/temp_mat_exp_range_SCEN1_RMI_mat_range_sub/BioticAbiotic_temp_mat_exp_range_SCEN1_RMI_mat_range_sub_pop_size_summary.rda")
BioticAbiotic_EXP_Scen1_sub <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Exponential increase",
                                          Climate = "Scenario 1",
                                          RMI = "Sub")

load("./PVA_summary/BioticAbiotic/temp_mat_exp_range_SCEN1_RMI_mat_range_add/BioticAbiotic_temp_mat_exp_range_SCEN1_RMI_mat_range_add_pop_size_summary.rda")
BioticAbiotic_EXP_Scen1_add <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Exponential increase",
                                          Climate = "Scenario 1",
                                          RMI = "Add")

load("./PVA_summary/BioticAbiotic/temp_mat_exp_range_SCEN2_RMI_mat_range_sub/BioticAbiotic_temp_mat_exp_range_SCEN2_RMI_mat_range_sub_pop_size_summary.rda")
BioticAbiotic_EXP_Scen2_sub <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Exponential increase",
                                          Climate = "Scenario 2",
                                          RMI = "Sub")

load("./PVA_summary/BioticAbiotic/temp_mat_exp_range_SCEN2_RMI_mat_range_add/BioticAbiotic_temp_mat_exp_range_SCEN2_RMI_mat_range_add_pop_size_summary.rda")
BioticAbiotic_EXP_Scen2_add <- data.frame(tot_pop,
                                          Scenario = "Biotic Abiotic",
                                          Temp = "Exponential increase",
                                          Climate = "Scenario 2",
                                          RMI = "Add")



# ------------------------ ABIOTIC
##### Linear
load("./PVA_summary/Abiotic/temp_mat_range_SCEN2_RMI_mat_range_sub/Abiotic_temp_mat_range_SCEN2_RMI_mat_range_sub_pop_size_summary.rda")
Abiotic_LIN_Scen1_sub <- data.frame(tot_pop,
                                          Scenario = "Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 1",
                                          RMI = "Sub")

load("./PVA_summary/Abiotic/temp_mat_range_SCEN1_RMI_mat_range_add/Abiotic_temp_mat_range_SCEN1_RMI_mat_range_add_pop_size_summary.rda")
Abiotic_LIN_Scen1_add <- data.frame(tot_pop,
                                          Scenario = "Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 1",
                                          RMI = "Add")

load("./PVA_summary/Abiotic/temp_mat_range_SCEN2_RMI_mat_range_sub/Abiotic_temp_mat_range_SCEN2_RMI_mat_range_sub_pop_size_summary.rda")
Abiotic_LIN_Scen2_sub <- data.frame(tot_pop,
                                          Scenario = "Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 2",
                                          RMI = "Sub")

load("./PVA_summary/Abiotic/temp_mat_range_SCEN2_RMI_mat_range_add/Abiotic_temp_mat_range_SCEN2_RMI_mat_range_add_pop_size_summary.rda")
Abiotic_LIN_Scen2_add <- data.frame(tot_pop,
                                          Scenario = "Abiotic",
                                          Temp = "Linear increase",
                                          Climate = "Scenario 2",
                                          RMI = "Add")

#### Exponential

load("./PVA_summary/Abiotic/temp_mat_exp_range_SCEN2_RMI_mat_range_sub/Abiotic_temp_mat_exp_range_SCEN2_RMI_mat_range_sub_pop_size_summary.rda")
Abiotic_EXP_Scen1_sub <- data.frame(tot_pop,
                                          Scenario = "Abiotic",
                                          Temp = "Exponential increase",
                                          Climate = "Scenario 1",
                                          RMI = "Sub")

load("./PVA_summary/Abiotic/temp_mat_exp_range_SCEN1_RMI_mat_range_add/Abiotic_temp_mat_exp_range_SCEN1_RMI_mat_range_add_pop_size_summary.rda")
Abiotic_EXP_Scen1_add <- data.frame(tot_pop,
                                          Scenario = "Abiotic",
                                          Temp = "Exponential increase",
                                          Climate = "Scenario 1",
                                          RMI = "Add")

load("./PVA_summary/Abiotic/temp_mat_exp_range_SCEN2_RMI_mat_range_sub/Abiotic_temp_mat_exp_range_SCEN2_RMI_mat_range_sub_pop_size_summary.rda")
Abiotic_EXP_Scen2_sub <- data.frame(tot_pop,
                                          Scenario = "Abiotic",
                                          Temp = "Exponential increase",
                                          Climate = "Scenario 2",
                                          RMI = "Sub")

load("./PVA_summary/Abiotic/temp_mat_exp_range_SCEN2_RMI_mat_range_add/Abiotic_temp_mat_exp_range_SCEN2_RMI_mat_range_add_pop_size_summary.rda")
Abiotic_EXP_Scen2_add <- data.frame(tot_pop,
                                    Scenario = "Abiotic",
                                    Temp = "Exponential increase",
                                    Climate = "Scenario 2",
                                    RMI = "Add")


load("./PVA_summary/Biotic/Biotic_pop_size_summary.rda")
Biotic <- data.frame(tot_pop,
                     Scenario = "Biotic",
                     Temp = "",
                     Climate = "",
                     RMI = "")

load("./PVA_summary/Null/Null_pop_size_summary.rda")
Null <- data.frame(tot_pop,
                     Scenario = "Null",
                     Temp = "",
                     Climate = "",
                     RMI = "")

dat <- rbind(BioticAbiotic_LIN_Scen1_sub, BioticAbiotic_LIN_Scen1_add, 
             BioticAbiotic_LIN_Scen2_sub, BioticAbiotic_LIN_Scen2_add,
             BioticAbiotic_EXP_Scen1_sub, BioticAbiotic_EXP_Scen1_add, 
             BioticAbiotic_EXP_Scen2_sub, BioticAbiotic_EXP_Scen2_add,
             Abiotic_LIN_Scen1_sub, Abiotic_LIN_Scen1_add, 
             Abiotic_LIN_Scen2_sub, Abiotic_LIN_Scen2_add,
             Abiotic_EXP_Scen1_sub, Abiotic_EXP_Scen1_add, 
             Abiotic_EXP_Scen2_sub, Abiotic_EXP_Scen2_add)
dat2 <- rbind(Biotic,
               Null)

# Replace species names
dat$species <- ifelse(dat$species == "occC", "P. cinereus", dat$species)
dat$species <- ifelse(dat$species == "occS", "P. shenandoah", dat$species)

# Replace species names
dat2$species <- ifelse(dat2$species == "occC", "P. cinereus", dat2$species)
dat2$species <- ifelse(dat2$species == "occS", "P. shenandoah", dat2$species)

# 3. Create a plot --------------------------------------------------------

Abiotic_BioticAbiotic <- 
  ggplot(data = dat, aes(x = year+2016, y = pop_mean/617, col = species)) +
  geom_point()+
  geom_pointrange(aes(ymin = pop_q2.5/617, ymax = pop_q97.5/617)) +
  facet_grid(Scenario ~ Climate +  Temp + RMI)+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylab("Proportion of sites occupied")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        legend.position = "top")+
  labs(col = "Species")

Biotic_Null <- 
  ggplot(data = dat2, aes(x = year+2016, y = pop_mean/617, col = species)) +
  geom_point()+
  geom_pointrange(aes(ymin = pop_q2.5/617, ymax = pop_q97.5/617)) +
  facet_grid( ~ Scenario)+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylab("Proportion of sites occupied")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        legend.position = "none")

# Put all the pieces together for the figure
plot_grid(Abiotic_BioticAbiotic, Biotic_Null, ncol = 1, rel_heights = c(7, 3))

# Save the plot
ggsave("/Volumes/GVD/Yeti/ShenSal/Figures/PVA_pop_size_AbioticBiotic_params.pdf", height = 13, width = 20)

# End script