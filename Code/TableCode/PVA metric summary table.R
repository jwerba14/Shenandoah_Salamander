# This code was written by: G. V. DiRenzo
# If you have any questions, please send them to: grace.direnzo@gmail.com

# Code Objective:
  # To synthesize the PVA summary output and create a table with final probabilities

##################################
###### Table of Contents #########
##################################

# 1. Load libraries
# 2. Set working directory
# 3. Load in data
# 4. Convert lists to data frames

##################################
##################################
##################################

# 1. Load libraries ---------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(ggplot2)

# 2. Set working directory ---------------------------------------------------------------

# set working directory
setwd("/Volumes/GVD/lustre 4/")


# 3. Load the data --------------------------------------------------------

# temp
#  var1 <- "Null"

# Set indicator object
a <- 1

# Create an empty object to hold the data
met02 <- list()
met05 <- list()
met10 <- list()
pop   <- list()

# Go through Null and Biotic scenarios
for(var1 in c("Null", "Biotic")){

# Create the path ID
path_ID <- paste0("./PVA_summary/", var1, "/")

# Determine which files to read in
nfiles.02 <- list.files(path = path_ID)[grep("metrics_02_per", list.files(path=path_ID))]
nfiles.05 <- list.files(path = path_ID)[grep("metrics_05_per", list.files(path=path_ID))]
nfiles.10 <- list.files(path = path_ID)[grep("metrics_10_per", list.files(path=path_ID))]
nfiles.pop <- list.files(path = path_ID)[grep("pop_size", list.files(path=path_ID))]

# Load in the data
load(paste0(path_ID, "/", nfiles.02))
load(paste0(path_ID, "/", nfiles.05))
load(paste0(path_ID, "/", nfiles.10))
load(paste0(path_ID, "/", nfiles.pop))

# Add new rows
met02[[a]] <- data.frame(model = var1,
                         Temp = NA,
                         RMI = NA,
                         calculation = "2%",
                         ext_02[ext_02$year == 60,])
met05[[a]] <- data.frame(model = var1,
                         Temp = NA,
                         RMI = NA,
                         calculation = "5%",
                         ext_05[ext_05$year == 60,])
met10[[a]] <- data.frame(model = var1,
                         Temp = NA,
                         RMI = NA,
                         calculation = "10%",
                         ext_10[ext_10$year == 60,])
pop[[a]]   <- data.frame(model = var1,
                         Temp = NA,
                         RMI = NA,
                         calculation = "final pop size",
                         tot_pop[tot_pop$year == 60,])
a <- a + 1

}


# # temp
# var1 <- "BioticAbiotic"
# var2 <- "temp_mat_range_SCEN1"
# var3 <- "RMI_mat_range_add"

# Go through the bioticabiotic and abiotic scenarios
for(var1 in c("BioticAbiotic", "Abiotic")){

  for(var2 in c("temp_mat_range_SCEN1", "temp_mat_exp_range_SCEN1",
                "temp_mat_range_SCEN2", "temp_mat_exp_range_SCEN2")){
    
    for(var3 in c("RMI_mat_range_add", "RMI_mat_range_sub")){
      
      # create the path ID
      path_ID <- paste0("./PVA_summary/", var1, "/", var2, "_", var3, "/")
      
      # Determine which files to read in
      nfiles.02 <- list.files(path = path_ID)[grep("metrics_02_per", list.files(path=path_ID))]
      nfiles.05 <- list.files(path = path_ID)[grep("metrics_05_per", list.files(path=path_ID))]
      nfiles.10 <- list.files(path = path_ID)[grep("metrics_10_per", list.files(path=path_ID))]
      nfiles.pop <- list.files(path = path_ID)[grep("pop_size", list.files(path=path_ID))]
      
      # Load in the data
      load(paste0(path_ID, "/", nfiles.02))
      load(paste0(path_ID, "/", nfiles.05))
      load(paste0(path_ID, "/", nfiles.10))
      load(paste0(path_ID, "/", nfiles.pop))
      
      # Add new rows
      met02[[a]] <- data.frame(model = var1,
                               Temp = var2,
                               RMI = var3,
                               calculation = "2%",
                               ext_02[ext_02$year == 60,])
      met05[[a]] <- data.frame(model = var1,
                               Temp = var2,
                               RMI = var3,
                               calculation = "5%",
                               ext_05[ext_05$year == 60,])
      met10[[a]] <- data.frame(model = var1,
                               Temp = var2,
                               RMI = var3,
                               calculation = "10%",
                               ext_10[ext_10$year == 60,])
      pop[[a]]   <- data.frame(model = var1,
                               Temp = var2,
                               RMI = var3,
                               calculation = "final pop size",
                               tot_pop[tot_pop$year == 60,])
      a <- a + 1
      
    }
    
  }
  
}


# 4. Convert lists into data frames ---------------------------------------------------------

# Convert list into 
met02 <- unlist(lapply(met02, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)
met05 <- unlist(lapply(met05, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)
met10 <- unlist(lapply(met10, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)
pop   <- unlist(lapply(pop,   function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)

# Rbind all lists in objects together
met02 <- do.call(rbind, met02)
met05 <- do.call(rbind, met05)
met10 <- do.call(rbind, met10)
pop <- do.call(rbind, pop)

# Create 1 object
metrics <- rbind(met02, met05, met10)


# Replace words in temp and RMI columns
metrics$Temp.new <- NA
metrics$Temp.new[grep("temp_mat_exp_range", metrics$Temp)] <- "Exponential increase"
metrics$Temp.new[grep("temp_mat_range", metrics$Temp)] <- "Linear increase"

metrics$Scenario.new <- NA
metrics$Scenario.new[grep("SCEN1", metrics$Temp)] <- "Scenario 1"
metrics$Scenario.new[grep("SCEN2", metrics$Temp)] <- "Scenario 2"

metrics$RMI[grep("RMI_mat_range_add", metrics$RMI)] <- "Add"
metrics$RMI[grep("RMI_mat_range_sub", metrics$RMI)] <- "Subtract"

# Replace species names
metrics$species <- ifelse(metrics$species == "occC", "P. cinereus", metrics$species)
metrics$species <- ifelse(metrics$species == "occS", "P. shenandoah", metrics$species)

# Split up the dataframe into: Biotic & Null vs. Abiotic & BioticAbiotic
bioNull <- metrics[metrics$model == "Null" | metrics$model == "Biotic",]
abioBiot <- metrics[metrics$model == "Abiotic" | metrics$model == "BioticAbiotic",]


# 5. Create a plots - Minimum population size --------------------------------------------------------

# Create plot
# Minimum population size
abioBiot_min_pop <- ggplot(data = abioBiot, aes( x = model, y = min_pop_size_mean/617, col= species))+
  geom_point()+
  facet_grid( ~ Scenario.new +  Temp.new + RMI)+
  geom_pointrange(aes(x = model, y = min_pop_size_mean/617, ymin = min_pop_size_q2.5/617, ymax = min_pop_size_q97.5/617))+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylab("Minimum occupancy in 60 years")+
  xlab("Model")+
  ylim(c(0, 1))+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.text.x = element_text(angle=45, hjust = 1),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        legend.position = "top")+
  labs(col = "Species")


bioNull_min_pop <- ggplot(data = bioNull, aes( x = model, y = min_pop_size_mean/617, col= species))+
  geom_point()+
  geom_pointrange(aes(x = model, y = min_pop_size_mean/617, ymin = min_pop_size_q2.5/617, ymax = min_pop_size_q97.5/617))+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylim(c(0, 1))+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        legend.position = "none")+
  labs(col = "Species")

empty_plot <- ggplot()+ theme_void()


obj1 <- plot_grid(empty_plot, bioNull_min_pop, empty_plot, rel_heights = c(0.22, 1, 0.08), ncol = 1)

plot_grid(abioBiot_min_pop, obj1, rel_widths = c(5, 0.5))

# Save the plot
ggsave("/Volumes/GVD/Yeti/ShenSal/Figures/Min_pop_size_AbioticBiotic_params.pdf", height = 8, width = 20)


# 6. Create a plots - Quasi extinction --------------------------------------------------------


# Create plot
# Quasi-extinction
abioBiot_quasi <- ggplot(data = abioBiot, aes( x = model, y = quasi_prob_mean, col= species))+
  geom_point()+
  facet_grid(calculation ~ Scenario.new +  Temp.new + RMI)+
  geom_pointrange(aes(x = model, y = quasi_prob_mean, ymin = quasi_prob_q2.5, ymax = quasi_prob_q97.5))+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylab("Quasi-extinction probability in 60 years")+
  xlab("Model")+
  ylim(c(0, 1))+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.text.x = element_text(angle=45, hjust = 1),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        legend.position = "top")+
  labs(col = "Species")


bioNull_quasi <- ggplot(data = bioNull, aes( x = model, y = quasi_prob_mean, col= species))+
  geom_point()+
  geom_pointrange(aes(x = model, y = quasi_prob_mean, ymin = quasi_prob_q2.5, ymax = quasi_prob_q97.5))+
  facet_wrap( ~ calculation, nrow = 3, strip.position="right")+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylim(c(0, 1))+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = "none",
        strip.background = element_rect(fill = "white"))+
  labs(col = "Species")

obj2 <- plot_grid(empty_plot, bioNull_quasi, empty_plot, rel_heights = c(0.15, 1, 0.05), ncol = 1)

plot_grid(abioBiot_quasi, obj2, rel_widths = c(5, 1))

# Save the plot
ggsave("/Volumes/GVD/Yeti/ShenSal/Figures/Quasi-extinction_AbioticBiotic_params.pdf", height = 13, width = 25)


# 7. Create a plots - Extinction --------------------------------------------------------



# Create plot
# Extinction
abioBiot_ext <- ggplot(data = abioBiot, aes( x = model, y = ext_prob_mean, col= species))+
  geom_point()+
  facet_grid( ~ Scenario.new +  Temp.new + RMI)+
  geom_pointrange(aes(x = model, y = ext_prob_mean, ymin = ext_prob_q2.5, ymax = ext_prob_q97.5))+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylab("Extinction probability in 60 years")+
  xlab("Model")+
  ylim(c(0, 1))+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.text.x = element_text(angle=45, hjust = 1),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        legend.position = "top")+
  labs(col = "Species")


bioNull_ext <- ggplot(data = bioNull, aes( x = model, y = ext_prob_mean, col= species))+
  geom_point()+
  geom_pointrange(aes(x = model, y = ext_prob_mean, ymin = ext_prob_q2.5, ymax = ext_prob_q97.5))+
  scale_color_manual(values = c("black", "deepskyblue3"))+
  ylim(c(0, 1))+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        legend.position = "none")+
  labs(col = "Species")

obj3 <- plot_grid(empty_plot, bioNull_ext, empty_plot, rel_heights = c(0.15, 1, 0.05), ncol = 1)

plot_grid(abioBiot_ext, obj3, rel_widths = c(5, 1))

# Save the plot
ggsave("/Volumes/GVD/Yeti/ShenSal/Figures/Extinction_AbioticBiotic_params.pdf", height = 13, width = 25)



# 8. Make the master table with probabilities -----------------------------

met.sub <- metrics[1:36, c("model", "species", "Scenario.new", "Temp.new", "RMI")]

# Take only shenandoah estimates
metrics <- metrics[metrics$species == "P. shenandoah",]




# Add an empty column
ext.sub <- data.frame(metrics[1:18, c("model", "Scenario.new", "Temp.new", "RMI")],
                       Metrics = "Extinction prob",
                       calculation = NA,
                       Mean = metrics[1:18, "ext_prob_mean"],
                       q2.5 = metrics[1:18, "ext_prob_q2.5"],
                       q97.5 = metrics[1:18, "ext_prob_q97.5"])

min.sub <- data.frame(metrics[1:18, c("model", "Scenario.new", "Temp.new", "RMI")],
                      Metrics = "Minimum occupancy",
                      calculation = NA,
                      Mean = metrics[1:18, "min_pop_size_mean"]/617,
                      q2.5 = metrics[1:18, "min_pop_size_q2.5"]/617,
                      q97.5 = metrics[1:18, "min_pop_size_q97.5"]/617)

quasi.sub <- data.frame(metrics[ ,c("model", "Scenario.new", "Temp.new", "RMI")],
                      Metrics = "Quasi-extinction",
                      calculation = metrics[, "calculation"],
                      Mean = metrics[, "quasi_prob_mean"],
                      q2.5 = metrics[, "quasi_prob_q2.5"],
                      q97.5 = metrics[, "quasi_prob_q97.5"])

all.metrics <- rbind(ext.sub, min.sub, quasi.sub)

write.csv(all.metrics, file = "/Volumes/GVD/Yeti/ShenSal/Tables/Summary_metrics_AbioticBiotic_params.csv")
