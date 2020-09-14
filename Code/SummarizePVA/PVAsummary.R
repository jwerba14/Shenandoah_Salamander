# This code was written by: G. V. DiRenzo
# If you have any questions, please send them to: grace.direnzo@gmail.com

# Code Objective:
  # To synthesize the PVA output

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

libLocation <- c(.libPaths(),
"/home/gdirenzo/R/x86_64-redhat-linux-gnu-library/4.0",
"/opt/ohpc/pub/usgs/libs/gnu8/R/4.0.1/lib64/R/library")

library(tidyverse, lib.loc = libLocation)


# 2. Set working directory ---------------------------------------------------------------

# set working directory
# setwd("/Volumes/GVD/lustre/")
# setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")
setwd("/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/Dropbox/USGS/ShenandoahSalamander/")


# 3. Set arguments --------------------------------------------------------


##### Set the Temp and RMI variables
args <- commandArgs(trailingOnly=TRUE)
cat("the arguments are:")
cat(args)

# temp
# args <- c("BioticAbiotic", "temp_mat_range_SCEN1", "RMI_mat_range_sub")

# Directory name
name1 <- as.character(args[1])

# If the argument is BioticAbiotic or Abiotic, we need to assign temp and RMI values
if(args[1] == "BioticAbiotic" | args[1] == "Abiotic"){
  # Temp name
  name2 <- as.character(args[2])
  
  # RMI name
  name3 <- as.character(args[3])
}

# 4. Load in data ---------------------------------------------------------


## Calculate number of files calling in

# If the argument is BioticAbiotic or Abiotic, we need to assign temp and RMI values
if(args[1] == "BioticAbiotic" | args[1] == "Abiotic"){
  
  # create the path ID
  path_ID <- paste0("./PVA/", name1, "/", name2, "_", name3, "/")
}

# If the argument is BioticAbiotic or Abiotic, we need to assign temp and RMI values
if(args[1] == "Null" | args[1] == "Biotic"){
  
  # create the path ID
  path_ID <- paste0("./PVA/", name1, "/")
}

# Determine which files to read in
nfiles.sample.02 <- list.files(path = path_ID)[grep("MCMC_._metrics_02_per", list.files(path=path_ID))]
nfiles.sample.02 <- c(nfiles.sample.02, 
                      list.files(path = path_ID)[grep("MCMC_.._metrics_02_per", list.files(path=path_ID))])
nfiles.sample.02 <- c(nfiles.sample.02, 
                      list.files(path = path_ID)[grep("MCMC_..._metrics_02_per", list.files(path=path_ID))])


nfiles.sample.05 <- list.files(path = path_ID)[grep("MCMC_._metrics_05_per", list.files(path=path_ID))]
nfiles.sample.05 <- c(nfiles.sample.05, 
                      list.files(path = path_ID)[grep("MCMC_.._metrics_05_per", list.files(path=path_ID))])
nfiles.sample.05 <- c(nfiles.sample.05, 
                      list.files(path = path_ID)[grep("MCMC_..._metrics_05_per", list.files(path=path_ID))])


nfiles.sample.10 <- list.files(path = path_ID)[grep("MCMC_._metrics_10_per", list.files(path=path_ID))]
nfiles.sample.10 <- c(nfiles.sample.10, 
                      list.files(path = path_ID)[grep("MCMC_.._metrics_10_per", list.files(path=path_ID))])
nfiles.sample.10 <- c(nfiles.sample.10, 
                      list.files(path = path_ID)[grep("MCMC_..._metrics_10_per", list.files(path=path_ID))])


nfiles.sample.pop <- list.files(path = path_ID)[grep("MCMC_._pop_size", list.files(path=path_ID))]
nfiles.sample.pop <- c(nfiles.sample.pop, 
                      list.files(path = path_ID)[grep("MCMC_.._pop_size", list.files(path=path_ID))])
nfiles.sample.pop <- c(nfiles.sample.pop, 
                      list.files(path = path_ID)[grep("MCMC_..._pop_size", list.files(path=path_ID))])


# 5. Format data -------------------------------------------------------------


# Create an empty object to hold the data
met02 <- list()
met05 <- list()
met10 <- list()
pop   <- list()

# Loop through all of the files- adding new rows the object
for(i in 1:length(nfiles.sample.02)){

  # Load in the data
  load(paste0(path_ID, "/", nfiles.sample.02[i]))
  load(paste0(path_ID, "/", nfiles.sample.05[i]))
  load(paste0(path_ID, "/", nfiles.sample.10[i]))
  load(paste0(path_ID, "/", nfiles.sample.pop[i]))
  
  # Add new rows
  met02[[i]] <- as.data.frame(metrics_02)
  met05[[i]] <- as.data.frame(metrics_05)
  met10[[i]] <- as.data.frame(metrics_10)
  pop[[i]]   <- as.data.frame(pop_size)

}

# Convert list into 
met02 <- unlist(lapply(met02, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)
met02 <- do.call(rbind, met02)

met05 <- unlist(lapply(met05, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)
met05 <- do.call(rbind, met05)

met10 <- unlist(lapply(met10, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)
met10 <- do.call(rbind, met10)

pop <- unlist(lapply(pop, function(x) if(is.data.frame(x)) list(x) else x), recursive = FALSE)
pop <- do.call(rbind, pop)

# 6. Summarise data -------------------------------------------------------------


ext_02 <- met02 %>%
  # Summarize across MCMC
  group_by(year, species) %>%
  summarise(
    # The number of simulations across MCMC iterations
    # This value can be used to divide the numbers to calculate proportions
    num_sims = sum(num_sims),
    
    # Number of times extinction happened
    # The mean number of simulations that went extinct
    ext_num_mean = mean(num_ext),
    ext_num_q2.5 = quantile(num_ext, prob = c(0.025, 0.975)[1]),
    ext_num_q97.5 = quantile(num_ext, prob = c(0.025, 0.975)[2]),
    
    # Number of times that the simulations went below the threshold
    quasi_num_mean = mean(num_quasi),
    quasi_num_q2.5 = quantile(num_quasi, prob = c(0.025, 0.975)[1]),
    quasi_num_q97.5 = quantile(num_quasi, prob = c(0.025, 0.975)[2]),
    
    # Extinction prob
    # The proportion of times that all sites were unoccupied
    ext_prob_mean = mean(ext_prob),
    ext_prob_q2.5 = quantile(ext_prob, prob = c(0.025, 0.975)[1]),
    ext_prob_q97.5 = quantile(ext_prob, prob = c(0.025, 0.975)[2]),
    
    # Quasi-extinction
    # The proportion of times that occupancy went below a threshold
    quasi_prob_mean = mean(quasi),
    quasi_prob_q2.5 = quantile(quasi, prob = c(0.025, 0.975)[1]),
    quasi_prob_q97.5 = quantile(quasi, prob = c(0.025, 0.975)[2]),
    
    # minimum population size
    min_pop_size_mean = mean(min_pop),
    min_pop_size_q2.5 = quantile(min_pop, prob = c(0.025, 0.975)[1]),
    min_pop_size_q97.5 = quantile(min_pop, prob = c(0.025, 0.975)[2])
  )

ext_05 <- met05 %>%
  # Summarize across MCMC
  group_by(year, species) %>%
  summarise(
    # The number of simulations across MCMC iterations
    # This value can be used to divide the numbers to calculate proportions
    num_sims = sum(num_sims),
    
    # Number of times extinction happened
    # The mean number of simulations that went extinct
    ext_num_mean = mean(num_ext),
    ext_num_q2.5 = quantile(num_ext, prob = c(0.025, 0.975)[1]),
    ext_num_q97.5 = quantile(num_ext, prob = c(0.025, 0.975)[2]),
    
    # Number of times that the simulations went below the threshold
    quasi_num_mean = mean(num_quasi),
    quasi_num_q2.5 = quantile(num_quasi, prob = c(0.025, 0.975)[1]),
    quasi_num_q97.5 = quantile(num_quasi, prob = c(0.025, 0.975)[2]),
    
    # Extinction prob
    # The proportion of times that all sites were unoccupied
    ext_prob_mean = mean(ext_prob),
    ext_prob_q2.5 = quantile(ext_prob, prob = c(0.025, 0.975)[1]),
    ext_prob_q97.5 = quantile(ext_prob, prob = c(0.025, 0.975)[2]),
    
    # Quasi-extinction
    # The proportion of times that occupancy went below a threshold
    quasi_prob_mean = mean(quasi),
    quasi_prob_q2.5 = quantile(quasi, prob = c(0.025, 0.975)[1]),
    quasi_prob_q97.5 = quantile(quasi, prob = c(0.025, 0.975)[2]),
    
    # minimum population size
    min_pop_size_mean = mean(min_pop),
    min_pop_size_q2.5 = quantile(min_pop, prob = c(0.025, 0.975)[1]),
    min_pop_size_q97.5 = quantile(min_pop, prob = c(0.025, 0.975)[2])
  )

ext_10 <- met10 %>%
  # Summarize across MCMC
  group_by(year, species) %>%
  summarise(
    # The number of simulations across MCMC iterations
    # This value can be used to divide the numbers to calculate proportions
    num_sims = sum(num_sims),
    
    # Number of times extinction happened
    # The mean number of simulations that went extinct
    ext_num_mean = mean(num_ext),
    ext_num_q2.5 = quantile(num_ext, prob = c(0.025, 0.975)[1]),
    ext_num_q97.5 = quantile(num_ext, prob = c(0.025, 0.975)[2]),
    
    # Number of times that the simulations went below the threshold
    quasi_num_mean = mean(num_quasi),
    quasi_num_q2.5 = quantile(num_quasi, prob = c(0.025, 0.975)[1]),
    quasi_num_q97.5 = quantile(num_quasi, prob = c(0.025, 0.975)[2]),
    
    # Extinction prob
    # The proportion of times that all sites were unoccupied
    ext_prob_mean = mean(ext_prob),
    ext_prob_q2.5 = quantile(ext_prob, prob = c(0.025, 0.975)[1]),
    ext_prob_q97.5 = quantile(ext_prob, prob = c(0.025, 0.975)[2]),
    
    # Quasi-extinction
    # The proportion of times that occupancy went below a threshold
    quasi_prob_mean = mean(quasi),
    quasi_prob_q2.5 = quantile(quasi, prob = c(0.025, 0.975)[1]),
    quasi_prob_q97.5 = quantile(quasi, prob = c(0.025, 0.975)[2]),
    
    # minimum population size
    min_pop_size_mean = mean(min_pop),
    min_pop_size_q2.5 = quantile(min_pop, prob = c(0.025, 0.975)[1]),
    min_pop_size_q97.5 = quantile(min_pop, prob = c(0.025, 0.975)[2])
  )

tot_pop <- pop %>%
  group_by(year, species) %>%
  summarise(# Total occupied sites
    # minimum population size
    pop_mean = mean(pop),
    pop_q2.5 = quantile(pop, prob = c(0.025, 0.975)[1]),
    pop_q97.5 = quantile(pop, prob = c(0.025, 0.975)[2])
  )


# 7. Save the summary -------------------------------------------------------------


# Save the results

if(name1 == "Biotic" | name1 == "Null"){
  save(tot_pop, file = paste0("./PVA_summary/", name1, "/", name1,  "_pop_size_summary.rda")) 
  
  save(ext_02, file = paste0("./PVA_summary/", name1, "/", name1, "_metrics_02_per_summary.rda")) 
  save(ext_05, file = paste0("./PVA_summary/", name1, "/", name1, "_metrics_05_per_summary.rda")) 
  save(ext_10, file = paste0("./PVA_summary/", name1, "/", name1, "_metrics_10_per_summary.rda")) 
}

if(name1 == "BioticAbiotic" | name1 == "Abiotic"){
  save(tot_pop, file = paste0("./PVA_summary/", name1 , "/", name2, "_", name3, "/", 
                               name1, "_", name2, "_", name3,"_pop_size_summary.rda")) 
  
  save(ext_02, file = paste0("./PVA_summary/", name1 , "/", name2, "_", name3, "/", 
                                 name1, "_", name2, "_", name3,"_metrics_02_per_summary.rda")) 
  save(ext_05, file = paste0("./PVA_summary/", name1 , "/", name2, "_", name3, "/", 
                                 name1, "_", name2, "_", name3,"_metrics_05_per_summary.rda")) 
  save(ext_10, file = paste0("./PVA_summary/", name1 , "/", name2, "_", name3, "/", 
                                 name1, "_", name2, "_", name3,"_metrics_10_per_summary.rda")) 
}

# End script