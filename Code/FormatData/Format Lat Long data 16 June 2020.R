##### The following R script was written by G. V. DiRenzo
### Please send questions to: grace.direnzo@gmail.com

# Objective: To create a list of lat/long for the sites

## Load libraries
library(tidyverse)

## Set working directory
setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")

# Read in site names for the presence/absence data
site.names <- data.frame(GPSID = read.csv("./Data/site_names.csv")[,-1])

# Read in the lat/long
eleven_2_16_data <- read.csv("./Data/SiteLatLong.csv")
# Replace the "PSH1T" with "PSHIT"
eleven_2_16_data$GPSID <- factor(eleven_2_16_data$GPSID, 
                                 levels = c(levels(eleven_2_16_data$GPSID), "PSHIT"))
eleven_2_16_data[which(eleven_2_16_data$GPSID == "PSH1T"),]$GPSID <- "PSHIT"
eleven_2_16_data <- droplevels(eleven_2_16_data)

oh_7_to_11_data <- read.csv("./Data/KnownRange_LatLong.csv")
# All Longitude has to be -
oh_7_to_11_data$Longitude <- -oh_7_to_11_data$Longitude
# Adjust site names
oh_7_to_11_data$PS.site <- paste0("PS", as.character(unique(oh_7_to_11_data$Site)))
oh_7_to_11_data[which(oh_7_to_11_data$PS.site == "PSH1T"),]$PS.site <- "PSHIT"
oh_7_to_11_data <- droplevels(oh_7_to_11_data)


# Create 1 master file with all lat/long
# First, need to remove sites that are double counted
matches <- which(eleven_2_16_data$GPSID %in% oh_7_to_11_data$PS.site)

# Remove the sites that match
eleven_2_16_data <- eleven_2_16_data[-matches,]

# Bring together the two objects with lat/long data
oh_7_to_11_data <- data.frame(GPSID = as.character(oh_7_to_11_data$PS.site),
                              Latitude = oh_7_to_11_data$Latitude,
                              Longitude = oh_7_to_11_data$Longitude)

lat_long <- rbind(oh_7_to_11_data, eleven_2_16_data)

# Join the original order of sites
x <- inner_join(site.names, lat_long, by = "GPSID")

# Confirm that the sites are in the correct order
cbind(site.names, x)

# Data inputted wrong - not within park range
x[15, "Latitude"] <- 38.55100
x[51, "Latitude"] <- 38.63100
x[51, "Longitude"] <- -78.3640

# Write the csv
write.csv(x, file = "./Data/site_lat_long.csv")
