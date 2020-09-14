##### The following R script was written by G. V. DiRenzo
### Please send questions to: grace.direnzo@gmail.com

# Objective: 
  # To extract temp and RMI estimates for the sampled Pshen sites 
  # To extract temp and RMI estimates for the known Pshen range for different scenarios:
    # temp: scenario 1 and 2
    # RMI: +/- 2

# Load library
library(rgdal) 
library(sp)
library(sf)
library(RColorBrewer) 
library(raster)

# Set working directory
setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")

# Read in Hobo logger positions data
# This will be used to re-project other layers
hobo <- st_read("./Data/SHENgisForGrace/StephanTempleHobo_positions/Hobo_positions.shp")

# Store the projection in an object
WGS_proj <- projection(hobo)

# Read in sites for known sites
site <- read.csv("./Data/site_lat_long.csv")

# Assign coordinates to the file - turns it into a spatial point file
coordinates(site)= ~ Longitude + Latitude
# Assign the project to match the hobo loggers
proj4string(site)<- CRS(WGS_proj)

# Projection (number of years for PVA)
projection <- 60

#############################
#############################
################ Known Shenandoah range
#############################
#############################
# Read in the data
range <- st_read("./Data/KnownRange/KnownPshenRange.shp")

# Shapefile reprojection
range_WGS <- st_transform(range, crs(WGS_proj))

# Save shape file as a data frame
df_range <-as.data.frame(range_WGS)

# rasterize the shapefile
r <- raster(ncol = 100, nrow = 100)

# Change the extent to include all of the surrounding area
extent(r) <- c(-78.42, -78.31, 38.54, 38.64)

# Rasterize
rp <- rasterize(range_WGS, r)

# New projection
epgs_proj <- "+proj=lcc +lat_1=39.2 +lat_2=38.03333333333333 +lat_0=37.66666666666666 +lon_0=-78.5 +x_0=3500000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

# Change resolution to 100 x 100 m
rp2 <-projectRaster(rp,
               res = c(100, 100),
               crs = epgs_proj)

# add a 300 m buffer around the edge of the range
rp_buffer <- buffer(rp2, width = 300)

 pdf("/Volumes/GVD/Yeti/ShenSal/Figures/Shen_area.pdf", width = 8, height = 8)
 # Plot the buffer
 plot(rp_buffer, ylab = "UTM Northing", 
      xlab = "UTM Easting",
      col = "black", legend = F)
 
 # Plot the known range
 plot(rp2, add = TRUE, col = "goldenrod3", legend = F)
 scalebar(1000, type = "bar", label = "1 km", xy = c(3507000, 2097500))
 legend("topleft", c("known range", "buffer"), fill = c("goldenrod3", "black"), bty = "n")
 dev.off()

# Convert raster to SpatialPointsDataFrame
r.pts <- rasterToPoints(rp_buffer, spatial = TRUE)

# Check the projection
proj4string(r.pts)

# reproject SpatialPointsDataFrame object to match hobo
r.pts <- spTransform(r.pts, CRS(WGS_proj)) 

# Check the reprojection
proj4string(r.pts)

# Assign coordinates to @data slot
r.pts@data <- data.frame(r.pts@data, 
                         long=coordinates(r.pts)[,1],
                         lat=coordinates(r.pts)[,2])                         

#############################
#############################
################ Present temperatures for the random sites
#############################
#############################

# Set path
dpath<-"./Data/Climate data_no correction/tmax_PRES_nocorrection/w001001.adf" 

# Read in the raster
shen <- raster(dpath)

# Check projection
crs(shen)

# Simplest approach to re-project a raster
pr1 <- projectRaster(shen, crs=WGS_proj)

#plot(pr1)
#plot(site, add = TRUE)

# Re-project to another system to calculate size of grid cells
# EPSG 6592
# pr2 <- projectRaster(shen, crs="+init=epsg:6592")
# 100 x 100 m


# Extract the values for the locations in the site data frame
values <- extract(pr1, site, df = TRUE)

# Create a dataframe with the present day temperatures along with lat/long data
daf3 <- data.frame(Tmax_p = values$w001001, 
                   Longitude = site$Longitude, 
                   Latitude = site$Latitude)

# Extract values for the range of shenandoah
range_values <- extract(pr1, r.pts, df = TRUE)

# Extract present day temperatures across the entire Pshen range
range_Tmax_p <- data.frame(Tmax_p = range_values$w001001, 
                           Longitude = r.pts@data$long, 
                           Latitude = r.pts@data$lat)

#############################
#############################
################ Present RMI for the random sites
#############################
#############################

# Set path
dpath<-"./Data/TopoRelativeMoistureIndex/trmi.img" 

# Read in the raster
shen_RMI <- raster(dpath)

# Check projection
crs(shen_RMI)

# Simplest approach to re-project a raster
pr1_rmi <- projectRaster(shen_RMI, crs=WGS_proj)

#plot(pr1_rmi)
#plot(site, add = TRUE)

# Re-project to another system to calculate size of grid cells
# EPSG 6592
# pr2_rmi <- projectRaster(shen_RMI, crs="+init=epsg:6592")
# pr2_rmi

# Extract the values for the locations in the site data frame
values_rmi <- extract(pr1_rmi, site, df = TRUE)

range_RMI_p <- cbind(daf3, 
                     RMI_p = values_rmi$layer)

# Extract the values across the RANGE
range_values_RMI <- extract(pr1_rmi, r.pts, df = TRUE)

range_RMI <- data.frame(RMI = range_values_RMI$layer, 
                           Longitude = r.pts@data$long, 
                           Latitude = r.pts@data$lat)
nrow(range_RMI)

##########################
### = Linear fit    -> adding 2sd
#########################
# Create a dataframe with present day & future RMI estimates
RMI_range_add <- data.frame(
  present = range_RMI$RMI,
  future = range_RMI$RMI + (2 * sd(range_RMI$RMI))
)
# Create a dataframe with the time for present day (t = 1) and future (t = 60)
time <- data.frame(
  present = rep(1, times = nrow(RMI_range_add)),
  future = rep(projection, times = nrow(RMI_range_add)))

# Make a plot to visualize the increase
#plot(c(t(RMI_range_add)) ~ c(t(time)), 
#     ylim = c(min(RMI_range_add)-2, max(RMI_range_add)+2), 
#     las = 1, pch = 21, 
#     col = "black", 
#     bg = "deepskyblue3", 
#     xlab = "Time (Yrs)", 
#     ylab = "Relative Moisture index", 
#     main = "Linear increase")

# Create an empty matrix to hold the projections
  # dimensions = columns = years, rows = sites
RMI_mat_range_add <- matrix(NA, ncol = projection, nrow = nrow(range_RMI))
# The first columns = present day 
RMI_mat_range_add[,1] <- RMI_range_add$present

# data frame with list of years 
df <- data.frame(x = 1:projection)

for(i in 1:nrow(RMI_mat_range_add)){
  
  # Fit a model to the temperature and time
  mod <- lm(c(t(RMI_range_add[i,])) ~ c(t(time[i,])))
  
  RMI_mat_range_add[i,] <- coef(mod)[1] + df$x * coef(mod)[2]
  # plot the temperature v time 
#  lines(RMI_mat_range_add[i,] ~ df$x, lwd = 2, col = "deepskyblue3")
  
} #i

# Replace the first column with present day estimates
RMI_mat_range_add[,1] <- RMI_range_add$present
# Replace the last column with future estimates
RMI_mat_range_add[,ncol(RMI_mat_range_add)] <- RMI_range_add$future

##########################
### = Linear fit    -> subtract 2sd
#########################
# Create a dataframe with present day & future RMI estimates
RMI_range_sub <- data.frame(
  present = range_RMI$RMI,
  future = range_RMI$RMI - (2 * sd(range_RMI$RMI))
)
# Create a dataframe with the time for present day (t = 1) and future (t = 60)
time <- data.frame(
  present = rep(1, times = nrow(RMI_range_sub)),
  future = rep(projection, times = nrow(RMI_range_sub)))

# Make a plot
#plot(c(t(RMI_range_sub)) ~ c(t(time)), 
#     ylim = c(min(RMI_range_sub)-2, max(RMI_range_sub)+2), 
#     las = 1, pch = 21, col = "black", 
#     bg = "deepskyblue3",
#     xlab = "Time (Yrs)", 
#     ylab = "Relative Moisture index", 
#     main = "Linear decrease")

# Create an empty matrix to hold the projections
  # dimensions = columns = years, rows = sites
RMI_mat_range_sub <- matrix(NA, ncol = projection, nrow = nrow(range_RMI))
# The first columns = present day 
RMI_mat_range_sub[,1] <- RMI_range_sub$present

# data frame with list of years 
df <- data.frame(x = 1:projection)

# Loop through every site - fit a model and calcualte the RMI for each year from 1:60
for(i in 1:nrow(RMI_mat_range_sub)){
  
  # Fit a model to the temperature and time
  mod <- lm(c(t(RMI_range_sub[i,])) ~ c(t(time[i,])))
  
  RMI_mat_range_sub[i,] <- coef(mod)[1] + df$x * coef(mod)[2]
  # plot the temperature v time 
#  lines(RMI_mat_range_sub[i,] ~ df$x, lwd = 2, col = "deepskyblue3")
  
} #i

# Replace the first column with present day estimates
RMI_mat_range_sub[,1] <- RMI_range_sub$present
# Replace the last column with future estimates
RMI_mat_range_sub[,ncol(RMI_mat_range_sub)] <- RMI_range_sub$future


#############################
#############################
################ FUTURE temperatures
#############################
#############################

##############################
########## Scenario 1
##############################

# Set path
dpath<-"./Data/Climate data_no correction/tmax_SCEN1_nocorrection/w001001x.adf" 

shen <- raster(dpath)

# Check projection
crs(shen)

# Simplest approach to re-project
pr1 <- projectRaster(shen, crs = WGS_proj)

# Extract values for the range of shenandoah
range_temp_SCEN1 <- extract(pr1, r.pts, df = TRUE)

range_Tmax_SCEN1 <- data.frame(Tmax_f = range_temp_SCEN1$w001001x, 
                               Longitude = r.pts@data$long, 
                               Latitude = r.pts@data$lat)

##############################
########## Scenario 2
##############################

# Set path
dpath<-"./Data/Climate data_no correction/tmax_SCEN2_nocorrection/w001001x.adf" 

shen2 <- raster(dpath)

# Check projection
crs(shen2)

# Simplest approach to re-project
pr1_2 <- projectRaster(shen2, crs=WGS_proj)

# Extract values for the range of shenandoah
range_temp_SCEN2 <- extract(pr1_2, r.pts, df = TRUE)

range_Tmax_SCEN2 <- data.frame(Tmax_f = range_temp_SCEN2$w001001x, 
                            Longitude = r.pts@data$long, 
                            Latitude = r.pts@data$lat)

#############################
#############################
################ Calculate future temp within the Pshen range for 60 years - scen1
#############################
#############################

##################
### Assuming mean temperature increases by 3.3C over 60 yrs

temp_range_SCEN1 <- data.frame(
  present = range_Tmax_p$Tmax_p, 
  future = range_Tmax_SCEN1$Tmax_f)

time <- data.frame(
  present = rep(1, times = nrow(temp_range_SCEN1)),
  future = rep(projection, times = nrow(temp_range_SCEN1)))

#######################################
################## SCENARIO 1
#######################################

### = Linear fit
# Create an empty matrix to fill in
  # dimensions = sites within range x number of years
temp_mat_range_SCEN1 <- matrix(NA, 
                               ncol = projection, 
                               nrow = nrow(temp_range_SCEN1))
# First year = present day temps
temp_mat_range_SCEN1[,1] <- temp_range_SCEN1$present

#plot(c(t(temp_range_SCEN1)) ~ c(t(time)), 
#     ylim = c(20, max(temp_range_SCEN1)+2), 
#     las = 1, pch = 21, col = "black", 
#     bg = "deepskyblue3", 
#     xlab = "Time (Yrs)", 
#     ylab = "Temp C", 
#     main = "Linear increase")
#
for(i in 1:nrow(temp_mat_range_SCEN1)){
  
  # Fit a model to the temperature and time
  mod <- lm(c(t(temp_range_SCEN1[i,])) ~ c(t(time[i,])))
  
  temp_mat_range_SCEN1[i,] <- coef(mod)[1] + df$x * coef(mod)[2]
  # plot the temperature v time 
 # lines(temp_mat_range_SCEN1[i,] ~ df$x, lwd = 2, col = "deepskyblue3")
  
} #i

# Replace the first column with present day estimates
temp_mat_range_SCEN1[,1] <- temp_range_SCEN1$present

# Replace the last column with future estimates
temp_mat_range_SCEN1[,ncol(temp_mat_range_SCEN1)] <- temp_range_SCEN1$future

### = Exponenatial fit
# Create an empty matrix to fill in
  # dimensions = sites within range x number of years
temp_mat_exp_range_SCEN1 <- matrix(NA, 
                                   ncol = projection, 
                                   nrow = nrow(temp_range_SCEN1))
# First year = present day temps
temp_mat_exp_range_SCEN1[,1] <- temp_range_SCEN1$present

# Formatting data to be used by nls
y <- c(t(temp_range_SCEN1[1,]), 
       rep(temp_range_SCEN1[1,1], times = 4))
x <- c(t(time[1,]), 2:5)
dat <- data.frame(y = y, x = x)

# fit non-linear model
mod <- nls(y ~ a + b^x, data = dat, start = list(a = dat$y[1], b = 1))
pred <- predict(mod, list(x = df$x))

## add fitted curve
#plot(c(t(temp_range_SCEN1)) ~ c(t(time)), 
#     ylim = c(20, max(temp_range_SCEN1)+2), 
#     las = 1, pch = 21, col = "black", 
#     bg = "deepskyblue3", 
#     xlab = "Time (Yrs)", 
#     ylab = "Temp C", 
#     main = "Exponential increase")
#lines(df$x, pred, col = "deepskyblue3", lwd = 3)

for(i in 1:nrow(temp_mat_exp_range_SCEN1)){
  
  y <- c(t(temp_range_SCEN1[i,]), 
         rep(temp_range_SCEN1[i,1], times = 4))
  x <- c(t(time[i,]), 2:5)
  dat <- data.frame(y = y, x = x)
  mod <- nls(y ~ a + b^x, data = dat, start = list(a = dat$y[1], b = 1))
  temp_mat_exp_range_SCEN1[i,] <- predict(mod, list(x = df$x))
  
  #lines(temp_mat_exp_range_SCEN1[i,] ~ df$x, lwd = 2, col = "deepskyblue3")
  
} #i

# Replace the first column with present day estimates
temp_mat_exp_range_SCEN1[,1] <- temp_range_SCEN1$present
# Replace the last column with future estimates
temp_mat_exp_range_SCEN1[,ncol(temp_mat_exp_range_SCEN1)] <- temp_range_SCEN1$future


#############################
#############################
################ Calculate future temp within the Pshen range for 60 years- scen2
#############################
#############################

##################
### Assuming mean temperature increases by 6.0C over 60 yrs

temp_range_SCEN2 <- data.frame(
  present = range_Tmax_p$Tmax_p, 
  future = range_Tmax_SCEN2$Tmax_f)

time <- data.frame(
  present = rep(1, times = nrow(temp_range_SCEN2)),
  future = rep(projection, times = nrow(temp_range_SCEN2)))

#######################################
################## SCENARIO 2
#######################################

### = Linear fit
# Create an empty matrix to fill in
  # dimensions = sites within range x number of years
temp_mat_range_SCEN2 <- matrix(NA, 
                               ncol = projection, 
                               nrow = nrow(temp_range_SCEN2))

# First year = present day temps
temp_mat_range_SCEN2[,1] <- temp_range_SCEN2$present

## Plot it to make sure it looks good
#plot(c(t(temp_range_SCEN2)) ~ c(t(time)), 
#     ylim = c(20, max(temp_range_SCEN2)+2), 
#     las = 1, pch = 21, col = "black", 
#     bg = "deepskyblue3", 
#     xlab = "Time (Yrs)", 
#     ylab = "Temp C", 
#     main = "Linear increase")

# Loop through each site
for(i in 1:nrow(temp_mat_range_SCEN2)){
  
  # Fit a model to the temperature and time
  mod <- lm(c(t(temp_range_SCEN2[i,])) ~ c(t(time[i,])))
  
  temp_mat_range_SCEN2[i,] <- coef(mod)[1] + df$x * coef(mod)[2]
  # plot the temperature v time 
 # lines(temp_mat_range_SCEN2[i,] ~ df$x, lwd = 2, col = "deepskyblue3")
  
} #i

# Replace the first column with present day estimates
temp_mat_range_SCEN2[,1] <- temp_range_SCEN2$present

# Replace the last column with future estimates
temp_mat_range_SCEN2[,ncol(temp_mat_range_SCEN2)] <- temp_range_SCEN2$future


######
### = Exponenatial fit
# Create an empty matrix to fill in
  # dimensions = sites within range x number of years
temp_mat_exp_range_SCEN2 <- matrix(NA, 
                                   ncol = projection, 
                                   nrow = nrow(temp_range_SCEN2))

# First year = present day temps
temp_mat_exp_range_SCEN2[,1] <- temp_range_SCEN2$present

y <- c(t(temp_range_SCEN2[1,]), rep(temp_range_SCEN2[1,1], times = 4))
x <- c(t(time[1,]), 2:5)
dat <- data.frame(y = y, x = x)

# fit non-linear model
mod <- nls(y ~ a + b^x, data = dat, start = list(a = dat$y[1], b = 1))
pred <- predict(mod, list(x = df$x))

## add fitted curve
#plot(c(t(temp_range_SCEN2)) ~ c(t(time)),
#     ylim = c(20, max(temp_range_SCEN2)+2), 
#     las = 1, pch = 21, col = "black",
#     bg = "deepskyblue3", 
#     xlab = "Time (Yrs)", 
#     ylab = "Temp C", 
#     main = "Exponential increase")
#lines(df$x, pred, col = "deepskyblue3", lwd = 3)

# Loop through each site
for(i in 1:nrow(temp_mat_exp_range_SCEN2)){
  
  y <- c(t(temp_range_SCEN2[i,]), rep(temp_range_SCEN2[i,1], times = 4))
  x <- c(t(time[i,]), 2:5)
  dat <- data.frame(y = y, x = x)
  mod <- nls(y ~ a + b^x, data = dat, start = list(a = dat$y[1], b = 1))
  temp_mat_exp_range_SCEN2[i,] <- predict(mod, list(x = df$x))
  
 # lines(temp_mat_exp_range_SCEN2[i,] ~ df$x, lwd = 2, col = "deepskyblue3")
  
} #i

# Replace the first column with present day estimates
temp_mat_exp_range_SCEN2[,1] <- temp_range_SCEN2$present

# Replace the last column with future estimates
temp_mat_exp_range_SCEN2[,ncol(temp_mat_exp_range_SCEN2)] <- temp_range_SCEN2$future

#############################
#############################
################ Data files to use and summary
#############################
#############################

# Present day temperatures:
  # Observed sites: daf3
    nrow(daf3)
  # Pshen range: range_Tmax_p
    nrow(range_Tmax_p)

# Present day RMI- 
  # Observed sites: range_RMI_p
    nrow(range_RMI_p)
  # Pshen range:
    nrow(range_RMI)
    
# Future RMI - year by year
    # Pshen range:
      # + 2 sd: RMI_mat_range_add
        nrow(RMI_mat_range_add)
      # - 2 sd: 
        nrow(RMI_mat_range_sub)
        
# Future: year-by-year 
  # Scenario 1
     # Pshen range:
  # Linear
    # temp_mat_range_SCEN1
      nrow(temp_mat_range_SCEN1)
  # Exponential
    # temp_mat_exp_range_SCEN1
      nrow(temp_mat_exp_range_SCEN1)

  # Scenario 2
    # Pshen range:
  # Linear
     # temp_mat_range_SCEN2
      nrow(temp_mat_range_SCEN2)
  # Exponential
    # temp_mat_exp_range_SCEN2
      nrow(temp_mat_exp_range_SCEN2)

#### Bundle the data

dat <- list(
  site.temp = daf3,
  site.RMI = range_RMI_p,
  range.temp = range_Tmax_p,
  range.rmi = range_RMI,
  
  # 
  RMI_mat_range_add = RMI_mat_range_add,
  RMI_mat_range_sub = RMI_mat_range_sub,
  
  temp_mat_range_SCEN1 = temp_mat_range_SCEN1,
  temp_mat_exp_range_SCEN1 = temp_mat_exp_range_SCEN1,
  
  temp_mat_range_SCEN2 = temp_mat_range_SCEN2,
  temp_mat_exp_range_SCEN2 = temp_mat_exp_range_SCEN2
  
)

save(dat, file = "./Data/temp_rmi.rda")