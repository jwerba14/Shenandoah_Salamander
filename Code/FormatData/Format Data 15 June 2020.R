##### The following R script was written by G. V. DiRenzo
### Please send questions to: grace.direnzo@gmail.com

### Code objective: To format data for 2 species of salamander in the following format:
  ### sites x replicate survey x year

## The product is 1 .rda file with the formatted data


###########################################
###########  Table of contents #######
###########################################

# 1. Load libraries & Set working directory
# 2. Read in data
# 3. Clean up data a little
# 4. Format Pshen data 
    # 4a. Determine what the output array will look like
    # 4b. Dataset #1 - Seperate the data by year 
    # 4c. Dataset #2 - Convert from long to wide format 
    # 4d. Dataset #2 - Seperate the data by year 
    # 4e. Add data from non-overlapping sites in 2011 
    # 4f. Fill in dat with the rest of the data
    # 4g. Save the array as formatted data 
# 5. Survey information 
# 6. Format Pcin data 
    # 6a. Determine what the output array will look like 
    # 6b. Dataset #1 - Seperate the data by year
    # 6c. Dataset #2 - Convert from long to wide format 
    # 6d. Dataset #2 - Seperate the data by year 
    # 6e. Add data from non-overlapping sites in 2011 
    # 6f. Fill in dat with the rest of the data
# 7. Rename the file 
# 8. Export the formatted data



###########################################
###########################################
###########################################



# 1. Load libraries & Set working directory -------------------------------------------------------



# Load libraries
library(reshape2)



# Set working directory
# setwd("/Volumes/GVD/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")
setwd("F:/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/")



# 2. Read in data-------------------------------------------------------


# Read in data from 2011 to 2016
# Survey data
surv <- read.csv("./Data/VisitData.csv")

# Read in data from 2007 to 2011
# Shenandoah salamander
dat_shen <- read.csv("./Data/KnownRangeData_Shen.csv")

# Red-back salamander
dat_cin <- read.csv("./Data/KnownRangeData_Cin.csv")



# 3. Clean up data a little-------------------------------------------------------





# Here, we will change some column names, remove some transects that aren't going to be used, and make sure that the site names match between files
# But first, look at the structure of the data
str(surv)
  #'data.frame':	1495 obs. of  19 variables:
  #  $ ID        : int  130 131 132 133 1608 1609 1664 1665 2111 2112 ...
  #  $ Site      : Factor w/ 132 levels "PS1522","PS1932",..: 1 1 1 1 1 1 1 1 1 1 ...
  #  $ Transect  : Factor w/ 6 levels "A","B","C","D",..: 1 2 1 2 1 2 1 2 1 1 ...
  #  $ Date      : Factor w/ 81 levels "4/15/13","4/16/13",..: 35 35 57 57 32 32 49 49 3 26 ...
  #  $ Observer  : Factor w/ 106 levels "A Brand","A Brand/A Wiewel",..: 1 8 75 8 38 1 38 45 22 58 ...
  #  $ Temp      : num  11.4 11.4 26.2 26.2 18.4 18.4 19.9 19.9 20.7 18.1 ...
  #  $ Sky       : int  0 0 1 1 1 1 0 0 2 5 ...
  #  $ Wind      : int  0 0 1 1 0 0 0 0 2 1 ...
  #  $ Start     : int  849 846 957 957 1120 1124 824 821 1002 1013 ...
  #  $ End       : int  854 851 1007 1002 1125 1129 828 826 1011 1019 ...
  #  $ Cover     : int  27 35 41 43 22 16 28 22 52 57 ...
  #  $ Pshen.str : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ Pshen.lead: int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ Pcin.str  : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ Pcin.lead : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ Pcyl      : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ Entered.By: Factor w/ 5 levels "","AED","CJN",..: 1 1 1 1 1 1 1 1 1 1 ...
  #  $ Checked.By: Factor w/ 4 levels "","AED","STW",..: 1 1 1 1 1 1 1 1 3 3 ...
  #  $ Notes     : Factor w/ 374 levels ""," ","~15m downhill from trail",..: 1 1 1 1 1 1 1 1 1 1 ...


str(dat_shen)
  #'data.frame':	124 obs. of  142 variables:
  #  $ Site       : Factor w/ 124 levels "3601","3602",..: 1 2 3 4 5 6 7 8 9 10 ...
  #  $ X07Spring1 : int  NA NA NA NA NA NA NA NA NA NA ...
  #  $ X07Spring2 : int  NA NA NA NA NA NA NA NA NA NA ...
  #  $ X07Spring3 : int  NA NA NA NA NA NA NA NA NA NA ...
  #  $ X08Spring1 : int  0 0 0 0 0 0 1 0 0 0 ...
  #  $ X08Spring2 : int  0 0 0 0 0 0 NA 0 0 0 ...
  #  $ X08Spring3 : int  0 0 0 0 0 0 NA 0 0 0 ...
  #  $ X08Summer1 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X08Summer2 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X08Summer3 : int  0 0 0 0 0 0 0 0 0 NA ...
  #  $ X08Fall1   : int  0 0 0 0 0 0 0 0 1 0 ...
  #  $ X08Fall2   : int  0 0 0 0 0 0 0 0 NA 0 ...
  #  $ X08Fall3   : int  NA NA NA NA NA NA 0 0 NA NA ...
  #  $ X09Spring1 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Spring2 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Spring3 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Summer1 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Summer2 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Summer3 : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Fall1   : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Fall2   : int  0 0 0 0 0 0 0 0 0 0 ...
  #  $ X09Fall3   : logi  NA NA NA NA NA NA ...
  #  $ X10Spring1 : logi  NA NA NA NA NA NA ...
  #  $ X10Spring2 : logi  NA NA NA NA NA NA ...
  #  $ X10Spring3 : logi  NA NA NA NA NA NA ...
  #  $ X10Sum1    : logi  NA NA NA NA NA NA ...
  #  $ X10Sum2    : logi  NA NA NA NA NA NA ...
  #  $ X10Sum3    : logi  NA NA NA NA NA NA ...
  #  $ X10Fall1   : logi  NA NA NA NA NA NA ...
  #  $ X10Fall2   : logi  NA NA NA NA NA NA ...
  #  $ X10Fall3   : logi  NA NA NA NA NA NA ...
  #  $ X11Spring1 : logi  NA NA NA NA NA NA ...
  #  $ X11Spring2 : logi  NA NA NA NA NA NA ...
  #  $ X11Spring3 : logi  NA NA NA NA NA NA ...
  #  $ X11Sum1    : int  0 NA NA NA NA NA NA 0 0 NA ...
  #  $ X11Sum2    : int  0 NA NA NA NA NA NA 0 0 NA ...
  #  $ X11Sum3    : logi  NA NA NA NA NA NA ...
  #  $ X11Fall1   : logi  NA NA NA NA NA NA ...
  #  $ X11Fall2   : logi  NA NA NA NA NA NA ...
  #  $ X11Fall3   : logi  NA NA NA NA NA NA ...
#.....


str(dat_cin)
  #'data.frame':	124 obs. of  142 variables:
  #  $ Site       : Factor w/ 124 levels "3601","3602",..: 1 2 3 4 5 6 7 8 9 10 ...
  #  $ X07Spring1 : int  NA NA NA NA NA NA NA NA NA NA ...
  #  $ X07Spring2 : int  NA NA NA NA NA NA NA NA NA NA ...
  #  $ X07Spring3 : int  NA NA NA NA NA NA NA NA NA NA ...
  #  $ X08Spring1 : int  0 0 1 0 1 1 1 1 0 1 ...
  #  $ X08Spring2 : int  0 1 1 0 1 1 NA 0 0 1 ...
  #  $ X08Spring3 : int  0 0 1 0 0 0 NA 1 1 1 ...
  #  $ X08Summer1 : int  0 0 1 0 1 0 0 1 1 1 ...
  #  $ X08Summer2 : int  0 0 0 0 0 1 0 1 1 1 ...
  #  $ X08Summer3 : int  0 0 1 0 0 0 1 1 1 NA ...
  #  $ X08Fall1   : int  1 1 1 0 1 1 1 1 1 1 ...
  #  $ X08Fall2   : int  1 0 1 0 1 0 1 1 NA 1 ...
  #  $ X08Fall3   : int  NA NA NA NA NA NA 1 0 NA NA ...
  #  $ X09Spring1 : int  0 1 1 1 1 0 1 0 1 1 ...
  #  $ X09Spring2 : int  1 0 0 0 0 0 1 0 1 1 ...
  #  $ X09Spring3 : int  1 1 1 0 0 0 1 0 1 1 ...
  #  $ X09Summer1 : int  0 0 0 0 0 0 0 0 0 1 ...
  #  $ X09Summer2 : int  0 0 0 0 0 0 0 1 0 1 ...
  #  $ X09Summer3 : int  1 0 0 0 0 0 0 0 0 1 ...
  #  $ X09Fall1   : int  1 1 1 1 1 1 1 1 1 1 ...
  #  $ X09Fall2   : int  1 0 0 0 1 1 0 1 1 1 ...
  #  $ X09Fall3   : logi  NA NA NA NA NA NA ...
  #  $ X10Spring1 : logi  NA NA NA NA NA NA ...
  #  $ X10Spring2 : logi  NA NA NA NA NA NA ...
  #  $ X10Spring3 : logi  NA NA NA NA NA NA ...
  #  $ X10Sum1    : logi  NA NA NA NA NA NA ...
  #  $ X10Sum2    : logi  NA NA NA NA NA NA ...
  #  $ X10Sum3    : logi  NA NA NA NA NA NA ...
  #  $ X10Fall1   : logi  NA NA NA NA NA NA ...
  #  $ X10Fall2   : logi  NA NA NA NA NA NA ...
  #  $ X10Fall3   : logi  NA NA NA NA NA NA ...
  #  $ X11Spring1 : logi  NA NA NA NA NA NA ...
  #  $ X11Spring2 : logi  NA NA NA NA NA NA ...
  #  $ X11Spring3 : logi  NA NA NA NA NA NA ...
  #  $ X11Sum1    : int  0 NA NA NA NA NA NA 1 0 NA ...
  #  $ X11Sum2    : int  1 NA NA NA NA NA NA 1 0 NA ...
  #  $ X11Sum3    : logi  NA NA NA NA NA NA ...
  #  $ X11Fall1   : logi  NA NA NA NA NA NA ...
  #  $ X11Fall2   : logi  NA NA NA NA NA NA ...
  #  $ X11Fall3   : logi  NA NA NA NA NA NA ...
  #......


# Replace the names of the 2nd column
colnames(surv)[2] <- "GPSID"

# Remove transects C, D, E, F
  # Just a handful of sites
surv <- surv[-grep("[C-F]", surv$Transect), ]

# Replace the "PSH1T" with "PSHIT"
surv[which(surv$GPSID == "PSH1T"),]$GPSID <- "PSHIT"

# Drop unused factor levels
surv <- droplevels(surv)

# this should be 0
which(surv$GPSID == "PSH1T")

# Create new site variable with the PS format to match the site names in surv
dat_shen$PS.Site <- paste0("PS", as.character(unique(dat_shen$Site)))
dat_cin$PS.Site <- paste0("PS", as.character(unique(dat_cin$Site)))

# Replace the "PSH1T" with "PSHIT"
dat_shen[which(dat_shen$PS.Site == "PSH1T"),]$PS.Site <- "PSHIT"

# Drop unused factor levels
dat_shen <- droplevels(dat_shen)

# this should be 0
which(dat_shen$PS.Site == "PSH1T")

# Replace the "PSH1T" with "PSHIT"
dat_cin[which(dat_cin$PS.Site == "PSH1T"),]$PS.Site <- "PSHIT"

# Drop unused factor levels
dat_cin <- droplevels(dat_cin)

# This should be 0
which(dat_cin$PS.Site == "PSH1T")

# Which sites do not match in the 2 files
  # This is be 0 (i.e., all sites match)
which(!dat_cin$PS.Site %in% dat_shen$PS.Site)



# 4. Notes on the datasets -------------------------------------------------------



######## ***** Beaware
  # 2011 overlaps in the 2 datasets
     # Some sites are repeated in the 2 datasets for this year
     # I am using all of the sites in dat_shen
     # And then looking at which sites do not match between dat_shen and shen
     # And then, those surveys are added to the master data file
  # Surveys from 2007 - 2011 occur in spring, summer, fall
  # Surveys from 2011 - 2016 occur from April to September
  # 2 transects are counted as independent (repeat) surveys at a site- starting in 2011




# 4. Format Pshen data -------------------------------------------------------




# 4a. Determine what the output array will look like -------------------------------------------------------



# Determine all of the unique sites that were visited across all the datasets
unique.sites <- unique(c(as.character(unique(dat_shen$PS.Site)),
                         as.character(unique(surv$GPSID))))

# The number of unique sites
length(unique.sites)

# Determine the number of unique sites
n.sites <- length(unique.sites)

# The number of years with surveys
  # first survey = 2007
  # last survey = 2016
n.years <- 2016 - 2006

# Maximum number of surveys per year
n.survey <- 14

# Create an empty array to fill in- master file
dat <- array(NA, dim = c(n.sites, n.survey, n.years))

# Fill in rownames
rownames(dat) <- unique.sites

# Fill in years
dimnames(dat)[[3]] <- 2007:2016



# 4b. Dataset #1 - Seperate the data by year -------------------------------------------------------


# Seperate the data into years
# Shenandoah
oh_seven <- as.matrix(dat_shen[,grep("07", colnames(dat_shen))])
oh_eight <- as.matrix(dat_shen[,grep("08", colnames(dat_shen))])
oh_nine  <- as.matrix(dat_shen[,grep("09", colnames(dat_shen))])
ten      <- as.matrix(dat_shen[,grep("10", colnames(dat_shen))])
eleven   <- as.matrix(dat_shen[,grep("11", colnames(dat_shen))])



# Now, we want to fill in the object dat (at the appropriate rows) with the data

# Loop across each site
for(i in 1:nrow(dat_shen)){
  
 # Determine which row in dat matches the row in dat_shen
 row.match.dat <-  match(dat_shen$PS.Site[i], rownames(dat))
  
 dat[row.match.dat, 1:ncol(oh_seven), "2007"] <- as.matrix(oh_seven[i,])
 dat[row.match.dat, 1:ncol(oh_eight), "2008"] <- as.matrix(oh_eight[i,])
 dat[row.match.dat, 1:ncol(oh_nine), "2009"]  <- as.matrix(oh_nine[i,])
 dat[row.match.dat, 1:ncol(ten), "2010"]      <- as.matrix(ten[i,])
 # Note that we will check to see if there are any overlapping sites surveyed in the 2 datafiles for 2011 later
 dat[row.match.dat, 1:ncol(eleven), "2011"]      <- as.matrix(eleven[i,])
 
}




# 4c. Dataset #2 - Convert from long to wide format -------------------------------------------------------



# Now looking at the second dataset: surv
# We first need to convert from long to wide format
# We will extract the information we need, and fill in the 3D array

# Need to sum across the columns with detections of the 2 morphs
sal.col <- apply(cbind(surv$Pshen.str, surv$Pshen.lead), 1, FUN = sum, na.rm = TRUE)

# Create a new data frame  with just the variables you need
sal <- data.frame(GPSID = surv$GPSID, 
                  Transect = surv$Transect,
                  Date = surv$Date, 
                  sal.col = sal.col)

# Format the date
sal$Date <- as.Date(sal$Date, format = "%m/%d/%y")

# Melt the data- turn it into long format
sal <- melt(sal, id = c("GPSID", "Transect", "Date", "sal.col"))

# Turn it into wide format
  # with sites along the rows and Date/Transects as the columns
sal <- dcast(sal, GPSID ~ Date + Transect, fun.aggregate = mean)

# save the site names column
site.names <- sal[,1]

# remove first column
sal <- sal[,-1]

# Anything with > 0 turns into a 1
sal[sal > 0] <- 1



# 4d. Dataset #2 - Seperate the data by year -------------------------------------------------------



# Note you may not see any data because of the large number of NAs
eleven2   <- sal[, grep("2011", colnames(sal))]
twelve    <- sal[, grep("2012", colnames(sal))]
thirteen  <- sal[, grep("2013", colnames(sal))]
fourteen  <- sal[, grep("2014", colnames(sal))]
# No data for 2015
sixteen   <- sal[, grep("2016", colnames(sal))]

# Calculate the maximum number of surveys
max.surv <- max(c(dim(eleven2)[2], 
                  dim(twelve)[2], 
                  dim(thirteen)[2], 
                  dim(fourteen)[2], 
                  dim(sixteen)[2] ))

# Move the data over to remove all the NAs
eleven2.1 <- twelve.1 <- thirteen.1 <- fourteen.1 <- sixteen.1 <- array(NA, dim = c(dim(sal)[1], max.surv))

for(i in 1:dim(sal)[1]){
  a <- b <- d <- e <- f <- 1
  for(j in 1:dim(eleven2)[2]){
    if(is.na(eleven2[i, j]) == F){eleven2.1[i, a] <- eleven2[i, j];
    a <- a + 1}
  }

  for(j in 1:dim(twelve)[2]){
      if(is.na(twelve[i, j]) == F){twelve.1[i, b] <- twelve[i, j];
      b <- b + 1}
    }

  for(j in 1:dim(thirteen)[2]){
      if(is.na(thirteen[i, j]) == F){thirteen.1[i, d] <- thirteen[i, j];
      d <- d + 1}
  }
  
  for(j in 1:dim(fourteen)[2]){
      if(is.na(fourteen[i, j]) == F){fourteen.1[i, e] <- fourteen[i, j];
      e <- e + 1}
  }
   
  for(j in 1:dim(sixteen)[2]){   
      if(is.na(sixteen[i, j]) == F){sixteen.1[i, f] <- sixteen[i, j];
      f <- f + 1}
  }
}	

# Calculate the true number of max surveys per site
n.surv.max <- max(c(length(which(apply(eleven2.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(twelve.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(thirteen.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(fourteen.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(sixteen.1, 2, max, na.rm = TRUE) > -1))))



# 4e. Add data from non-overlapping sites in 2011 -------------------------------------------------------




# Because I don't know the dates that were surveyed in the first dataset (dat_shen), but I know the sites that were surveyed, I will look to see what sites were not surveyed in both of datasets
dont_match <- as.character(rownames(dat)[which(!rownames(dat) %in% dat_shen$PS.Site)])

# Now fill in the 2011 sheet with the data from the rows that do not match between the file with the 2007-2011 data and the file with 2011-2016 data
for(i in 1:length(dont_match)){
  
  # Determine which row in dat matches the dont_match row
  row.match.dat <-  match(dont_match[i], rownames(dat))
  
  # Determine which row in eleven2.1 matches the dont_match row
  row.match.eleven <-  match(dont_match[i], site.names)
  
  # Fill in the big dataset
  dat[row.match.dat, 1:n.surv.max, "2011"] <- as.matrix(eleven2.1[row.match.eleven, 1:n.surv.max])
}



# 4f. Fill in dat with the rest of the data -------------------------------------------------------



# Now you can fill in the other years
for(i in 1:length(site.names)){
  
  # Determine which row in dat matches the row in dat_shen
  row.match.dat <-  match(site.names[i], rownames(dat))
  
  dat[row.match.dat, 1:n.surv.max, "2012"] <- as.matrix(twelve.1[i,1:n.surv.max])
  dat[row.match.dat, 1:n.surv.max, "2013"] <- as.matrix(thirteen.1[i,1:n.surv.max])
  dat[row.match.dat, 1:n.surv.max, "2014"] <- as.matrix(fourteen.1[i,1:n.surv.max])
    # No data = 2015
  dat[row.match.dat, 1:n.surv.max, "2016"] <- as.matrix(sixteen.1[i,1:n.surv.max])

}




# 4g. Save the array as formatted data -------------------------------------------------------


# Save array with shenandoah name
SHEN_df <- dat

# Site names
write.csv(unique.sites, file = "./Data/site_names.csv")




# 5. Survey information -------------------------------------------------------



# Determine the number of times each transect was surveyed each year
dat1 <- dat

# Replace all 0's with 1s
dat1[dat1 == 0] <- 1

# Calculate the number of surveys per site
n_surv_per_site <- apply(dat1, c(1, 3), sum, na.rm = TRUE)

# Make a document with the total number of surveys per site ever
write.csv(n_surv_per_site, file = "./Data/n_surv_per_site_true.csv")

# Now, for the model, we want the last column for each site and year that a survey was conducted
# Check out the 7th row in the 2nd sheet for an example
# Replace NA values in between entries with a 1
for(i in 1:nrow(dat1)){
  for(k in 1:dim(dat1)[3]){
    # Determine which columns have an entry
    dat.vals <- which(is.na(dat1[i, , k]) == FALSE)
    
    # If the whole row is not all NAs then do this:
    if(length(dat.vals) > 0){
      # save the value of the last column
      dat.vals.max <- max(dat.vals)
      
      # Then, create a full sequence from 1 to max number of surveys
      all.cols <- seq(from = 1, to = dat.vals.max, by = 1)
      
      # Determine if which values do not match between the 2 sequences
      dont_match <- which(!(all.cols %in%  dat.vals))
      
      # If there is > 0 values that do not match, then replace them with a 1
      if(length(dont_match) > 1){
        dat1[i, dont_match, k] <- 1
      }
    }
  }
}

# Calculate the number of surveys per site
n_surv_per_site_mod <- apply(dat1, c(1, 3), sum, na.rm = TRUE)

# Make a document with the total number of surveys per site ever
write.csv(n_surv_per_site_mod, file = "./Data/n_surv_per_site_mod.csv")



# Calculate the total number of sites surveyed per year
n_sites_per_year <- apply(dat1, c(1, 3), sum, na.rm = TRUE)
n_sites_per_year[n_sites_per_year > 0] <- 1
n_sites_per_year <- apply(n_sites_per_year, 2, sum)

# Make a document with the total number of unique sites surveyed per year
write.csv(n_sites_per_year, file = "./Data/n_sites_per_year.csv")





# 6. Format Pcin data -------------------------------------------------------




###########################################
###########  Red-back salamander ########
###########################################



# 6a. Determine what the output array will look like -------------------------------------------------------



# Determine all of the unique sites that were visited
unique.sites.cin <- unique(c(unique(dat_cin$PS.Site),
                            as.character(unique(surv$GPSID))))

# Do the shen and cin sites match?
which(unique.sites.cin %in% unique.sites == FALSE)
  # > yes

# Create an empty array to fill in
# Created with the same dimensions as the Shen array
dat.cin <- array(NA, dim = c(n.sites, n.survey, n.years))

# Fill in rownames
rownames(dat.cin) <- unique.sites.cin

# Fill in years
dimnames(dat.cin)[[3]] <- 2007:2016



# 6b. Dataset #1 - Seperate the data by year -------------------------------------------------------



# Now looking at the first dataset: dat_cin

# Seperate the data into years
# Shenandoah
oh_seven <- as.matrix(dat_cin[,grep("07", colnames(dat_cin))])
oh_eight <- as.matrix(dat_cin[,grep("08", colnames(dat_cin))])
oh_nine <- as.matrix(dat_cin[,grep("09", colnames(dat_cin))])
ten <- as.matrix(dat_cin[,grep("10", colnames(dat_cin))])
eleven <- as.matrix(dat_cin[,grep("11", colnames(dat_cin))])


# For each row in the dat_cin object:
for(i in 1:nrow(dat_cin)){
  
  # Determine which row in dat matches the row in dat_cin
  row.match <-  match(dat_cin$PS.Site[i], rownames(dat.cin))
  
  dat.cin[row.match, 1:ncol(oh_seven), "2007"] <- as.matrix(oh_seven[i,])
  dat.cin[row.match, 1:ncol(oh_eight), "2008"] <- as.matrix(oh_eight[i,])
  dat.cin[row.match, 1:ncol(oh_nine), "2009"]  <- as.matrix(oh_nine[i,])
  dat.cin[row.match, 1:ncol(ten), "2010"]      <- as.matrix(ten[i,])
  # We will check to see if there are any overlapping sites surveyed in the 2 datafiles for 2011 later
  dat.cin[row.match, 1:ncol(eleven), "2011"]      <- as.matrix(eleven[i,])
  
}




# 6c. Dataset #2 - Convert from long to wide format -------------------------------------------------------



# Now looking at the second dataset: surv
# We will extract the information we need, and fill in the 3D array

# Subset the data to the species you need
sal.col <- apply(cbind(surv$Pcin.str, surv$Pcin.lead), 1, FUN = sum, na.rm = TRUE)

# Create a new data frame  
sal <- data.frame(GPSID = surv$GPSID, 
                  Transect = surv$Transect,
                  Date = surv$Date, 
                  sal.col = sal.col)

# Format the date
sal$Date <- as.Date(sal$Date, format = "%m/%d/%y")


# Gather the data- turn it into long format
sal <- melt(sal, id = c("GPSID", "Transect", "Date", "sal.col"))

# Turn it into wide format
# with sites along the rows and Date/Transects as the columns
sal <- dcast(sal, GPSID ~ Date + Transect, fun.aggregate = mean)

# save the site names
site.names <- sal[,1]

# remove the site names
sal <- sal[,-1]

# Anything with > 0 turns into a 1
sal[sal > 0] <- 1




# 6d. Dataset #2 - Seperate the data by year -------------------------------------------------------




# Note you may not see any data because of the large number of NAs
eleven2   <- sal[, grep("2011", colnames(sal))]
twelve    <- sal[, grep("2012", colnames(sal))]
thirteen  <- sal[, grep("2013", colnames(sal))]
fourteen  <- sal[, grep("2014", colnames(sal))]
# No data for 2015
sixteen   <- sal[, grep("2016", colnames(sal))]

# Calculate the maximum number of surveys
max.surv <- max(c(dim(eleven2)[2], 
                  dim(twelve)[2], 
                  dim(thirteen)[2], 
                  dim(fourteen)[2], 
                  dim(sixteen)[2] ))

# Move over data to remove all the NAs
eleven2.1 <- twelve.1 <- thirteen.1 <- fourteen.1 <- sixteen.1 <- array(NA, dim = c(dim(sal)[1], max.surv))

for(i in 1:dim(sal)[1]){
  a <- b <- d <- e <- f <- 1
  for(j in 1:dim(eleven2)[2]){
    if(is.na(eleven2[i, j]) == F){eleven2.1[i, a] <- eleven2[i, j];
    a <- a + 1}
  }
  
  for(j in 1:dim(twelve)[2]){
    if(is.na(twelve[i, j]) == F){twelve.1[i, b] <- twelve[i, j];
    b <- b + 1}
  }
  
  for(j in 1:dim(thirteen)[2]){
    if(is.na(thirteen[i, j]) == F){thirteen.1[i, d] <- thirteen[i, j];
    d <- d + 1}
  }
  
  for(j in 1:dim(fourteen)[2]){
    if(is.na(fourteen[i, j]) == F){fourteen.1[i, e] <- fourteen[i, j];
    e <- e + 1}
  }
  
  for(j in 1:dim(sixteen)[2]){   
    if(is.na(sixteen[i, j]) == F){sixteen.1[i, f] <- sixteen[i, j];
    f <- f + 1}
  }
}	

# Calculate the true number of max surveys per site
n.surv.max <- max(c(length(which(apply(eleven2.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(twelve.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(thirteen.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(fourteen.1, 2, max, na.rm = TRUE) > -1)),
                    length(which(apply(sixteen.1, 2, max, na.rm = TRUE) > -1))))




# 6e. Add data from non-overlapping sites in 2011 -------------------------------------------------------




# Because I don't know the dates that were surveyed in the first dataset (dat_shen), but I know the sites, I will look to see what sites what surveyed in both of sets
dont_match <- as.character(rownames(dat.cin)[which(!rownames(dat.cin) %in% dat_cin$PS.Site)])


# Now fill in the 2011 sheet with the data from the rows that do not match between the file with the 2007-2011 data and the file with 2011-2016 data
for(i in 1:length(dont_match)){

  # Determine which row in dat matches the dont_match row
  row.match.dat <-  match(dont_match[i], rownames(dat.cin))
  
  # Determine which row in eleven2.1 matches the dont_match row
  row.match.eleven <-  match(dont_match[i], site.names)
  
  # Fill in the big dataset
  dat.cin[row.match.dat, 1:n.surv.max, "2011"] <- as.matrix(eleven2.1[row.match.eleven, 1:n.surv.max])
  
}



# 6f. Fill in dat with the rest of the data -------------------------------------------------------



# Now you can fill in the other years
for(i in 1:length(site.names)){
  # Determine which row in dat matches the row in dat_shen
  row.match <-  match(site.names[i], rownames(dat))
  
  dat.cin[row.match, 1:n.surv.max, "2012"] <- as.matrix(twelve.1[i,1:n.surv.max])
  dat.cin[row.match, 1:n.surv.max, "2013"] <- as.matrix(thirteen.1[i,1:n.surv.max])
  dat.cin[row.match, 1:n.surv.max, "2014"] <- as.matrix(fourteen.1[i,1:n.surv.max])
  dat.cin[row.match, 1:n.surv.max, "2016"] <- as.matrix(sixteen.1[i,1:n.surv.max])
  
}



# 7. Rename the file -------------------------------------------------------


# Save array with shenandoah name
CIN_df <- dat.cin




# 8. Export the formatted data -------------------------------------------------------

# Create a single object with all objects
dat <- list(shen = SHEN_df,
            cin = CIN_df)

# Export the object
save(dat, file = "./Data/formatted_shen_cin_dat.rda")


# End script