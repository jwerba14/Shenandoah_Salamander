# All code was written by G. V. DiRenzo
# If you have any questions, please email: grace.direnzo@gmail.com

# Objective: This document tells you the location of all files used in the Pshen PVA

######################################
######### Table of Contents ##########
######################################

# 0. Format the data
# 1. FINAL MODELS
# 2. To estimate the parameters of the model
# 3. To estimate population viability analysis (PVA) & calculate extinction metrics:
# 4. To summarize the PVA output
# 5. To create population size over time figure
# 6. To create a figure with PVA metrics plotted
# 7. To create table with PVA metrics
# 8. PVA output
# 9. PVA summary output
# 10. FINAL FIGURES
# 11. FINAL Tables

######################################
######################################
######################################
# 0. Format the data
    - Detection/non-detection data
	   ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/FormatData/Format Data 15 June 2020.R
    - Latitude/Longitude data
        ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/FormatData/Format Format Lat Long data 16 June 2020.R
    - Temp/RMI data
        ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/FormatData/Format temp and RMI data.R

# 1. FINAL MODELS
    ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Models

# 2. To estimate the parameters of the model:
	~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimateParameters/ShenSal_ModRuns.R

# 3. To estimate population viability analysis (PVA) & calculate extinction metrics:
    # This needs to be done on the cluster because
    # large arrays are created and can not be dealt with on your local computer
    # Notice that the folders for this output are EMPTY because it is too much to download off the cluster
        # Bash script
	       ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimatePVA/ShenSal1.sh
        # SLURM code for: BioticAbiotic and Abiotic
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimatePVA/ShenSal2_abiotic.sh
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimatePVA/ShenSal2_bioticabiotic.sh
        # SLURM code for: Biotic and Null
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimatePVA/ShenSal3.sh
        # R script
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimatePVA/PVAruns_1MCMC.R

 # 4. To summarize the PVA output:
        # Again- this is done on the cluster
        # Bash script
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/SummarizePVA/ShenSal_PVAsum1.sh
        # SLURM code for: BioticAbiotic and Abiotic
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/SummarizePVA/ShenSal_PVAsum2.sh
        # SLURM code for: Biotic and Null
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/SummarizePVA/ShenSal_PVAsum3.sh
        # R script
            ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/SummarizePVA/PVAsummary.R

# 5. To create population size over time figure:
     ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/FigureCode/Population size through time.R

# 6. To create a figure with PVA metrics plotted:
     ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/TableCode/PVA metric summary table.R

# 7. To create table with PVA metrics:
     ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/TableCode/PVA metric summary table.R

# 8. PVA output
    # This is where it is on Yeti - can not download - too big
	~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/PVA/

# 9. PVA summary output
    ~/Yeti/ShenSal/Dropbox/USGS/ShenandoahSalamander/PVA_summary/

# 10. FINAL FIGURES
    ~/Yeti/ShenSal/Figures

# 11. FINAL Tables
	~/Yeti/ShenSal/Tables
