#!/bin/bash
#SBATCH --job-name="PVA null biotic"
#SBATCH -n 1
#SBATCH -c 1
#SBATCH --time=24:00:00
#SBATCH --mem=32gb
#SBATCH --partition=normal
#SBATCH --account=pwrc

# Get started
echo "Job started on `hostname` at `date`"

# Call modules
module load gnu8/8.3.0 R/3.6.3 jags/4.3.0

# Call Rscript
Rscript --vanilla /lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/SummarizePVA/PVAsummary.R ${var1}

echo "Job ended at `date`"
