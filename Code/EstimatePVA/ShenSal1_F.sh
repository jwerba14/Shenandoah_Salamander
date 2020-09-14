#!/bin/bash

# Start loop
for var1 in BioticAbiotic Abiotic;
    do
    for var2  in temp_mat_exp_range_SCEN2;
    do
        for var3 in RMI_mat_range_add RMI_mat_range_sub;
            do
                sbatch --export var1=${var1},var2=${var2},var3=${var3} --output=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_${var2}_${var3}_%a.output.txt --error=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_${var2}_${var3}_%a.error.txt /lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimatePVA/ShenSal2.sh
                done
        done
done
