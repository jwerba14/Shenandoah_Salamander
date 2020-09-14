#!/bin/bash

# Start loop
for var1 in Abiotic BioticAbiotic;
    do
    for var2  in temp_mat_range_SCEN1 temp_mat_range_SCEN2 temp_mat_exp_range_SCEN1 temp_mat_exp_range_SCEN2;
    do
        for var3 in RMI_mat_range_add RMI_mat_range_sub;
            do
				sbatch --export var1=${var1},var2=${var2},var3=${var3} --output=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_${var2}_${var3}_summary.output.txt --error=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_${var2}_${var3}_summary.error.txt /lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/SummarizePVA/ShenSal_PVAsum2.sh
                done
		done
done

# Start loop
for var1  in Biotic Null;
    do
		sbatch --export var1=${var1} --output=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_summary.output.txt --error=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_summary.error.txt /lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/SummarizePVA/ShenSal_PVAsum3.sh
done
