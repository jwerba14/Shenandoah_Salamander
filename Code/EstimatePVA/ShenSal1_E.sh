#!/bin/bash

# Start loop
for var1  in Biotic Null;
    do
    sbatch --export var1=${var1} --output=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_%a.output.txt --error=/lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/logs_ShenSal/${var1}_%a.error.txt /lustre/projects/ecosystems/pwrc/gdirenzo/ShenSal/Dropbox/USGS/ShenandoahSalamander/Code/EstimatePVA/ShenSal3.sh
done
