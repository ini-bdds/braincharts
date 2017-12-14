#! /bin/bash
################################################################################
# 
#  BrainChart - Main Script
# 
#    This script demonstrates the creation and use of brain growth charts.
#    The input files for this script are:
#     
#      input/PNC_PNG_FS_imaging.csv: regional brain metrics
#      input/PNC_PNG_ages_gender_WBvals.csv: demographic and whole brain metrics
#      input/PNC0015_M10_FS_imaging.csv: an example individual dataset
#
#    After running, this will create:
#
#      output/models: growth model parameters
#      output/plots: growth model plots 
#      output/PNC0015_M10_FS_percentiles.csv: percentiles for an individual
#
#  Authors: Ryan Cabeen (rcabeen@ini.usc.edu)
#           Kristi Clark (kclark@ini.usc.edu)
#
#  Date: December 2017
# 
################################################################################

################################################################################
echo "Started"
################################################################################

cd $(dirname ${0})/..

################################################################################
echo "Splitting data into individual tables"
################################################################################

if [ ! -e output/data ]; then
  lib/split.R \
    input/PNC_PNG_FS_imaging.csv.gz \
    input/PNC_PNG_ages_gender_WBvals.csv \
    output/data
fi

################################################################################
echo "Estimating models"
################################################################################

for table in output/data/*data.csv; do
    echo "... processing: ${table}"
    lib/estimate.R ${table} output/models
done

################################################################################
echo "Evaluating single subject"
################################################################################

lib/evaluate.R \
  input/TEST_M10_FS_imaging.csv \
  M 125 \
  output/models/none PNC_PNG \
  output/TEST_M10_FS_percentiles.csv

################################################################################
echo "Plotting a morphometric"
################################################################################

mkdir -p output/plots

for gender in M F; do 
  for model in none normICV; do 
    for metric in area thickness volume; do
      a="PNC_PNG FS_aparc_2009"
      b="PNC_PNG_FS_aparc_2009"

      lib/plot.R ${model} ${a} LeftHemi L ${metric} ${gender} \
        output/plots/${model}_${b}_LeftHemi_L_${metric}_${gender}.pdf

      lib/plot.R ${model} ${a} RightHemi R ${metric} ${gender} \
        output/plots/${model}_${b}_RightHemi_R_${metric}_${gender}.pdf
    done
  done

  c="EstimatedTotalIntraCranialVol"
  lib/plot.R none PNC_PNG FS_aseg ${c} N volume ${gender} \
    output/plots/none_PNC_PNG_FS_aseg_${c}_N_volume_${gender}.pdf

  c="ctx-lh_precentral"
  d="area"
  lib/plot.R none PNC_PNG FS_aparc_aseg ${c} N ${d} ${gender} \
    output/plots/none_PNC_PNG_FS_aparc_aseg_${c}_N_${d}_${gender}.pdf
done


################################################################################
echo "Finished"
################################################################################
