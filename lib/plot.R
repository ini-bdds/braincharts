#!/usr/bin/env /usr/local/R-3.3.3/bin/Rscript
################################################################################
#
#  This script is a wrapper around an R script to plot the model and raw data
#  of a given morphometric.
#
#  Authors: Kristi Clark (kclark@ini.usc.edu)
#           Ryan Cabeen (rcabeen@ini.usc.edu)
#
#  Date: December 2017
#
################################################################################

args <- commandArgs(TRUE)

model   <- as.character(args[1])
dataset <- as.character(args[2])
atlas   <- as.character(args[3])
region  <- as.character(args[4])
hemi    <- as.character(args[5])
metric  <- as.character(args[6])
gender  <- as.character(args[7])
output  <- as.character(args[8])

source("lib/brainchart.R")
chart.plot(model, dataset, atlas, region, hemi, metric, gender, output)

################################################################################
# End
################################################################################
