#!/usr/bin/env /usr/local/R-3.3.3/bin/Rscript
################################################################################
#
#  This script is a wrapper around an R script to split the complete dataset
#  into individual CSV tables for each morphometric 
#
#  Authors: Kristi Clark (kclark@ini.usc.edu)
#           Ryan Cabeen (rcabeen@ini.usc.edu)
#
#  Date: December 2017
#
################################################################################

args <- commandArgs(TRUE)
imagfile <- as.character(args[1])
covfile <- as.character(args[2])
outdir <- as.character(args[3])

source("lib/brainchart.R")
split_input(imagfile, covfile, outdir)

################################################################################
# End
################################################################################
