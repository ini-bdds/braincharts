#!/usr/bin/env Rscript
################################################################################
#
#  This script is a wrapper around an R script to estimate models for an 
#  individual CSV table.  This produces six models with different types of 
#  normalization.
#
#  Authors: Kristi Clark (kclark@ini.usc.edu)
#           Ryan Cabeen (rcabeen@ini.usc.edu)
#
#  Date: December 2017
#
################################################################################

source("lib/brainchart.R")

args <- commandArgs(TRUE)
infile <- as.character(args[1])
outdir <- as.character(args[2])

noneDir <- file.path(outdir, "none")
icvDir <- file.path(outdir, "normICV")
varyDir <- file.path(outdir, "normVARY")

dir.create(outdir, showWarnings=F)
dir.create(noneDir, showWarnings=F)
dir.create(icvDir, showWarnings=F)
dir.create(varyDir, showWarnings=F)

base <- gsub("data.csv", "model", basename(infile))
none <- file.path(noneDir, base)
icv  <- file.path(icvDir,  base)
vary <- file.path(varyDir, base)

estimate_brain_LMS_combo(infile, none, 1) 
estimate_brain_LMS_combo(infile, icv,  2) 
estimate_brain_LMS_combo(infile, vary, 3)
estimate_brain_LMS_combo(infile, vary, 4)
estimate_brain_LMS_combo(infile, vary, 5)
estimate_brain_LMS_combo(infile, vary, 6)

################################################################################
# End
################################################################################
