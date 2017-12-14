#!/usr/bin/env Rscript
################################################################################
#
#  This script is a wrapper around an R script to evaluate a model with a 
#  given individual.  This will produce percentiles representing how the
#  individual compares to the reference population.
#
#  Authors: Kristi Clark (kclark@ini.usc.edu)
#           Ryan Cabeen (rcabeen@ini.usc.edu)
#
#  Date: December 2017
#
################################################################################

args <- commandArgs(TRUE)
subjfile <- as.character(args[1])
gender <- as.character(args[2])
age <- as.double(args[3])
databasedir <- as.character(args[4])
databaseprefix <- as.character(args[5])
outfile <- as.character(args[6])

source("lib/brainchart.R")
CompareIndividual(subjfile, gender, age, databasedir, databaseprefix, outfile)

################################################################################
# End
################################################################################
