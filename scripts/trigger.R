################################################################################
# LOAD PACKAGES AND SCRIPTS IN SEQUENCE TO PRODUCE AHV EXPENDITURE PROJECTIONS #
################################################################################

# Clear workspace and load necessary packages.
rm(list = ls())
pkg <- c("tidyverse", "magrittr", "simputation", "readxl", "zoo", "tidymodels")
sapply(pkg, library, char = TRUE)

# Indicators whether 13th AHV pension/AHV21 should be respected.
ind_AHV13 <- TRUE
ind_AHV21 <- TRUE

# Indicate nominal versus real output.
ind_real <- TRUE

# Indicate usage of exogenous widow projections due to time series break in 2023
# (source: Thomas Borek @BSV, departement mathematics).
ind_wid <- TRUE 

# Set start and end year of projections.
p_start <- 2024
p_end   <- 2040

# Calculations. -----------------------------------------------------------

# Wrapper to read and process all necessary inputs.
source("scripts/prepare_inputs.R")

# Execute sliding-window crossvalidation and save optimal parameter values for
# extrapolations of explanatory variables.
source("scripts/cv_out.R")

# Produce projections.
source("scripts/proj_out.R", echo = TRUE)
