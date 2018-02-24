#-----------------------------------------------------------------------------
# Study.......: STHLM3MR
# Date........: 2016-12-15
# Author......: petstr (peter.strom@ki.se)
# Purpose.....: Combining STHLM3MR raw data into analysis data. 
# Software....: R v.3.2.5
# Data used...: P:/STHLM3MR/STHLM3MR_Admin/Data/R?Data/
#         /Oslo_20161212/Sthlm3_Oslog_data 091216.xlsx
#         /Stockholm_20161212/
#         /T?nsberg_20161212/
# Output......: P:/STHLM3MR/STHLM3MR_Admin/Data/Analysisdata/
#-----------------------------------------------------------------------------

#-- Libraries ----------------------------------------------------------------
wants <- c("xlsx", "dplyr", "tidyr", "data.table", "ggplot2")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)
rm(wants, has)

#-- Main program -------------------------------------------------------------
folder <- "C:/Users/petstr/Box Sync/ProjectDocs/MRI1/" ## Write tables.
n = 1000 ## Bootstrap samples
write_output <- TRUE
external_age_tonsberg <- TRUE
source("raw_to_analysis_MRI1.R")
source("tables.R")
source("supplementary_tables.R") ## Include supplementary tables.
source("figure.R")

##############################################################################
## NOTES about the data
##############################################################################
# Problem: Oslo does not report biopsy length if there was no cancer in a specific core.
# Solution: Use the logical variable "Standart BX not taken /registered" to identify if 
# systematic biopsy was done. In sto_ton use biopsy length more than 0 mm. 
# 
# Problem: Oslo does not report an overall Gleason score. 
# Solutions: Use max function over each biopsy core. If exist (for sto_ton) use Over_all_Gleason
# for target and Over_all_gleason for standard (i.e. low-case g).
