#Trial 1 & 2 PRE and ERE Calculations
#A. Host 1/24/23

# load packages----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(gsheet))install.packages("gsheet");library(gsheet)
if(!require(readxl))install.packages("readxl");library(readxl)

# bring in data----
source("scripts/download_data-EX.R")
source("scripts/proximates_dataorg_analysis.R")
