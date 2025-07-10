# The Political Economy of Women's Suffrage and World War I
# Madison Arnsbarger
# August 2021-present

# Settings
setwd("~/Dropbox/Weber/Research/JMP") # set working directory for entire project

options(
  scipen = 999, # no scientific notation
  max.print=10000 # set default value limit
)

# Load libraries
library(readxl)
library(dplyr)
library(haven)
library(stringr)
library(tidyr)
library(tigris)
library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(stringr)
library(geosphere)
library(viridis)
library(cowplot)


# Step 1: Clean raw data
source("~/git/new-wwi-women/clean-voteview.R")
source("~/git/new-wwi-women/harmonize-congressional-districts.R")

# Step 2: Merge data

# Step 3: Analysis