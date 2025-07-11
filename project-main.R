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
library(purrr)


# Clean Voteview data
source("~/git/new-wwi-women/clean-voteview.R")

# Harmonize congressional district boundaries from 63rd and 65th to 66th Congress.
source("~/git/new-wwi-women/harmonize-congressional-districts.R")

# Clean county-level data
  # Census
  # Etc.
# Merge county-level data and aggregate up to congressional district.
# Merge congressional district-level data.
# Analysis