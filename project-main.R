# The Political Economy of Women's Suffrage and World War I
# Madison Arnsbarger
# August 2021-present
rm(list = ls())

# Settings
setwd("~/Dropbox/Weber/Research/JMP") # set working directory for entire project

options(
  scipen = 999, # no scientific notation
  max.print=10000 # set default value limit
)

# Load libraries
library(readxl) # for importing excel data
library(dplyr)
library(haven)
library(stringr) # for manipulating strings
library(tidyr)
library(tigris)
library(sf)
library(raster)
library(ggplot2) # for visualizations
library(ggmap)
library(stringr)
library(geosphere)
library(viridis)
library(cowplot)
library(purrr)
library(tidyverse)
library(fixest) # for fixed effects, clustering
library(modelsummary) # for regression tables
library(stargazer)
library(estimatr)

# Clean LHS - Voteview data
source("~/git/new-wwi-women/clean-voteview.R")

# Harmonize congressional district boundaries from 63rd and 65th to 66th Congress.
source("~/git/new-wwi-women/harmonize-congressional-districts.R")

# Clean county-level data
source("~/git/new-wwi-women/clean-cnty-data.R")
  # WWI Casualties (from Andy Ferrara)
  # WWI Draft (from Andy Ferrara)
  # ICPSR 1910 county characteristics
  # ICPSR 1860 county characteristics
  # 1910 Census characteristics (from NBER)
  # Civil War casualties, etc.
  # Unions (from Henry Downes)

# Clean the county-to-congressional district crosswalks
source("~/git/new-wwi-women/clean-cnty-cd-crosswalks.R")

# Merge county-level data and aggregate up to congressional district.
source("~/git/new-wwi-women/merge-cnty-to-cd.R")

# Clean "other" RHS congressional district-level data
source("~/git/new-wwi-women/clean-other-data.R")
  # Congressional representative characteristics (ICPSR)
  # States' voting rights for women

# Merge & clean RHS congressional district-level data.
source("~/git/new-wwi-women/clean-cd-data.R")

# Analysis - DiD
source("~/git/new-wwi-women/analysis.R")

# Compute SSIV
source("~/git/new-wwi-women/write-ssiv.R")

# Analysis - SSIV


# Visualizations
source("~/git/new-wwi-women/viz.R")