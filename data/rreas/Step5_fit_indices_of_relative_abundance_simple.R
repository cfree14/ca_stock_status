
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)
library(foreach)
library(doParallel)

# Directories
datadir <- "data/rreas/processed"
outdir <- "data/rreas/output"
plotdir <- "data/rreas/figures/detailed"

# Read data
data_orig <- readRDS(file=file.path(datadir, "RREAS_catch_data_use.Rds"))
tows_orig <- readRDS(file=file.path(datadir, "RREAS_haul_key_use.Rds"))
stations <- readRDS(file=file.path(datadir, "RREAS_station_key.Rds")) %>% filter(strata=="Central")


# Loop through species
################################################################################

# Species
species_list <- sort(unique(data_orig$comm_name_long))

# Loop through species
x <- species_list[1]
results <- purrr::map_df(species_list, function(x){
  
  # Species do
  species_do <- x

  # Build data
  data <- data_orig %>% 
    filter(comm_name_long==species_do)
  
  # Calculate index
  df <- data %>% 
    group_by(comm_name, sci_name, year) %>% 
    summarize(index=mean(catch_n)) %>% 
    ungroup()
    
})

# Export
saveRDS(results, file=file.path(datadir, "RREAS_indices_of_abundance_simple.Rds"))
  
