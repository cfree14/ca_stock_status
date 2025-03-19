
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

# Read prediction grid
pred_grid <- readRDS(file.path(datadir, "RREAS_prediction_grid.Rds"))


# Read helper functions
source("data/rreas/helper_functions.R")

# Build mesh
mesh <- make_mesh(tows_orig, c("long_utm10km", "lat_utm10km"), cutoff = 5) # 10 is a slow model fit, 5 is too big
plot(mesh)


# Loop through species
################################################################################

# Set up parallel backend
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Species
species_list <- sort(unique(data_orig$comm_name_long))

# Loop through species
i <- 11
# for(i in 1:length(species_list)){
results <- foreach(i = 1:length(species_list), 
                   .packages = c("dplyr", "ggplot2", "sdmTMB")) %dopar% {
  
  # Species do
  species_do <- species_list[i]
  print(paste(i, species_do, sep=" - "))
  
  # Build data
  data <- build_data(tow_data=tows_orig,
                     count_data=data_orig,
                     species=species_do)
  
  # Fit model
  model <- fit_model(data=data)

  # Inspect model (diagnostics)
  inspect_model(model)

  # Make predictions
  preds <- make_preds(model, pred_grid)

  # Extract and visualize index
  index <- extract_index(preds)

  # # Plot spatial random effects
  # # plot_spatial_effects(preds)

  # Save output
  output <- list(model, preds, index)
  outfile <- species_do %>% tolower(.) %>% gsub(" ", "_", .) %>% paste0(., ".Rds")
  saveRDS(output, file.path(outdir, outfile))
  
}

# Stop parallel cluster
stopCluster(cl)



