
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
datadir <- "data/calcofi/processed"
outdir <- "data/calcofi/output"
plotdir <- "data/calcofi/figures/detailed"

# Read data
data_orig <- readRDS(file=file.path(datadir, "calcofi_fish_larvae_counts_use.Rds"))
tows_orig <- readRDS(file=file.path(datadir, "calcofi_fish_larvae_tows_use.Rds"))
stations <- readRDS(file=file.path(datadir, "calcofi_stations.Rds")) %>% filter(survey=="ROS")

# Read prediction grid
pred_grid <- readRDS(file.path(datadir, "calcofi_prediction_grid.Rds"))

# Read species
species_df <- read.csv(file=file.path(datadir, "calcofi_species_to_evaluate.csv"), as.is=T) %>% 
  arrange(desc(ptows))

# Read helper functions
source("data/calcofi/helper_functions.R")

# Build mesh
mesh <- make_mesh(tows_orig, c("long_utm11km", "lat_utm11km"), cutoff = 50) # 10 is a slow model fit, 5 is too big
plot(mesh)


# Loop through species
################################################################################

# Set up parallel backend
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Species
species_list <- species_df$comm_name

# Loop through species
i <- 1
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
  
  # Plot spatial random effects
  # plot_spatial_effects(preds)
  
  # Save output
  output <- list(model, preds, index)
  outfile <- species_do %>% tolower(.) %>% gsub(" ", "_", .) %>% paste0(., ".Rds")
  saveRDS(output, file.path(outdir, outfile))
  
}

# Stop parallel cluster
stopCluster(cl)



