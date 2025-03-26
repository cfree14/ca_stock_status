
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
library(indexwc)
#devtools::install_github("pfmc-assessments/indexwc")

# Directories
indir <- "data/gbts/raw"
outdir <- "data/gbts/processed"
plotdir <- "data/gbts/figures/detailed"

# Read data
hauls <- readRDS(file=file.path(outdir, "GBTS_hauls_use.Rds"))
catch <- readRDS(file=file.path(outdir, "GBTS_catch_use.Rds"))
species <- readRDS(file=file.path(outdir, "GBTS_species_to_evaluate.Rds"))

# Source code
source("data/gbts/helper_functions.R")

# Add UTM coordinates
hauls_utm <- sdmTMB::add_utm_columns(dat = hauls, 
                                     ll_names = c("long_dd", "lat_dd"), 
                                     ll_crs=4326, 
                                     utm_crs = 32611, 
                                     utm_names = c("long_utm11km", "lat_utm11km"),
                                     units="km")

# Build mesh
mesh <- sdmTMB::make_mesh(hauls_utm, c("long_utm11km", "lat_utm11km"), cutoff = 50)
plot(mesh)


# Index package play
################################################################################

indexwc_configs <- indexwc::configuration
indexwc_data <- indexwc_configs |>
  dplyr::filter(species == "yellowtail rockfish") |>
  dplyr::filter(source == "NWFSC.Combo" & family == "sdmTMB::delta_gamma()") |>
  dplyr::filter(
    formula == "catch_weight ~ 0 + fyear*split_mendocino + pass_scaled"
  ) |>
  # Row by row ... do stuff then ungroup
  dplyr::rowwise() |>
  # Pull the data based on the function found in fxn column
  dplyr::mutate(
    data_raw = list(indexwc::format_data(eval(parse(text = fxn)))),
    data_filtered = list(data_raw |>
                           dplyr::filter(
                             depth <= min_depth, depth >= max_depth,
                             latitude >= min_latitude, latitude <= max_latitude,
                             year >= min_year, year <= max_year
                           ) |>
                           dplyr::mutate(split_mendocino = ifelse(latitude > 40.1666667, "N", "S")))
  ) |>
  dplyr::ungroup()
indexwc_data_filtered <- indexwc_data$data_filtered[[1]]


# Subset data
################################################################################

# Set up parallel backend
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Species
species_list <- species$comm_name_orig

# Loop through species
i <- 1
# for(i in 1:length(species_list)){
results <- foreach(i = 1:length(species_list), 
                   .packages = c("dplyr", "ggplot2", "sdmTMB")) %dopar% {
                     
  # Species do
  species_do <- species_list[i]
  print(paste(i, species_do, sep=" - "))
  
  # Build data
  data <- build_data(hauls=hauls_utm,
                     catch=catch,
                     species=species_do)
  
  # # Fit model
  # model <- fit_model(data=data)
  # 
  # # Inspect model (diagnostics)
  # inspect_model(model)
  # 
  # # Make predictions
  # preds <- make_preds(model, pred_grid)
  # 
  # # Extract and visualize index
  # index <- extract_index(preds)
  # 
  # # # Plot spatial random effects
  # # # plot_spatial_effects(preds)
  # 
  # # Save output
  # output <- list(model, preds, index)
  # outfile <- species_do %>% tolower(.) %>% gsub(" ", "_", .) %>% paste0(., ".Rds")
  # saveRDS(output, file.path(outdir, outfile))
                     
}

# Stop parallel cluster
stopCluster(cl)
