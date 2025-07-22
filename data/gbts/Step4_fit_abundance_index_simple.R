
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
outdir <- "data/gbts/processed"
plotdir <- "data/gbts/figures/detailed"

# Read data
hauls <- readRDS(file=file.path(outdir, "GBTS_hauls_use.Rds"))
catch <- readRDS(file=file.path(outdir, "GBTS_catch_use.Rds"))
species <- readRDS(file=file.path(outdir, "GBTS_species_to_evaluate.Rds")) %>% 
  arrange(desc(ptows))


# Fit index
################################################################################

# Species
species_list <- species$comm_name_orig

# Loop through species
x <- 59
# for(i in 1:length(species_list)){
results <- purrr::map_df(1:length(species_list), function(x){
  
  # Species do
  species_do <- species_list[x]
  print(paste(x, species_do, sep=" - "))
  
  # Subset data
  sdata <- catch %>% 
    # Filter to species of interest
    filter(comm_name_orig==species_do)
  
  # Build full data
  data <- hauls %>% 
    # Add common name
    mutate(comm_name=species_do) %>% 
    # Add larval densities
    left_join(sdata %>% select(trawl_id, cpue_kg_ha), by="trawl_id") %>% 
    # Change NAs to zeros
    mutate(cpue_kg_ha=ifelse(is.na(cpue_kg_ha), 0, cpue_kg_ha))
  
  # Summarize
  df <- data %>% 
    group_by(year) %>% 
    summarize(n=n(),
              index=mean(cpue_kg_ha)) %>% 
    ungroup() %>% 
    mutate(comm_name=species_do)

  # Plot
  ggplot(df, aes(x=year, y=index)) +
    geom_line() +
    theme_bw()
  
  # Return
  df
  
  
})

# Format results
results1 <- results %>% 
  left_join(species %>% select(comm_name_orig, sci_name), by=c("comm_name"="comm_name_orig")) %>% 
  select(comm_name, sci_name, year, index)

# Export
saveRDS(results1, file=file.path(outdir, "GBTS_indices_of_abundance_simple.Rds"))
  