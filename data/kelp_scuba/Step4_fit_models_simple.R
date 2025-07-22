
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
indir <- "data/kelp_scuba/raw"
datadir <- "data/kelp_scuba/processed"
plotdir <- "data/kelp_scuba/figures"
outdir <- "data/kelp_scuba/output"

# Read data
spp_orig <- readRDS(file=file.path(datadir, "scuba_species.Rds"))
data_orig <- readRDS(file=file.path(datadir, "scuba_data.Rds"))
transect_orig <- readRDS(file=file.path(datadir, "scuba_transects.Rds"))
spp2eval <- readRDS(file=file.path(datadir, "scuba_species_to_evaluate.Rds"))
sites2eval <- readRDS(file.path(datadir, "scuba_sites_to_evaluate.Rds"))



# Build data
################################################################################

# Build data 
data_nonzero <- data_orig %>% 
  # 2008 forward (so that we can incude southern VRG sites)
  filter(year>=2008) %>% 
  # Only surveys after July 1
  filter(yday>=182) %>% 
  # Reduce to qualifying sites
  filter(site %in% sites2eval$site) %>% 
  # Reduce to qualifying species
  filter(sci_name %in% spp2eval$sci_name) %>% 
  # Simplify
  select(year, date, yday, site, zone, level, 
         transect, transect_id, survey_id, lat_dd, long_dd,
         comm_name, sci_name, count)

# Inspect
freeR::complete(data_nonzero)

# All transects at qualifying sites
################################################################################

freeR::which_duplicated(transect_orig$transect_id)

# All of the transects to be analyse
transects_use <- transect_orig %>% 
  filter(year>=2008 & yday>=182 & site %in% sites2eval$site) %>% 
  # Add UTM coordinates
  sdmTMB::add_utm_columns(ll_names=c("long_dd", "lat_dd"),
                          utm_names=c("long_utm10km", "lat_utm10km"),
                          ll_crs=4326,
                          utm_crs = 32610,
                          units="km")

# Expand data to include zeros
spp_list <- spp2eval$comm_name
x <- spp_list[1]
data <- purrr::map_df(spp_list, function(x){
  
  sdata_nonzero <- data_nonzero %>% 
    filter(comm_name==x) %>% 
    select(transect_id, comm_name, sci_name, count)
  
  sdata <- transects_use %>%
    # Simplify
    select(year, date, yday, site, zone, level, 
           transect, transect_id, survey_id, lat_dd, long_dd, lat_utm10km, long_utm10km) %>% 
    # Add non-zero counts
    left_join(sdata_nonzero, by="transect_id") %>% 
    # Fill in missing zeros
    mutate(count=ifelse(is.na(count), 0, count)) %>% 
    fill(comm_name:sci_name, .direction = "downup")

}) 


# Fit model
################################################################################

# Loop through species
x <- spp_list[1]
results <- purrr::map_df(spp_list, function(x){
  
  # Subset data
  spp_do <- x
  sdata <- data %>% 
    filter(comm_name==spp_do)
  sci_name <- unique(sdata$sci_name)
  
  # Calculate index
  df <- sdata %>% 
    group_by(year) %>% 
    summarize(index=mean(count)) %>% 
    ungroup() %>% 
    mutate(comm_name=x,
           sci_name=sci_name) %>% 
    select(comm_name, sci_name, year, index)
  
  ggplot(df, aes(x=year, y=index)) +
    geom_line()
  
  df
  
  
})

# Export
saveRDS(results, file=file.path(datadir, "SCUBA_indices_of_abundance_simple.Rds"))
