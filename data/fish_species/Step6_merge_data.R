
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/fish_species/allen"
datadir1 <- "data/fish_species/miller_lea_1972"

# Read data
spp <- read.csv(file=file.path(datadir1, "ca_fish_species_miller_lea_1972.csv"), as.is=T)
tl <- readRDS(file=file.path(datadir, "allen_trophic_levels.Rds"))
size_orig <- readRDS(file=file.path(datadir, "larry_allen_size_age_data.Rds"))


# Merge
################################################################################

# Prep age/size
size <- size_orig %>% 
  group_by(species) %>% 
  summarize(lmax_cm=max(lmax_cm, na.rm=T), 
            tmax_yr=max(tmax_yr, na.rm=T)) %>% 
  ungroup()

# Merge data
data <- spp %>% 
  # Add trophic level
  left_join(tl %>% select(-comm_name), by=c("species"="sci_name"))
