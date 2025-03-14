
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(rerddap)

# Directories
datadir <- "data/calcofi/processed"
plotdir <- "data/calcofi/figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "calcofi_fish_larvae_counts.Rds"))
tows_orig <- readRDS(file=file.path(datadir, "calcofi_fish_larvae_tows.Rds"))
stations <- readRDS(file=file.path(datadir, "calcofi_stations.Rds"))


# Tows to use
################################################################################

# Format
table(tows_orig$tow_type)
tows <- tows_orig %>% 
  # Filter to standardized data
  filter(year>=1985 & year<2023) %>% 
  filter(!is.na(survey) & survey=="ROS") %>% 
  filter(tow_type=="CalCOFI oblique bongo tow")


ggplot(tows, aes(x=yday, y=year, color=tow_type)) +
  geom_point() +
  # Theme
  theme_bw()

# Export
saveRDS(tows, file=file.path(datadir, "calcofi_fish_larvae_tows_use.Rds"))

# Data to use
################################################################################

# Format
data <- data_orig %>% 
  # Only data from relevant tows
  filter(tow_id %in% tows$tow_id)

# Export
saveRDS(data, file=file.path(datadir, "calcofi_fish_larvae_counts_use.Rds"))


# Species to use
################################################################################

# Export
stats <- data %>% 
  # Count number of years and tows
  group_by(comm_name, sci_name, taxa_type) %>% 
  summarize(nyrs=n_distinct(year),
            ntows=n_distinct(tow_id)) %>% 
  ungroup() %>% 
  # Filter to requirements
  filter(nyrs>=30 & ntows>=300) %>% 
  # Compute percent of tows
  mutate(ptows=ntows/nrow(tows))

# Export
write.csv(stats, file=file.path(datadir, "calcofi_species_to_evaluate.csv"))


