
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(rerddap)

# Directories
indir <- "data/rreas/raw"
outdir <- "data/rreas/processed"


# Read data
catch_orig <- readRDS(file=file.path(outdir, "RREAS_catch_data.Rds"))
stations <- readRDS(file=file.path(outdir, "RREAS_station_key.Rds"))
spp_key <- readRDS(file=file.path(outdir, "RREAS_species_key.Rds"))
hauls_orig <- readRDS(file=file.path(outdir, "RREAS_haul_key.Rds"))


# Build species to do
################################################################################

# Nyr
n_distinct(hauls_orig$year)
ntows_tot <- hauls_orig %>% 
  # Focus on Central
  filter(strata=="Central") %>% 
  nrow()

# Species
stats <- catch_orig %>% 
  # Focus on Central
  filter(strata=="Central" & maturity_code %in% c("A", "Y") & catch_n>0) %>% 
  # Summarize
  group_by(spp_code, comm_name, sci_name, maturity_code) %>% 
  summarize(nyrs=n_distinct(year),
            ntows=n()) %>% 
  # Filter
  filter(nyrs>25 & ntows>250) %>% 
  # Compute percent of tows
  mutate(ptows=ntows/ntows_tot) %>% 
  # Add common name long
  mutate(comm_name_long=ifelse(maturity_code=="A", paste(comm_name, "(adults)"), comm_name))

# Plot data
ggplot(stats, aes(y=reorder(comm_name_long, desc(ptows)), x=ptows, fill=maturity_code)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of tows", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw()

# Export


# Build data
################################################################################

# Hauls
##################################

# Filter data
hauls <- hauls_orig %>% 
  # Reduce to Central
  filter(strata=="Central") 

# Convert to spatial
hauls_sf <- hauls %>% 
  # Convert to sf object with WGS84 (EPSG:4326)
  sf::st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326) %>% 
  # Transform to UTM Zone 10N (EPSG:32610)
  sf::st_transform(tows_sf, crs = 32610)

# Extract UTM coordinates
utm10_easting <- sf::st_coordinates(hauls_sf)[, 1]
utm10_northing <- sf::st_coordinates(hauls_sf)[, 2]
hauls_out <- hauls %>% 
  mutate(long_utm10m = utm10_easting,
        lat_utm10m = utm10_northing,
        long_utm10km = long_utm10m / 1000,
        lat_utm10km = lat_utm10m / 1000)

# Export data
saveRDS(hauls_out, file=file.path(outdir, "RREAS_haul_key_use.Rds"))


# Catch
##################################

# Filter data
catch <- catch_orig %>% 
  # Reduce to Central
  filter(strata=="Central") %>% 
  # Add common name long
  mutate(comm_name_long=ifelse(maturity_code=="A", paste(comm_name, "(adults)"), comm_name)) %>% 
  # Reduce to species to do
  filter(comm_name_long %in% stats$comm_name_long)

# Convert to spatial
catch_sf <- catch %>% 
  # Convert to sf object with WGS84 (EPSG:4326)
  sf::st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326) %>% 
  # Transform to UTM Zone 10N (EPSG:32610)
  sf::st_transform(tows_sf, crs = 32610)

# Extract UTM coordinates
utm10_easting <- sf::st_coordinates(catch_sf)[, 1]
utm10_northing <- sf::st_coordinates(catch_sf)[, 2]
catch_out <- catch %>% 
  mutate(long_utm10m = utm10_easting,
        lat_utm10m = utm10_northing,
        long_utm10km = long_utm10m / 1000,
        lat_utm10km = lat_utm10m / 1000)

# Export data
saveRDS(catch_out, file=file.path(outdir, "RREAS_catch_data_use.Rds"))

