
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(rerddap)
library(raster)
library(fasterize)

# Directories
rawdir <- "data/calcofi/raw"
outdir <- "data/calcofi/processed"
plotdir <- "data/calcofi/figures"

# Read station key
stations_orig <- readRDS(file=file.path(outdir, "calcofi_stations.Rds")) 
tows_orig <- readRDS(file=file.path(outdir, "calcofi_fish_larvae_tows_use.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf") %>% 
  sf::st_transform(crs = 32611)
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")  %>% 
  sf::st_transform(crs = 32611)


# Build prediction grid
################################################################################

# Resolution
res_km <- 5
res_m <- res_km * 1000

# Build prediction grid
pred_grid <- tows_orig %>% 
  # Round
  mutate(long_utm11km=floor(long_utm11km/res_km)*res_km,
         lat_utm11km=floor(lat_utm11km/res_km)*res_km,
         long_utm11m=long_utm11km*1000,
         lat_utm11m=lat_utm11km*1000)

# Export
saveRDS(pred_grid, file=file.path(outdir, "calcofi_prediction_grid.Rds"))


# Plot grid
################################################################################

ggplot(pred_grid, aes(x=long_utm11m, y=lat_utm11m)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot grid
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = range(pred_grid$long_utm11m), ylim = range(pred_grid$lat_utm11m)) +
  # Theme
  theme_bw()


