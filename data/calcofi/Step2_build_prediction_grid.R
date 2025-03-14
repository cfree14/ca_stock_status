
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

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Read footprint
footprint <- sf::st_read(file.path(outdir, "calcofi.shp")) %>% 
  sf::st_transform(crs = "+proj=utm +zone=11 +datum=WGS84 +units=m")

# TO-DO LIST
# Add area

# Build raster
################################################################################

# Resolution
res_m <- 40000

# Buffer footprint
footprint_buffered <- sf::st_buffer(footprint, dist=res_m)

# Extent of stations
xmin <- min(stations_orig$long_utm11m) - res_m
xmax <- max(stations_orig$long_utm11m) + res_m
ymin <- min(stations_orig$lat_utm11m) - res_m
ymax <- max(stations_orig$lat_utm11m) + res_m

# Create raster with 30 km resolution
ras <- raster(xmn = xmin, 
              xmx = xmax, 
              ymn = ymin, 
              ymx = ymax, 
              resolution = res_m, 
              crs = "+proj=utm +zone=11 +datum=WGS84 +units=m")

# Assign unique IDs to each raster cell
ras[] <- 1:ncell(ras)

# Print raster details
print(ras)

# Plot the raster to visualize
plot(ras)
points(stations_orig$long_utm11m, stations_orig$lat_utm11m, col = "red", pch = 19)

# Mask by footprint
ras_masked <- mask(ras, as(footprint_buffered, "Spatial")) 

# Plot
plot(ras_masked)
points(stations_orig$long_utm11m, stations_orig$lat_utm11m, col = "red", pch = 19)

# Make dataframe
grid <- ras_masked %>% 
  # Convert to dataframe
  raster::as.data.frame(xy=T) %>% 
  # Only cells in footprint
  filter(!is.na(layer)) %>% 
  select(-layer) %>% 
  # Rename
  rename(long_utm11m=x,
         lat_utm11m=y) %>% 
  # Calculate
  mutate(long_utm11km=long_utm11m/1000,
         lat_utm11km=lat_utm11m/1000)


# Export
saveRDS(grid, file=file.path(outdir, "calcofi_prediction_grid.Rds"))


# Plot raster
################################################################################





