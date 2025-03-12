
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(rerddap)

# Directories
plotdir <- "figures"
rawdir <- "data/calcofi/raw"
outdir <- "data/calcofi/processed"

# Read data
stations_orig <- read.csv(file.path(rawdir, "CalCOFIStationOrder.csv"))

# https://calcofi.org/sampling-info/
# https://calcofi.org/sampling-info/station-positions/
# https://www.fisheries.noaa.gov/inport/item/20691

# Format data
################################################################################

# Format data
stations <- stations_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(order=order_occ, 
         station=sta, 
         lat_dd=lat_dec, 
         long_dd=lon_dec, 
         depth=est_depth, 
         type=sta_type)


# Plot
ggplot(stations, aes(x=long_dd, y=lat_dd, shape=type, color=depth)) +
  geom_point(size=2)

# Export
saveRDS(stations, file=file.path(outdir, "calcofi_stations.Rds"))





