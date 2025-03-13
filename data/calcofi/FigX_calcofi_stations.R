
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(rerddap)

# Directories
rawdir <- "data/calcofi/raw"
outdir <- "data/calcofi/processed"
plotdir <- "data/calcofi/figures"

# Read data
# https://calcofi.org/sampling-info/
# https://calcofi.org/sampling-info/station-positions/
# https://www.fisheries.noaa.gov/inport/item/20691
stations_orig <- read.csv(file.path(rawdir, "CalCOFIStationOrder.csv"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Build data
################################################################################

# Build data
stations <- stations_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(order=order_occ, 
         station=sta, 
         lat_dd=lat_dec, 
         long_dd=lon_dec, 
         depth=est_depth, 
         survey=sta_type) %>% 
  # Simplify
  select(survey, line, station, order, long_dd, lat_dd, depth) %>% 
  arrange(order) %>% 
  # Add pattern
  mutate(pattern=ifelse(line>=76.7, "75 stations (summer/fall)", "104/113 stations (winter/spring)"),
         pattern=factor(pattern, levels=c("75 stations (summer/fall)", "104/113 stations (winter/spring)"))) 

# Add UTM coordinates
############################

# Convert to sf object with WGS84 (EPSG:4326)
stations_sf <- sf::st_as_sf(stations, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform to UTM Zone 11N (EPSG:32611)
stations_sf_utm <- sf::st_transform(stations_sf, crs = 32611)

# Extract UTM coordinates
utm11_easting <- sf::st_coordinates(stations_sf_utm)[, 1]
utm11_northing <- sf::st_coordinates(stations_sf_utm)[, 2]

# Add
stations <- stations %>% 
  mutate(lat_utm11m=utm11_northing,
         long_utm11m=utm11_easting,
         lat_utm11km=lat_utm11m/1000,
         long_utm11km=long_utm11m/1000) %>% 
  relocate(depth, .after=long_utm11m)


# Export
############################

# Export station data
saveRDS(stations, file=file.path(outdir, "calcofi_stations.Rds"))
write.csv(stations, file=file.path(outdir, "calcofi_stations.csv"), row.names = F)

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stations, aes(x=long_dd, y=lat_dd, shape=survey, color=pattern, label=order)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Path
  geom_path(mapping=aes(x=long_dd, y=lat_dd), color="grey80", inherit.aes=F) +
  # Stations
  geom_point() + 
  # geom_text(color="black") +
  # Legend
  scale_shape_discrete(name="Survey") +
  scale_color_discrete(name="Pattern") +
  # Crop
  coord_sf(xlim = c(-126.5, -116.5), ylim = c(30, 38)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.78, 0.8))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_stations.png"), 
       width=4.5, height=4.5, units="in", dpi=600)



