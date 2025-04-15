
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(taxize)

# Directories
plotdir <- "figures"

# Read trawl survey footprint
gbts <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/dcrab_multistressor/data/trawl_survey/processed/nwfsc_wcbts_grid_cells.Rds") %>% 
  sf::st_as_sf() %>% 
  filter(ll_lat<=42)

# Read other stations
calcofi_orig <- readRDS("data/calcofi/processed/calcofi_stations.Rds") 
rreas_orig <- readRDS("data/rreas/processed/RREAS_station_key.Rds")
ccfrp_orig <- readRDS("data/ccfrp/processed/ccfrp_sites.Rds")
scuba_orig <- readRDS("data/kelp_scuba/processed/scuba_transects.Rds")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Build stations
################################################################################

# JANKY SCUBA STATIONS - MAKE CORRECT IN SCUBA SECTIN
scuba <- scuba_orig %>% 
  filter(year>=2008) %>% 
  group_by(campus, site) %>% 
  summarize(nyear=n_distinct(year),
            lat_dd=mean(lat_dd, na.rm=T),
            long_dd=mean(long_dd, na.rm=T)) %>% 
  ungroup() %>% 
  filter(nyear>=14) %>% 
  mutate(survey="SCUBA") %>% 
  select(survey, lat_dd, long_dd)


calcofi <- calcofi_orig %>% 
  filter(survey=="ROS") %>% 
  mutate(survey="CalCOFI") %>% 
  select(survey, lat_dd, long_dd)

rreas <- rreas_orig %>% 
  filter(strata=="Central" & area!="Monterey Outside") %>% 
  mutate(survey="RREAS") %>% 
  select(survey, lat_dd, long_dd)

ccfrp <- ccfrp_orig %>% 
  filter(mpa_region=="Central") %>% 
  mutate(survey="CCFRP") %>% 
  select(survey, lat_dd, long_dd)

stations <- bind_rows(calcofi, rreas, ccfrp, scuba)


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
g <- ggplot(stations, aes(x=long_dd, y=lat_dd, color=survey, )) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot trawl survey
  geom_sf(data=gbts, color="grey30", fill="grey30", inherit.aes = F) +
  # Path
  #geom_path(mapping=aes(x=long_dd, y=lat_dd), color="grey80", inherit.aes=F) +
  # Stations
  geom_point() + 
  # geom_text(color="black") +
  # Legend
  scale_color_discrete(name="Survey") +
  # Crop
  coord_sf(xlim = c(-126.5, -116.5), ylim = c(30, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.78, 0.8))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_monitoring_surveys.png"), 
       width=3.4, height=4.5, units="in", dpi=600)

