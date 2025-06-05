
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landing_receipts_2023/processed/"
data_orig <- readRDS(file.path(datadir, "1980_2022_landings_receipts.Rds"))

# Blocks
blocks <- wcfish::blocks

# State waters
state_waters_poly <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/ca_gillnet_bycatch/data/gis_data/CA_state_waters_polygons.Rds")
state_waters_line <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/ca_gillnet_bycatch/data/gis_data/CA_state_waters_polyline.Rds")


# Build data
################################################################################

# Build data
length(2013:2022)
data_blocks <- data_orig %>% 
  # Last 10 years
  filter(year>=2013) %>% 
  # Summarize
  group_by(block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id), 
            landings_lbs=sum(landings_lbs, na.rm=T)/10) %>% 
  ungroup() %>% 
  # Convert
  mutate(landings_kg=measurements::conv_unit(landings_lbs, "lbs", "kg"),
         landings_mt=landings_kg/1000) %>% 
  # Rule of three
  filter(nvessels>=3)

# Spatialize
data_blocks_sf <- blocks %>% 
  # Add landings
  left_join(data_blocks) %>% 
  # Filter to blocks of interest
  filter(block_state=="California" & block_type!="Offshore")

# Time series data
data_ts <- data_orig %>%
  # Summarize
  group_by(year) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nspecies=n_distinct(species_id),
            landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Convert
  mutate(landings_kg=measurements::conv_unit(landings_lbs, "lbs", "kg"),
         landings_mt=landings_kg/1000)


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text=element_text(size=8),
                    plot.tag=element_text(size=9),
                    plot.title=element_text(size=9),
                    plot.subtitle = element_text(size=8, face="italic"),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g1 <- ggplot() +
  # Plot blocks
  geom_sf(data_blocks_sf, mapping=aes(fill=landings_mt)) +
  # Plot state waters
  geom_sf(data=state_waters_line, color="black", lwd=0.3) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Labels
  labs(subtitle="A few rarely visited blocks are not shown for rule-of-three compliance", tag="A") +
  # Legend
  scale_fill_gradientn(name="Landings (mt/yr)\n2013-2022 average",
                       trans="log10",
                       breaks=c(0.1, 1, 10, 100, 1000),
                       labels=c("0.1", "1", "10", "100", "1000"),
                       na.value = "grey90",
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="inside", 
        legend.position.inside = c(0.2, 0.2),
        legend.key.size = unit(0.5, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot landings
g2 <- ggplot(data_ts, aes(x=year, y=landings_mt/1000)) +
  geom_line() +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Landings\n(1000s mt)", tag="B") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot value
g3 <- ggplot(data_ts, aes(x=year, y=value_usd/1e6)) +
  geom_line() +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Ex-vessel revenue\n(USD millions)", tag="C") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Plot value
g4 <- ggplot(data_ts, aes(x=year, y=nspecies)) +
  geom_line() +
  # Axes
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Year", y="Number of taxa\nin the catch", tag="D") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g4

# Merge data
layout_matrix <- matrix(c(1,2,
                          1,3,
                          1,4), nrow=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, widths=c(0.62, 0.38), layout_matrix=layout_matrix)


# Export
ggsave(g, filename=file.path(plotdir, "Fig1_ca_landings_map_history.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

