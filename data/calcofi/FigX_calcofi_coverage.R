
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
data <- readRDS(file=file.path(outdir, "calcofi_fish_larvae_counts.Rds"))

# Stations
stations <- data %>% 
  count(year, survey, order_occupied, line, station, lat_dd, long_dd, tow_type)


# Plot map
################################################################################



# CALCOFI stations 
stations_do <- stations %>% filter(!is.na(survey))

# Theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  legend.text=element_text(size=7),
                  strip.text = element_text(size=6),
                  legend.title=element_blank(),
                  plot.tag = element_blank(),
                  plot.title=element_blank(),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = "bottom",
                  legend.key.size=unit(0.3, "cm"),
                  legend.key=element_blank(),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot data
g <- ggplot(stations_do, aes(x=long_dd, y=lat_dd, color=tow_type)) +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Data
  geom_point(size=0.5) +
  # Legend
  guides(color = guide_legend(nrow = 2)) +
  # Crop
  coord_sf(xlim = c(-126.5, -116.5), ylim = c(30, 38)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_map.png"), 
       width=6.5, height=7, units="in", dpi=600)



# Plot within year sampling
################################################################################

# Build
tows_do <- data %>% 
  # Tow-level attributes
  count(year, date, 
        survey, line, station, tow_number, 
        cruise, ship, ship_code, 
        order_occupied, lat_dd, long_dd, tow_type_code, tow_type, net_location) %>% 
  # Only CALCOFI surveys
  filter(!is.na(survey)) %>% 
  # Add day
  mutate(yday=yday(date)) %>% 
  # Add dummy date
  mutate(date_dummy=paste("2000", month(date), day(date), sep="-") %>% ymd())


# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_blank(),
                  legend.text=element_text(size=7),
                  legend.title=element_blank(),
                  plot.tag = element_blank(),
                  plot.title=element_blank(),
                  # Gridlines
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = "bottom",
                  legend.key.size=unit(0.3, "cm"),
                  legend.key=element_blank(),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(tows_do, aes(x=date_dummy, y=year, color=tow_type)) +
  geom_point(size=0.5) +
  # Labels
  labs(x="", y="") +
  # X-axis
  scale_x_date(breaks=seq(ymd("2000-01-01"), 
                          ymd("2000-12-01"), by="1 month"),
               date_label="%b") +
  # Y-axis
  scale_y_reverse(breaks=seq(1950,2025,5)) +
  # Legend
  guides(color = guide_legend(nrow = 2)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_temporal.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Plot within year sampling
################################################################################

# Data
stats <- tows_do %>% 
  count(year, tow_type)

my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  plot.tag = element_blank(),
                  plot.title=element_blank(),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.key.size=unit(0.3, "cm"),
                  legend.key=element_blank(),
                  legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(stats, aes(x=year, y=n, fill=tow_type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="# of tows") +
  # Axis
  scale_x_continuous(breaks=seq(1950, 2025, 5)) +
  # Legend
  scale_fill_discrete(name="Sampling type") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_calcofi_time_series.png"), 
       width=6.5, height=3, units="in", dpi=600)



