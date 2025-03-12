
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

# Read keys
station_key_orig <- readRDS(file=file.path(outdir, "calcofi_stations.Rds"))
station_key <- station_key_orig %>% 
  select(line, station, type) %>% 
  rename(survey=type)


# Get data
################################################################################

# CalCOFI datasets
# https://coastwatch.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=calcofi

# CalCOFI NOAA Fish Larvae Counts
# https://coastwatch.pfeg.noaa.gov/erddap/tabledap/erdCalCOFIlrvcnt.html

# Dataset info
dataset_info <- rerddap::info('erdCalCOFIlrvcnt', url = "https://coastwatch.pfeg.noaa.gov/erddap/")
towtype_info <-  rerddap::info('erdCalCOFItowtyp', url = "https://coastwatch.pfeg.noaa.gov/erddap/") 
station_info <-  rerddap::info('erdCalCOFIstns', url = "https://coastwatch.pfeg.noaa.gov/erddap/") 
tow_info <-  rerddap::info('erdCalCOFItows', url = "https://coastwatch.pfeg.noaa.gov/erddap/") 

# Station key
station_orig <- rerddap::tabledap(x=station_info, 
                                  url="https://coastwatch.pfeg.noaa.gov/erddap/", 
                                  fmt="csv") 

# Tow type key
towtype_orig <- rerddap::tabledap(x=towtype_info, 
                               url="https://coastwatch.pfeg.noaa.gov/erddap/", 
                               fmt="csv") 

# Tow  key
tow_orig <- rerddap::tabledap(x=tow_info, 
                              url="https://coastwatch.pfeg.noaa.gov/erddap/", 
                              fmt="csv") 

# Download table data
data_orig <- rerddap::tabledap(x=dataset_info, 
                               url="https://coastwatch.pfeg.noaa.gov/erddap/", 
                               fmt="csv") 

# Export
#saveRDS(data_orig, file=file.path(rawdir, "calcofi_fish_larvae_counts_raw.Rds"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(sci_name=scientific_name,
         comm_name=common_name,
         spp_code_itis=itis_tsn,
         spp_code_calcofi=calcofi_species_code,
         lat_dd=latitude,
         long_dd=longitude,
         tow_type_code=tow_type) %>% 
  # Add tow type
  mutate(tow_type=recode(tow_type_code,
                         "C1"="CalCOFI 1-meter oblique tow",     
                         "CB"="CalCOFI oblique bongo tow",     
                         "CV"="CalCOFI vertical egg tow",      
                         "DC"="Oblique tow to 600 m using the CB net (historically used to sample sablefish)",      
                         "MT"="Twin winged continuous-flow surface tow",      
                         "PV"="Paired tows of CalCOFI vertical egg net")) %>% 
  # Format date
  mutate(time1=lubridate::ymd_hms(time),
         date=substr(time, 1, 10) %>% lubridate::ymd(),
         year=lubridate::year(date)) %>% 
  # Add survey type
  left_join(station_key) %>% 
  # Arrange
  select(year, date, time, time1, 
         cruise, ship, ship_code,
         survey, order_occupied, line, station, lat_dd, long_dd,
         tow_type_code, tow_type, net_location, tow_number,
         comm_name, sci_name, spp_code_calcofi, spp_code_itis,
         standard_haul_factor, volume_sampled, proportion_sorted,
         larvae_count, larvae_10m2, larvae_100m3,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect more
range(data$year)
range(data$date)
table(data$cruise)
table(data$ship)
table(data$ship_code)
table(data$order_occupied)
sort(unique(data$line))
sort(unique(data$station))
table(data$tow_number)
table(data$tow_type)
table(data$tow_type)
table(data$net_location)
table(data$standard_haul_factor)

# Tow type
tow_type_key <- data %>% 
  count(tow_type_code, tow_type)

# Net location

# Ship key
ship_key <- data %>% 
  count(ship, ship_code)
freeR::which_duplicated(ship_key$ship)
freeR::which_duplicated(ship_key$ship_code)

# Stations
stations <- data %>% 
  count(year, survey, order_occupied, line, station, lat_dd, long_dd, tow_type)

# Tow id
tow_key <- data %>% 
  select(cruise, ship, ship_code, tow_type, tow_number)

# Species key
spp_key <- data %>% 
  # Count
  count(comm_name, sci_name) %>% 
  # Species or generic
  mutate(nword=freeR::nwords(sci_name),
         taxa_type=ifelse(nword>1, "species", "general"))


# Export data
################################################################################

# Export
saveRDS(data_orig, file=file.path(rawdir, "calcofi_fish_larvae_counts_raw.Rds"))


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
  coord_sf(xlim = c(-126, -116.5), ylim = c(30, 38)) +
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
