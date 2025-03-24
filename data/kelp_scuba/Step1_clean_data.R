
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(sf)
library(tidyverse)
library(sdmTMB)
library(foreach)
library(doParallel)

# Directories
indir <- "data/kelp_scuba/raw"
outdir <- "data/kelp_scuba/processed"
plotdir <- "data/kelp_scuba/figures"

# Downloaded from:
# https://opc.dataone.org/view/doi%3A10.25494%2FP6%2FMLPA_kelpforest.9

# Algae/inverts
upc_orig <- read.csv(file=file.path(indir, "MLPA_kelpforest_upc.6.csv"), na.strings = c("", "NA")) # benthic algae/invertebrates
swath_orig <- read.csv(file=file.path(indir, "MLPA_kelpforest_swath.7.csv"), na.strings = c("", "NA")) # benthic algae/invertebrates

# Fish data
fish_orig <- read.csv(file=file.path(indir, "MLPA_kelpforest_fish.6.csv"), na.strings = c("", "NA"))

# Keys
spp_orig <- read.csv(file=file.path(indir, "MLPA_kelpforest_taxon_table.7.csv"), na.strings = c("", "NA"))
sites_orig <- read.csv(file=file.path(indir, "MLPA_kelpforest_site_table.6.csv"), na.strings = c("", "NA", "N/A"))

# Length data
lengths_orig <- read.csv(file=file.path(indir, "MLPA_kelpforest_sizefreq.6.csv"), na.strings = c("", "NA"))


# Format species key
################################################################################

# Species key
spp_full <- spp_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name=common_name,
         sci_name=species_definition) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name))

# Inspect
freeR::complete(spp_full)

# Confirm unique id - not remotely
freeR::which_duplicated(spp_full$classcode)

# Simple species key
spp_simple <- spp_full %>% 
  filter(sample_type=="FISH") %>% 
  select(classcode, sci_name, comm_name) %>% 
  unique()

# Confirm unique id
freeR::which_duplicated(spp_simple$classcode)
freeR::which_duplicated(spp_simple$sci_name)

# Export
saveRDS(spp_simple, file=file.path(outdir, "species.Rds"))


# Format site key
################################################################################

# Site key
sites <- sites_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(lat_dd=latitude,
         long_dd=longitude,
         mpa=ca_mpa_name_short,
         mpa_type=site_designation) %>% 
  # Remove duplicates
  unique()

# Inspect
str(sites)
freeR::complete(sites)

# Campus
table(sites$campus)

# Method
table(sites$method)

# Year
range(sites$survey_year)

# MPA type
table(sites$mpa_type)

# Simple site
site_key_use1 <- sites %>% 
  # Unique fish survey sites
  filter(grepl("FISH", method)) %>% 
  select(campus, method, survey_year, site, lat_dd, long_dd) %>% 
  unique() %>% 
  # Compute average since there are sometimes different GPS coords for a site
  group_by(campus, method, survey_year, site) %>% 
  summarise(n=n(),
            lat_dd=mean(lat_dd),
            long_dd=mean(long_dd)) %>% 
  ungroup()
site_key_use2 <- sites %>% 
  # Unique fish survey sites
  filter(grepl("FISH", method)) %>% 
  select(campus, method, survey_year, site, lat_dd, long_dd) %>% 
  unique() %>% 
  # Compute average since there are sometimes different GPS coords for a site
  group_by(campus, method, site) %>% 
  summarise(n=n(),
            lat_dd2=mean(lat_dd),
            long_dd2=mean(long_dd)) %>% 
  ungroup()



# Site map
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot map
ggplot(sites, aes(x=long_dd, y=lat_dd)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Plot MPAs
  # geom_sf(data=mpas,fill="red", color=NA, inherit.aes = F) + # mapping=aes(fill=ccfrp),
  # Plot sites
  geom_point() +
  # Legend
  # scale_fill_manual(name="MPA status", values=c("red", "blue")) +
  # Crop
  coord_sf(xlim = c(-124.5, -116.5), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() +
  theme(legend.position = c(0.8, 0.8))


# Format fish data
################################################################################

# Format data
data <- fish_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(site_old=site_name_old, 
         length_cm=fish_tl,
         length_cm_min=min_tl,
         length_cm_max=max_tl,
         depth_m=depth,
         visibility_m=vis,
         temp_c=temp,
         kelp_coverage_code=pctcnpy) %>% 
  # Build date
  mutate(date=paste(year, month, day, sep="-") %>% lubridate::ymd(.),
         yday=lubridate::yday(date)) %>% 
  # Clean zone
  mutate(zone=recode(zone,
                     "OUTER MIDDLE"="OUTMID")) %>% 
  # Clean observer
  mutate(observer=gsub("_", "", observer) %>% stringr::str_to_title()) %>% 
  # Clean surge
  mutate(surge=stringr::str_to_sentence(surge)) %>% 
  # Add species info
  left_join(spp_simple, by="classcode") %>% 
  # Add site info
  left_join(site_key_use1 %>% select(-n), by=c("campus", "method", "survey_year", "site")) %>%
  # Add transect id
  mutate(survey_id=paste(date, site, sep="-"),
         transect_id=paste(date, site, zone, level, transect, sep="-")) %>%
  # Arrange
  select(campus, method, survey_year, year, month, day, date, yday, 
         site, site_old, zone, level, transect, survey_id, transect_id, lat_dd, long_dd, 
         depth_m, visibility_m, temp_c, surge, kelp_coverage_code, observer,
         classcode, comm_name, sci_name, count, 
         length_cm, length_cm_min, length_cm_max, sex, notes,
         everything())

# Inspect
str(data)

# Completeness
freeR::complete(data)

# Zone
# INNER	~5m of water depth, or the inner edge of the reef	
# INMID	~10m of water depth 
# MID	~10-15m of water depth, used by VRG and in early years of PISCO	
# OUTMID	~15m of water depth
# OUTER	~20m of water depth
# DEEP	~25m of water depth, where present, used only by VRG
table(data$zone)

# Level
# BOT	Transect placed on the seafloor and the lower 2m of the water column surveyed	
# MID	Transect placed in the midwater, at roughly half of the depth of the seafloor (depending on visibility)	
# CAN	Transect placed at the surface and the upper 2m of water column and kelp canopy surveyed. Canopy transects are only conducted when kelp is present and forms a canopy at the surface	
# CNMD	When an inner transect is too shallow to allow both canopy and midwater transects without overlapping, level is coded as CNMD and transects can be ascribed to either midwater or canopy (applies to UCSB and VRG only)
table(data$level)


table(data$observer)
table(data$surge)

# Export data
saveRDS(data, file=file.path(outdir, "data.Rds"))


# Transect key
################################################################################

# Transect
transects <- data %>% 
  # Reduce to unique transects
  select(campus:observer) %>% 
  unique()

# Confirm transect ids are unique
freeR::which_duplicated(transects$transect_id)  

# Export
saveRDS(transects, file=file.path(outdir, "transects.Rds"))


# If zone depth is less than 6m and there is a canopy transect, delete midwater




