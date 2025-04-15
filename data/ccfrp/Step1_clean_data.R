
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
indir <- "data/ccfrp/raw"
outdir <- "data/ccfrp/processed"
plotdir <- "data/ccfrp/figures"

# Read data
effort_orig <- read.csv(file=file.path(indir, "2007_2023_CCFRP_derived_effort_table.csv"), na.strings = c("", "NA"))
spp_orig <- read.csv(file=file.path(indir, "CCFRP_species_table.csv"), na.strings = c("", "NA"))
sites_orig <- read.csv(file=file.path(indir, "CCFRP_location_table.csv"), na.strings = c("", "NA"))
lengths_orig <- read.csv(file=file.path(indir, "2007_2023_CCFRP_derived_length_table.csv"), na.strings = c("", "NA"))

# To-do list
# Lots of sites are missing lat/longs


# Format species key
################################################################################

# Species key
spp <- spp_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name_orig=common_name,
         sci_name=scientific_name) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name_orig)) %>% 
  # Format scientific names
  mutate(sci_name=recode(sci_name, 
                         "Sebastes dalli"="Sebastes dallii",
                         "Beringraja stellulata"="Caliraja stellulata",
                         "Traikis semifasciata"="Triakis semifasciata",
                         "Xenistius californiensis"="Brachygenys californiensis"))

freeR::check_names(spp$sci_name)

# Inspect
freeR::complete(spp)

# Confirm unique id
freeR::which_duplicated(spp$species_code)



# Format site key
################################################################################

# Get MPAs
mpas <- wcfish::mpas_ca

# Which MPAs are in monitoring data and not shapefile?
mpas_ccfrp <- sort(unique(sites_orig$MPA_names))
mpas_ccfrp[!mpas_ccfrp %in% mpas$name_full]

# Site key
sites <- sites_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(cell_id=grid_cell_id,
         lat_dd=lat_center_point_dd,
         long_dd=lon_center_point_dd,
         mpa_long=mpa_names) %>% 
  # Format MPA name
  mutate(mpa_long=recode(mpa_long, "Ano Nuevo State Marine Reserve"="Año Nuevo State Marine Reserve")) %>% 
  # Add MPA attributes
  left_join(mpas %>% select(name_full, name, region, type) %>% sf::st_drop_geometry(), by=c("mpa_long"="name_full")) %>% 
  rename(mpa_region=region,
         mpa_type=type,
         mpa=name) %>% 
  # Add MPA region
  # mutate(mpa_region=cut(lat_dd, 
  #                       breaks=c(0,34.5, 37.5, 39, 100), 
  #                       labels=c("South", "Central", "North Central", "North"))) %>% 
  # Recode MPA region
  mutate(mpa_region=recode(mpa_region,
                          "SCSR"="South",
                          "CCSR"="Central",
                          "NCCSR"="North Central",
                          "NCSR"="North")) %>%
  # Fill in missing regions
  group_by(area) %>% 
  fill(mpa_region, .direction = "updown") %>% 
  ungroup() %>% 
  mutate(mpa_region=ifelse(area=="Trinidad", "North", mpa_region)) %>% 
  # Arrange
  select(-c(ltm_project_short_code, ca_mpa_name_short)) %>% 
  select(monitoring_group, area, mpa, mpa_long, mpa_region, mpa_type, everything())

# Confirm unique id
freeR::which_duplicated(sites$cell_id)

# Inspects
str(sites)
freeR::complete(sites)

# Inspect more
table(sites$area)

# Area key
count(sites, area, area_code, mpa_region)

# Export
saveRDS(sites, file=file.path(outdir, "ccfrp_sites.Rds"))


# Plot map
################################################################################

# Mark MPA shapefile
mpas <- mpas %>% 
  mutate(ccfrp=ifelse(name %in% sites$mpa, "Monitored", "Unmonitored"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot map
ggplot(sites, aes(x=long_dd, y=lat_dd, color=mpa_region)) +
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


# Area key
################################################################################

# Build area key
area_key <- sites %>% 
  group_by(area) %>% 
  summarize(lat_dd=mean(lat_dd, na.rm=T),
            mpa_region=unique(mpa_region) %>% na.omit() %>% paste(collapse=", "),) %>% 
  ungroup() %>% 
  arrange(desc(lat_dd)) %>% 
  mutate(mpa_region=ifelse(area=="Trinidad", "North", mpa_region))




# Format lengths
################################################################################

# Format lengths
lengths <- lengths_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name=common_name,
         cell_id=grid_cell_id,
         mpa=ca_mpa_name_short,
         survey_id=id_cell_per_trip) %>% 
  # Format date
  mutate(date=lubridate::ymd(date)) %>% 
  # Remove useless
  select(-c(ltm_project_short_code)) %>% 
  # Format area
  mutate(area=recode(area, 
                     "Cape Mendocino"="South Cape Mendocino",
                     "Farallon Islands"="Southeast Farallon Islands"))

# Complete
freeR::complete(lengths)


# Format CPUE
################################################################################

# Format data
data <- effort_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name_orig=common_name,
         cell_id=grid_cell_id,
         mpa=ca_mpa_name_short,
         survey_id=id_cell_per_trip,
         sst_c=surface_water_temp_c, # what is this?
         bottom_c=depth_water_temp_c, # what is this?
         vessel_c=vessel_water_temp_c, # what is this?
         relief=relief_1_3, # what is this?
         wind_kt=wind_speed_kt,
         swell_m=swell_height_m,
         angler_hrs=total_angler_hours,
         cpue_n_hr=cpue_catch_per_angler_hour,
         cpue_kg_hr=bpue_biomass_kg_per_angler_hour) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         yday=yday(date)) %>% 
  relocate(yday, .after=date) %>% 
  # Format area
  mutate(area=recode(area, 
                     "Cape Mendocino"="South Cape Mendocino",
                     "Farallon Islands"="Southeast Farallon Islands")) %>% 
  # Add MPA region
  left_join(area_key, by="area") %>% 
  # Add species info
  left_join(spp %>% select(comm_name_orig, comm_name, sci_name), by="comm_name_orig") %>% 
  # Factor area
  mutate(area=factor(area, levels=area_key$area)) %>% 
  # Arrange
  select(ltm_project_short_code:comm_name_orig, comm_name, sci_name, everything())


# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$area)

# Export
saveRDS(data, file=file.path(outdir, "ccfrp_catch_data.Rds"))


# Survey key
################################################################################

# Survey
surveys <- data %>% 
  # Unique
  select(monitoring_group, mpa, area, mpa_status, year, month, day,  date, yday, survey_id, cell_id, sst_c, bottom_c, vessel_c, relief,
         start_depth_m, end_depth_m, wind_kt, swell_m) %>% 
  unique() %>% 
  # Add MPA region
  left_join(area_key, by="area") %>% 
  # Format areas
  mutate(area=factor(area, levels=area_key$area))

# Check id
freeR::which_duplicated(surveys$survey_id)

# Inspect
str(surveys)
freeR::complete(surveys)


# Export
saveRDS(surveys, file=file.path(outdir, "ccfrp_surveys.Rds"))


# Temporal coverage
################################################################################

ggplot(surveys, aes(x=yday, y=year, color=area)) +
  geom_point() +
  # Labels
  labs(x="Day of year", y="") +
  scale_y_reverse(breaks=seq(2004,2026,2)) +
  # Theme
  theme_bw()

ggplot(surveys, aes(x=date, y=reorder(area, desc(area)), color=mpa_region)) +
  geom_point() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw()



# Species coverage
################################################################################

# Total surveys
nsurveys_tot <- data %>% 
  filter(mpa_region=="Central") %>% 
  pull(survey_id) %>% n_distinct()

# Species stats
stats <- data %>% 
  filter(mpa_region=="Central" & count>0) %>% 
  group_by(comm_name) %>% 
  summarize(nyr=n_distinct(year),
            nsurveys=n_distinct(survey_id)) %>% 
  ungroup() %>% 
  mutate(psurveys=nsurveys/nsurveys_tot) %>% 
  filter(nyr>=14 & nsurveys >= 10)

# Plot data
ggplot(stats, aes(x=psurveys, y=reorder(comm_name, desc(psurveys)))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of surveys", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw()




